package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection abstract Variables and factor Templates for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).


/** An immutable-valued Variable that has a 'source' GenerativeDistribution and a 'pr' probability of being generated.  
 * For the corresponding mutable-valued Variable, see GenerativeVariable. */
trait GenerativeObservation[This<:GenerativeObservation[This] with Variable] extends Variable {
  this: This =>
  // This types are a pain.  Could Scala be changed to replace them with this#type ??? 
  type SourceType <: GenerativeDistribution[This]
  protected var _source: SourceType = _ // TODO Consider making this 'private' instead of 'protected'
  @inline final def source = _source
  def setSource(s:SourceType)(implicit d:DiffList) : Unit = {
    if (_source != null) _source._unregisterSample(this)
    _setSource(s)
    if (_source != null) _source._registerSample(this)
  }
  /** Set 'source' directly without coordinating via source.(un)generate.  Don't call this yourself; call 'setSource' instead. */
  @inline override def _setSource(s:AnyRef)(implicit d:DiffList) : Unit = {
    val ss = s.asInstanceOf[SourceType] // TODO Arg.  I want s:SourceType, but Scala type system doesn't like M#OutcomeType vs M.OutcomeType
    if (d != null) d += SetSourceDiff(_source, ss)
    _source = ss
  }
  /** Register this variable as having been generated from source 's'. */
  def ~(s:SourceType): this.type = { setSource(s)(null); this }
  /** Register this variable as having been generated from the source indicated by the MixtureChoice 'mmc'. */
  def ~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = {
    mmc.setOutcome(this); 
    this.~(mmc.choice) // either here or in mmc.setOutcome; not sure which is more natural
  }
  /** Probability of this variable given its 'source' parents. */
  def pr: Double = if (_source != null) _source.pr(this) else throw new Error("Source not set")
  /** log-probability of this variable given its 'source' parents. */
  def logpr: Double = Math.log(pr)
  case class SetSourceDiff(oldSource:SourceType, newSource:SourceType) extends Diff {
    def variable = GenerativeObservation.this
    def redo = _source = newSource
    def undo = _source = oldSource
  }
}

trait GenerativeVariable[This<:GenerativeVariable[This] with Variable] extends GenerativeObservation[This] with AbstractGenerativeVariable {
  this: This =>
  /** Register this variable as having been generated from source 's', and furthermore set this variable's value to a sample from 's'. */
  def :~(s:SourceType): this.type = { this.~(s); s.sampleInto(this); this }
  def :~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = { this.~(mmc); mmc.choice.sampleInto(this); this }
  //def maximize(implicit d:DiffList): Unit // TODO Consider adding this also; how interact with 'estimate'?  // Set value to that with highest probability
}

/** A stand-in for GenerativeVariable that does not take type parameters, which we use as the type argument to GenerativeVariableSampler. */
trait AbstractGenerativeVariable extends Variable {
  /** Set this variable to a new value, sampled according to the distribution 
  		indicated by both its 'source' parents and also its 'generatedSamples' children if available. */
	def sample(implicit d:DiffList): Unit
}

// TODO  Consider something like this.  A trait for factory objects
//trait GenerativeFactory[Source<:AbstractGenerativeDistribution] { def apply(Source#OutcomeType#ValueType): Source#OutcomeType }

/** A Variable representing a probability distribution that generates other variable values with type OutcomeType
    It provides methods returning the probability of given values, and for (re-)sampling a new value into an existing variable. 
    Optionally, it keeps track of all the variables generated, for use in estimating the parameters of this distribution,
    and for use in finding neighbors for factor Templates. */
// Change this to O<:TypedVariable so that we can add def sampleValue:O#ValueType
trait GenerativeDistribution[O<:Variable] extends AbstractGenerativeDistribution {
  // Note that 'O' is not *required* to be a GenerativeVariable.  This allows us to put SingleIndexedVariable into Multinomial, for example.
  type OutcomeType = O // TODO Consider insisting the OutcomeType = GenerativeObservation[O]; For now I've simly added a noop 'setSource' method to Variable
}

/** A stand-in for GenerativeDistribution that does not take type parameters */
trait AbstractGenerativeDistribution extends Variable {
  type OutcomeType <: Variable 
  // Perhaps we really want OutcomeType to be 'either ConstantVariable or GenerativeObservation', 
  //  because changing Variables should be able to find their source.
  // How to do this?  
  // Perhaps define a trait VariableWithSource[This] { type SourceType <: GenerativeDistribution[This]; def _setSource...}
  // and then make ConstantVariable and GenerativeObservation both inherit from it?
  // No, I don't think this is necessary.  Putting Real into Gaussian would be useful, 
  //  and it is useful even when we are not trying to track score changes due to changes in Real.value
  type VariableType <: AbstractGenerativeDistribution
	def estimate: Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?
  lazy val generatedSamples = new HashSet[OutcomeType];
  var keepGeneratedSamples = true
  // TODO Rename registerSample, unregisterSample
  def _registerSample(o:OutcomeType)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
    generatedSamples += o
    if (d != null) d += GenerativeDistributionRegisterDiff(o)
  }
  def _unregisterSample(o:OutcomeType)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    generatedSamples -= o
    if (d != null) d += GenerativeDistributionUnregisterDiff(o)
  }
  /** Notify this GenerativeDistribution that it is now associated with an additional sampled outcome, and set o's source to this. */
  final def registerSample(o:OutcomeType)(implicit d:DiffList): Unit = {
    _registerSample(o)
		//o._setSource(this)//.asInstanceOf[OutcomeType#SourceType]
    o._setSource(this)
  }
  /** Notify this GenerativeDistribution that it is no longer associated with a sampled outcome, and set o's source to null. */
  final def unregisterSample(o:OutcomeType)(implicit d:DiffList): Unit = {
    _unregisterSample(o)
  	//if (o.isInstanceOf[GenerativeObservation[_]]) o.asInstanceOf[GenerativeObservation[_]]._setSource(null.asInstanceOf[OutcomeType#SourceType])
  	o._setSource(null)
  }
  // TODO consider removing preChange/postChange, because it requires extra infrastructure/Diffs from implementers?  Can just use (un)registerSample
  /** Notify this GenerativeDistribution that the value of its associated outcome 'o' is about to change.  
      Calls to this method are always paired with (followed by) a call to postChange. */
  def preChange(o:OutcomeType)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  def postChange(o:OutcomeType)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  /** Return the probability that this GenerativeDistribution would generate outcome 'o' */
  def pr(o:OutcomeType): Double
  /** Return the log-probability that this GenerativeDistribution would generate outcome 'o' */
	def logpr(o:OutcomeType): Double = Math.log(pr(o))
	/** Change the value of outcome 'o' to a value sampled from this GenerativeDistribution */
	def sampleInto(o:OutcomeType): Unit
	// Fighting with the Scala type system
	// See http://www.nabble.com/Path-dependent-type-question-td16767728.html
  //// http://www.nabble.com/Fwd:--lift--Lift-with-Scala-2.6.1--td14571698.html 
	def unsafeRegisterSample(o:Variable)(implicit d:DiffList) = registerSample(o.asInstanceOf[OutcomeType])
	def unsafeUnregisterSample(o:Variable)(implicit d:DiffList) = unregisterSample(o.asInstanceOf[OutcomeType])
	def unsafePr(o:Variable) = pr(o.asInstanceOf[OutcomeType])
	def unsafeLogpr(o:Variable) = logpr(o.asInstanceOf[OutcomeType])
	case class GenerativeDistributionRegisterDiff(m:OutcomeType) extends Diff {
		def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
		def undo = { generatedSamples -= m }
	}
 	case class GenerativeDistributionUnregisterDiff(m:OutcomeType) extends Diff {
		def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { generatedSamples -= m }
		def undo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
	}
}




  
/** Trait for any distribution that might be selected as as part of a Multinomial mixture. 
Creates is own Domain.  Number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponent objects.
 Note that this is not a MultinomialOutcome, it is the *value* of MultinomialOutcome. */
//trait MixtureComponent[This<:MixtureComponent[This] with GenerativeDistribution] extends TypedSingleIndexedVariable[This] with GenerativeDistribution
trait MixtureComponent[This<:MixtureComponent[This] with AbstractGenerativeDistribution] extends TypedSingleIndexedVariable[This] with AbstractGenerativeDistribution {
	// TODO Make it "This<:MixtureComponent[This] with AbstractGenerativeDistribution with ItemizedVariable[This]" and I think we can get rid of many lines of code below
  this : This =>
	//type OutcomeType = This#OutcomeType // Not necessary?
	type VariableType = This  // can we get away with this = ?
	type DomainType = IndexedDomain[VariableType] // We must make sure that this.domain is an IndexedDomain
	class DomainClass extends IndexedDomain[VariableType]
	//override type ValueType = This
	//override final def domain : Domain[This] = super.domain // TODO I think this line can be commented out???
	//def domain : Domain[This] = Domain[This].get(this.getClass)
  //println (this.getClass.getName+" MixtureComponent domain class = "+domain.getClass)
	_index = domain.asInstanceOf[IndexedDomain[This]].index(this) // TODO can we avoid this cast?
	//println("Creating MixtureComponent this.getClass = "+this.getClass.toString+" index="+_index+" domain size="+domain.asInstanceOf[IndexedDomain[This]].size)
	override final def setByIndex(index:Int)(implicit d:DiffList) : Unit = new Error // TODO this class should inherit from TypedSingleIndexedObservation instead?  But is does change, though.
}


// Example usage, in LDA: 
// class Topic extends Multinomial[Word] with MixtureComponent[Topic]
// class Z extends MixtureChoice[Topic,Z]; Domain.alias[Z,Topic]
// class Theta extends Multinomial[Z];
/** A multinomial outcome that is an indicator of which mixture component in a MixtureChoice is chosen.  
 * The "Z" in Latent Dirichlet Allocation is an example. */                                 
class MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends MultinomialOutcomeVariable[This] {
  this : This =>
  type VariableType = This
  type ValueType = M
  class DomainInSubclasses
  def choice: M = domain.get(index)
  _index = Global.random.nextInt(domain.size) // TODO is this how _index should be initialized?
  if (!Global.defaultModel.contains(MixtureChoiceTemplate)) Global.defaultModel += MixtureChoiceTemplate
  // The following is done in MultinomialOutcome[This]:  type DomainType = IndexedDomain[M]; class DomainClass extends IndexedDomain[M]
  Domain.alias[This,M](mt,mm)
	private var _outcome : M#OutcomeType = _
	@inline final def outcome : M#OutcomeType = _outcome // The particular outcome that was generated from this choice of mixture component
	def setOutcome(o:M#OutcomeType) = if (_outcome == null) _outcome = o else throw new Error("Outcome already set")
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
	  if (_outcome == null) throw new Error("No outcome yet set.")
	  choice.unsafeUnregisterSample(outcome) // was unsafe // TODO Change to preChange/postChange
	  super.setByIndex(newIndex) // this changes the value of 'choice'
	  choice.unsafeRegisterSample(outcome) // was unsafe
	}
  override def pr: Double = choice.unsafePr(outcome) * source.pr(index) // TODO Perhaps this should just return pr(index), and handle pr(outcome) somewhere else?  Then must also change MixtureChoiceTemplate below! 
  override def logpr: Double = choice.unsafeLogpr(outcome) + source.logpr(index)
  // On 127 documents with 185129 tokens and 17032 types (Users/mccallum/research/data/text/nipstxt/nips05)
  // 9 iterations, printing topics 4 times: 
  // this.sample with this.setByIndex = 118.4 seconds (old version)
  // this.sample with local super.setByIndex = 114.5 seconds
  // this.sample with above and "val dom" = 109.0 seconds
  // this.sample with above and "DirichletMultinomial.pre/postChange" = 108.0 seconds // TODO Didn't help much; consider removing pre/postChange?
  // this.sample with above and keepGeneratedSamples = false = 103.3 seconds (off by default)
  // this.sample with local super.setByIndex and "val dom" and noDiffList = 503.0 seconds.  Fishy!!!!  // TODO Why?????  Investigate!
  // GibbsSampler = 368.3 seconds
  override def sample(implicit d:DiffList): Unit = {
    //println("MixtureChoice.sample "+index+" diff "+d)
    //val isDM = classOf[DirichletMultinomial[This]].isAssignableFrom(source.getClass)
    val src = source
    // Remove this variable and its sufficient statistics from the model
    choice.unsafeUnregisterSample(outcome)
    source.unregisterSample(this)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, -1)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    val distribution = Array.fromFunction[Double]((i:Int) => src.pr(i) * dom.get(i).unsafePr(outcome))(dom.size)
    val i = Maths.nextDiscrete(distribution, distribution.foldLeft(0.0)(_+_))(Global.random)
    //var i = 0; val max = distribution.size; val r = Global.random.nextDouble * distribution.foldLeft(0.0)(_+_); var s = 0.0
    //while (s < r && i < max) { s += distribution(i); i += 1 }
    //choice.unsafeGenerate(outcome) // Put outcome back, although, inefficiently, the next line moves it again
    //setByIndex(i - 1) // So, instead we do it ourselves.  But then subclassers cannot meaningfully override setByIndex!! // TODO Consider alternatives
    super.setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    src.registerSample(this)
    choice.unsafeRegisterSample(outcome)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, 1)
  }
}

  


class GenericMultinomialOutcome extends MultinomialOutcome[GenericMultinomialOutcome] { def index = -1 }
// TODO Is this arbitrary "2" below OK?
class GenericMixtureComponent extends DenseCountsMultinomial[GenericMultinomialOutcome](2) with MixtureComponent[GenericMixtureComponent]
class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]
// trait MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]] extends MultinomialOutcome[This] {
object MixtureChoiceTemplate extends TemplateWithStatistics1[MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]] {
  def score(s:Stat) = { /*println("MixtureChoiceTemplate score");*/ s.s1.logpr } // MixtureComponent.logpr current includes both source and outcome, but perhaps it shouldn't and both should be here
}

//trait GenericGenerativeVariable extends GenerativeVariable[GenericGenerativeVariable]
class GenerativeVariableSampler extends Sampler[AbstractGenerativeVariable] {
  def process1(v:AbstractGenerativeVariable): DiffList = { val d = newDiffList; v.sample(d); d }
}
