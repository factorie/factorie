package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection abstract Variables (and a generic Sampler) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).

/** An immutable-valued Variable that has a 'source' GenerativeDistribution and a 'pr' probability of being generated.  
    For the corresponding mutable-valued Variable, see GenerativeVariable.  
    The type parameter O is the return type for 'asOutcome', which typically simply returns 'this'; 
    so the most common case is to put a self-type there. */
//trait GenerativeObservation[This<:GenerativeObservation[This] with Variable] extends Variable
// TODO Consider GenerativeObservation[O<:Outcome]?  No this concept is already 'Outcome'
// TODO Consider renaming GenerativeValue[O]
// TODO Consider renaming Outcome[O] or OutcomeValue[O] for consistency
// I'm hesitating for the same reason that "Value" caused me hesitation: "Outcome" refers to the value of a random variable, not the variable itself.
trait GenerativeObservation[This] extends Variable {
  //this: This => // Don't require is, for example see ?
  type SourceType <: GenerativeDistribution[This]
  private var _source: SourceType = _
  // TODO Consider renaming this "generatingSource" in order to avoid possible method naming conflicts with subclasses
  @inline final def source = _source
  def asOutcome: This // Everywhere this is, there used to be "this"
  def setSource(s:SourceType)(implicit d:DiffList) : Unit = {
    if (_source ne null) _source._unregisterSample(asOutcome)
    _setSource(s)
    if (_source ne null) _source._registerSample(asOutcome)
  }
  /** Set 'source' directly without coordinating via source.(un)generate.  Don't call this yourself; call 'setSource' instead. */
  @inline override def _setSource(s:AnyRef)(implicit d:DiffList) : Unit = {
    val ss = s.asInstanceOf[SourceType] // TODO Arg.  I want s:SourceType, but Scala type system doesn't like M#OutcomeType vs M.OutcomeType
    if (d ne null) d += SetSourceDiff(_source, ss)
    _source = ss
  }
  /** Register this variable as having been generated from source 's'. */
  def ~(s:SourceType): this.type = { setSource(s)(null); this }
  /** Register this variable as having been generated from the source indicated by the MixtureChoice 'mmc'. */
  def ~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = {
    mmc.setOutcome(asOutcome); 
    this.~(mmc.choice) // either here or in mmc.setOutcome; not sure which is more natural
  }
  /** Probability of this variable given its 'source' parents. */
  def pr: Double = _source.pr(asOutcome)
  /** log-probability of this variable given its 'source' parents. */
  def logpr: Double = Math.log(pr)
  case class SetSourceDiff(oldSource:SourceType, newSource:SourceType) extends Diff {
    def variable = GenerativeObservation.this
    def redo = _source = newSource
    def undo = _source = oldSource
  }
}

//trait GenerativeVariable[This<:GenerativeVariable[This] with Variable] extends GenerativeObservation[This] with AbstractGenerativeVariable {
trait GenerativeVariable[O] extends GenerativeObservation[O] with AbstractGenerativeVariable {
  //this: This =>
  /** Sample a new value for this variable from the distribution defined by 's'.  
      Note, this differs from the method 'sample' (defined in AbstractGenerativeVariable), which accounts for both the parent and children 
      when this is a GenerativeDistribution as well as a GenerativeVariable */
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit = throw new Error("Must be defined in subclasses.")
  /** Register this variable as having been generated from source 's', and furthermore set this variable's value to a sample from 's'. */
  def :~(s:SourceType): this.type = { this.sampleFrom(s)(null); this.~(s); this } 
  // Better to sampleFrom first then :~, because :~ may look at this.value and because :~ may add this' currentValue to s' parameter estimate
  // Note however that you can't do "MixtureChoice :~ Multinomial" because of outcome-setting-ordering.
  // We could try to change the design to allow this if we decide it is important.
  /** Register this variable as having been generated from the MixtureComponent source indicated by the given MixtureChoice, 
      and furthermore set this variable's value to a sample from it. */
  def :~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = { this.sampleFrom(mmc.choice)(null); this.~(mmc); this }
  //def maximize(implicit d:DiffList): Unit // TODO Consider adding this also; how interact with 'estimate'?  // Set value to that with highest probability
  // 'maximize' would differ from 'estimate' in that maximum likelihood estimation is just one way to 'estimate' a parameter      
}

/** A stand-in for GenerativeVariable that does not take type parameters, which we use as the type argument to GenerativeVariableSampler. */
trait AbstractGenerativeVariable extends Variable {
  /** Set this variable to a new value, sampled according to the distribution 
      indicated by both its 'source' parents and also its 'generatedSamples' children if available. */
  def sample(implicit d:DiffList): Unit
}


// Some specific cases of GenerativeObservation types

// TODO Consider changing DiscreteValue to DiscreteValues
// It would help Inferencer.IndexedMarginal
// But it is a little odd because
// * It could be a BinaryVectorVariable, 
// * It doesn't have "def index".  It might be a little painful to match/case all these places.  
// but we could certainly define pr(BinaryVectorVariable)
// Hmm... Requires further thought.
                                    

/** A GenerativeObservation with densely-packed integer values, for example the outcome of a Multinomial or Poisson.  
    This trait is used for Variables whose value is observed and does not change; 
    for Variables with changing value, you should use DiscreteOutcomeVariable. */
@DomainInSubclasses
trait DiscreteOutcome[This<:DiscreteOutcome[This] with DiscreteValue with GenerativeObservation[This]] extends DiscreteValue with GenerativeObservation[This] {
  // DiscreteOutcome should not be merged with DiscreteOutcomeVariable because not everything we want to generate has a "setByIndex" to override
  this: This =>  
  // "This" types are a bit verbose.  Could Scala be changed to replace them with this#type ??? 
  // Geoffrey Washburn says yes it is technically possible, but that Martin is simply against adding this feature to the language.
  type SourceType = DiscreteGenerating[This]
  @inline final def asOutcome = this
}

/** A DiscreteOutcome that is also a DiscreteVariable whose value can be changed. */
trait DiscreteOutcomeVariable[This<:DiscreteOutcomeVariable[This] with DiscreteVariable] extends DiscreteVariable with DiscreteOutcome[This] with GenerativeVariable[This] {
  this : This =>
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (source ne null) source.preChange(this)
    super.setByIndex(newIndex)
    if (source ne null) source.postChange(this)
  }
  /** Alternative setByIndex that avoids coordination with source, for use when you *really* know what you are doing, 
      and you are doing the source coordination yourself. */
  def _setByIndex(newIndex:Int)(implicit d:DiffList) = super.setByIndex(newIndex) 
  // TODO The above method is a bit scary because we may loose opportunities to fruitfully override setByIndex in subclasses
  // However it saved us (115-111) seconds in the LDA timing run described in Generative.scala. 
  override def sampleFrom(source:DiscreteGenerating[This])(implicit d:DiffList) = setByIndex(source.sampleIndex)
  def distribution: Array[Double] = { // TODO Remove or rename?  Rename proportion?
    val buffer = new Array[Double](domain.size);
    for (i <- 0 until buffer.length) buffer(i) = source.pr(i); 
    buffer
  }
  def sample(implicit d:DiffList): Unit = {
    // TODO Thursday Check!!!  Just removed this because I think it is redundant with preChange/postChange in setByIndex 
    //val isDM = classOf[DirichletMultinomial[This]].isAssignableFrom(getClass)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, -1)
    setByIndex(source.sampleIndex)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, 1)
  }
  def maximize(implicit d:DiffList): Unit = {
    setByIndex(source.maxPrIndex)
  }
}

trait CategoricalOutcome[This<:CategoricalOutcome[This] with CategoricalValue] extends CategoricalValue with DiscreteOutcome[This] {
  this : This =>
}
trait CategoricalOutcomeVariable[This<:CategoricalOutcomeVariable[This] with CategoricalVariable with CategoricalOutcome[This]] extends CategoricalVariable with DiscreteOutcomeVariable[This] with CategoricalOutcome[This] {
  this : This =>
}


/** A GenerativeObservation that consists of a vector of Doubles that sum to one, for example the parameters of a Multinomial. */
trait ProportionOutcome[O<:DiscreteValue] extends RandomAccessSeq[Double] with GenerativeVariable[ProportionOutcome[O]] {
  def localPr(index:Int): Double // TODO Needed in DirichletMomentMatchingEstimator, but general enough?
  def proportion: RandomAccessSeq[Double]
}




/** A generic Sampler for any AbstractGenerativeVariable, relying on its 'sample' method. */
class GenerativeVariableSampler extends Sampler[AbstractGenerativeVariable] {
  def process1(v:AbstractGenerativeVariable): DiffList = { 
    val d = newDiffList
    v.sample(d)
    //println("GenerativeVariableSampler "+d)
    d
  }
}
