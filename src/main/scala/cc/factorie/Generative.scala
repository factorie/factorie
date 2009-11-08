package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection of Variables and Factors relevant to generative models

/** A Variable that has a 'source' GenerativeDistribution and a 'pr' probability of being generated. */
trait GenerativeVariable[This<:GenerativeVariable[This]] extends Variable {
  this: This =>
  type SourceType <: GenerativeDistribution[This]
  protected var source: SourceType = _
  def setSource(dir:SourceType)(implicit d:DiffList) : Unit = {
    // TODO: consider not calling ungenerate and generate here.  Setting the source shouldn't change the Multinomial parameters immediately
    if (d != null) d += SetSourceDiff(source, dir)
    if (source != null) source.ungenerate(this)
    source = dir
    source.generate(this)
  }
  /** Register this variable as having been generated from source 's'. */
  def ~(s:SourceType): this.type = { setSource(s)(null); this }
  /** Register this variable as having been generated from source 's', and furthermore set this variable's value to a sample from 's'. */
  def :~(s:SourceType): this.type = { this.~(s); s.sampleInto(this); this }
  /** Register this variable as having been generated from the source indicated by the MixtureChoice 'mmc'. */
  def ~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = {
    mmc.setOutcome(this); 
    this.~(mmc.choice) // either here or in mmc.setOutcome; not sure which is more natural
  }
  def :~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = { this.~(mmc); mmc.choice.sampleInto(this); this }
  def pr: Double
  //def sample(implicit d:DiffList): Unit
  case class SetSourceDiff(oldSource:SourceType, newSource:SourceType) extends Diff {
    def variable = GenerativeVariable.this
    def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
    def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
  }
}


// Note that 'O' is not *required* to be a GenerativeVariable.  This allows us to put SingleIndexedVariable into Multinomial, for example.
trait GenerativeDistribution[O<:Variable] extends AbstractGenerativeDistribution {
  type OutcomeType = O
}

// A stand-in for GenerativeDistribution that does not take type parameters
trait AbstractGenerativeDistribution extends Variable {
  type OutcomeType <: Variable
  type VariableType <: GenerativeDistribution[OutcomeType]
	def estimate : Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?
	//// This odd arg type below is due to:
	//// http://www.nabble.com/Fwd:--lift--Lift-with-Scala-2.6.1--td14571698.html	
	//def pr[O<:OutcomeType](o:O) : Double
  //// This caused the compiler to crash. For now we have the "unsafe" work-around below.
  lazy val generatedSamples = new HashSet[OutcomeType];
  var keepGeneratedSamples = true
	def generate[O2<:OutcomeType](o:O2)(implicit d:DiffList): Unit = 
   if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
   else if (keepGeneratedSamples) {
     generatedSamples += o
     if (d != null) d += GenerativeDistributionGenerateDiff(o)
   }
  // TODO Add Handling of DiffList!!!!
  def ungenerate[O2<:OutcomeType](o:O2)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
  	generatedSamples -= o
  	if (d != null) d += GenerativeDistributionUngenerateDiff(o)
  }	
	def pr[O2<:OutcomeType](o:O2) : Double
	def logpr(o:OutcomeType) : Double
	def sampleInto(o:OutcomeType): Unit
	// Fighting with the Scala type system
	// See http://www.nabble.com/Path-dependent-type-question-td16767728.html
	def unsafeGenerate(o:Variable)(implicit d:DiffList) = generate(o.asInstanceOf[OutcomeType])
	def unsafeUngenerate(o:Variable)(implicit d:DiffList) = ungenerate(o.asInstanceOf[OutcomeType])
	def unsafePr(o:Variable) = pr(o.asInstanceOf[OutcomeType])
	def unsafeLogpr(o:Variable) = logpr(o.asInstanceOf[OutcomeType])
	case class GenerativeDistributionGenerateDiff(m:OutcomeType) extends Diff {
		def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
		def undo = { generatedSamples -= m }
	}
 	case class GenerativeDistributionUngenerateDiff(m:OutcomeType) extends Diff {
		def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { generatedSamples -= m }
		def undo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
	}
}

class Gaussian1(initialMean:Double, initialVariance:Double) extends GenerativeDistribution[Real] {
  //type OutcomeType = Real
  private var mean = initialMean
  private var variance = initialVariance
  def sample : Double = Maths.nextGaussian(mean,variance)(Global.random)
  def sampleInto(o:Real) : Unit = o.set(sample)(null) // TODO should we put a difflist here?
  def logpr(x:Double) : Double = {
    val diff = x - mean
    return - diff * diff / (2 * variance) - 0.5 * Math.log(2 * Math.Pi * variance)
  }
  def logpr(o:Real) = logpr(o.value)
  def pr(x:Double):Double = Math.exp(x)
  def pr[O2<:Real](o:O2):Double = pr(o.value)
  def minSamplesForVarianceEstimate = 5
  /** This implements a moment-matching estimator. */
  def estimate : Unit = {
    if (generatedSamples.size == 0) { mean = 0.0; variance = 1.0; return }
    mean = 0.0
    generatedSamples.foreach(s => mean += s.value)
    mean /= generatedSamples.size
    if (generatedSamples.size < minSamplesForVarianceEstimate) { variance = 1.0; return }
    variance = 0.0
    generatedSamples.foreach(s => { val diff = mean - s.value; variance += diff * diff })
    variance = Math.sqrt(variance / (generatedSamples.size - 1))
  }
}

// Consider using DenseVector instead of Array[Double] everywhere in this file

//trait AbstractDirichlet[O<:MultinomialOutcome[O]] extends GenerativeDistribution/*[Multinomial[O]]*/
trait AbstractDirichlet[O<:SingleIndexed] extends GenerativeDistribution[AbstractMultinomial[O]] {
	//type OutcomeType = AbstractMultinomial[O]
  def size : Int
	def alpha(index:Int) : Double
	def alphas : Seq[Double] = for (i <- 0 until size) yield alpha(i)
	def sum : Double
	def mean(index:Int) : Double
  def apply(index:Int) = alpha(index)
	def sampleValue : OutcomeType
 	def estimate : Unit = throw new Error("Method estimate is not implemented in this class.  You must add a trait for estimation.")
  //def sample(n:Int) : Seq[Multinomial[O]] = for (i <- 0 until n) yield sample
	def sampleInto(m:OutcomeType) : Unit = {
	  //println("sampleInto")
	  var norm = 0.0
	  val c = new Array[Double](m.size)
		for (val i <- 0 until c.length) {
		  //println("sampleInto alpha(i)="+alpha(i))
			c(i) = Maths.nextGamma(alpha(i), 1)(Global.random)
			if (c(i) <= 0.0) c(i) = 0.0001
			norm += c(i)
		}
	  norm /= sum
		for (i <- 0 until c.length) {
		  c(i) /= norm
		  //println("sampleInto c(i)="+c(i))
		  //if (ret.source != null) ret.counts(i) -= ret.source.alpha(i) // TODO consider putting this back!  Because now ret.pr(i) != c(i) !!!
      // coordinate with Dirichlet parameter estimation
    }
	  m.setCounts(c)
	}
  //def pr[O2<:OutcomeType](m:O2) : Double = throw new Error("Not yet implemented")
  def pr(m:OutcomeType) : Double = Math.exp(logpr(m))
  //def logpr[O2<:OutcomeType](m:O2) : Double = Math.log(pr(m))
  def logpr(m:OutcomeType) : Double = {
    var ret = Maths.logGamma(sum)
    for (i <- 1 until size) ret -= Maths.logGamma(alpha(i))
    for (i <- 1 until size) ret += alpha(i) * Math.log(m.pr(i))
    assert(ret == ret) // check for NaN
    ret
  }
}


// Make a general class Dirichlet to cover both constant and estimated varieties
// TODO Consider implementing this as a Multinomial plus a concentration parameter
abstract class Dirichlet[O<:MultinomialOutcome[O]](initialAlpha:Seq[Double], pseudoEvidence:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] {
	//println("Dirichlet")
  def this(initialAlpha:Double)(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).allocSize, initialAlpha), 0.1)(m)
  def this(initialAlpha:Seq[Double])(implicit m:Manifest[O]) = this(initialAlpha, 0.1)(m)
	type VariableType <: Dirichlet[O];
	class DomainInSubclasses
  protected val _mean: Array[Double] = new Array[Double](initialAlpha.length)
  protected var _meanNormalizer = pseudoEvidence
	protected var _sum = initialAlpha.foldLeft(0.0)(_+_)
	for (i <- 0 until _mean.length) _mean(i) = initialAlpha(i) * _meanNormalizer / _sum 
	def size = _mean.size
	val outcomeDomain = Domain[O](m)
	assert(initialAlpha.length == outcomeDomain.allocSize)
	/** Just for convenient access as a sequence; allocates a new Seq.  For per-index access use mean(i:Int).
  	This method name is slightly awkward, but I want to be sure that no one uses this when they really want mean(i:Int) */
	def means : RandomAccessSeq[Double] = new RandomAccessSeq[Double] { def apply(i:Int) = mean(i); def length = _mean.size } 
	@scala.inline final def mean(i:Int) = _mean(i) / _meanNormalizer
	@scala.inline final def alpha(i:Int) = mean(i) * _sum
	@scala.inline final def sum = _sum
 	def sampleValue = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
  override def generate(m:OutcomeType)(implicit d:DiffList) = {
		DirichletGenerateDiff
    super.generate(m)
  }
  override def ungenerate(m:OutcomeType)(implicit d:DiffList) = {
		DirichletUngenerateDiff
    super.ungenerate(m)
  }
  case class DirichletGenerateDiff(m:OutcomeType)(implicit d:DiffList) extends AutoDiff {
    def variable = Dirichlet.this
    def redo = { for (i <- 0 until size) _mean(i) += m.pr(i); _meanNormalizer += 1.0 }
    def undo = { for (i <- 0 until size) _mean(i) -= m.pr(i); _meanNormalizer -= 1.0 }
  }
  case class DirichletUngenerateDiff(override val m:OutcomeType)(implicit d:DiffList) extends DirichletGenerateDiff(m) {
    override def redo = super.undo
    override def undo = super.redo
  }
}
object Dirichlet {
  def apply[O<:MultinomialOutcome[O]](initialAlpha:Double)(implicit m:Manifest[O]) = new SymmetricDirichlet[O](initialAlpha)
  def apply[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) = new SymmetricDirichlet[O](1.0)
}
  
trait DirichletMomentMatchingEstimator[O<:MultinomialOutcome[O]] extends AbstractDirichlet[O] {
  this : Dirichlet[O] =>
  private def setUniform : Unit = {
    _sum = 1.0
    _meanNormalizer = uniformPseudoEvidence
    for (i <- 0 until _mean.length) _mean(i) = _meanNormalizer
  }
  def minSamplesForVarianceEstimate = 10
  /** Add a uniform pseudo-multinomial to estimation, with this weight relative to real multinomials */
  def uniformPseudoEvidence = 0.1 // Set to non-zero to avoid logGamma(0.0), leading to NaN
	override def estimate : Unit = {
	  if (generatedSamples.size == 0) { setUniform; return }
		// TODO this would be much easier if Multinomial and Dirichlet used scalala; I should convert eventually
		val smoothing = uniformPseudoEvidence / _mean.length
	  for (i <- 0 until _mean.length) _mean(i) = smoothing
	  val variance = new Array[Double](_mean.length)
	  for (m <- generatedSamples; i <- 0 until _mean.length) _mean(i) += m.count(i)/m.countTotal
	  _meanNormalizer = generatedSamples.size + uniformPseudoEvidence
	  //for (i <- 0 until _mean.length) _mean(i) /= (generatedSamples.size + uniformPseudoEvidence)
		//println("unnormalized mean "+_mean.take(20).toList)
		//println("normalized mean "+(new Range(0,_mean.size,1).map(mean(_)).toList))
		assert (Maths.almostEquals(1.0, new Range(0,size,1).foldLeft(0.0)(_+mean(_))))
		if (generatedSamples.size <= minSamplesForVarianceEstimate) { // Not enough samples for a reliable variance estimate
		  _sum = _mean.length
		  return
		}
		// Calculate variance = E[x^2] - E[x]^2 for each dimension
	  for (m <- generatedSamples; i <- 0 until _mean.length) {
	    val p = m.count(i)/m.countTotal
	    variance(i) += p * p
	  }
		for (i <- 0 until _mean.length) {
		  val a = mean(i)
		  variance(i) = (variance(i) / (generatedSamples.size - 1.0)) - a*a
		}
		//for (i <- 0 until _mean.length) variance(i) /= (generatedSamples.size - 1)
		//println("variance "+variance.take(10).toList)
	  _sum = 0.0
	  for (i <- 0 until _mean.length) 
	  	if (_mean(i) != 0.0) _sum += Math.log((mean(i) * (1.0 - mean(i)) / variance(i)) - 1.0)
	  	assert (_sum == _sum)
	  	// sum += Math.log(( partition[bin] * ( 1 - partition[bin] ) / variances[bin] ) - 1);
	  _sum = Math.exp(_sum / (_mean.length - 1))
	}
}

// Does not have its own Domain.  Size of alpha is Domain of O
class SymmetricDirichlet[O<:MultinomialOutcome[O]](initialAlpha:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] {
	type VariableType <: Dirichlet[O];
	class DomainInSubclasses
	type OutcomeDomainType = O
	val outcomeDomain = Domain[O](m)
	keepGeneratedSamples = false
	def size = outcomeDomain.size
	protected val meanAlpha = 1.0 / outcomeDomain.size 
	def alpha(index:Int) = initialAlpha
	def mean(index:Int) = meanAlpha
	lazy val sum = initialAlpha * outcomeDomain.size
 	def sampleValue = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
}


  
/** Trait for any distribution that might be selected as as part of a Multinomial mixture. 
Creates is own Domain.  Number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponent objects.
 Note that this is not a MultinomialOutcome, it is the *value* of MultinomialOutcome. */
//trait MixtureComponent[This<:MixtureComponent[This] with GenerativeDistribution] extends TypedSingleIndexedVariable[This] with GenerativeDistribution
trait MixtureComponent[This<:MixtureComponent[This] with AbstractGenerativeDistribution] extends TypedSingleIndexedVariable[This] with AbstractGenerativeDistribution {	this : This =>
	//type OutcomeType = This#OutcomeType
	type VariableType = This  // can we get away with this = ?
	type DomainType = IndexedDomain[VariableType] // We must make sure that this.domain is an IndexedDomain
	class DomainClass extends IndexedDomain[VariableType]
	//override type ValueType = This
	//override final def domain : Domain[This] = super.domain // TODO I think this line can be commented out???
	//def domain : Domain[This] = Domain[This].get(this.getClass)
  //println (this.getClass.getName+" MixtureComponent domain class = "+domain.getClass)
	_index = domain.asInstanceOf[IndexedDomain[This]].index(this) // TODO can we avoid this cast?
	//println("Creating MixtureComponent this.getClass = "+this.getClass.toString+" index="+_index+" domain size="+domain.asInstanceOf[IndexedDomain[This]].size)
	override final def setByIndex(index:Int)(implicit d:DiffList) : Unit = new Error
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
  def choice : M = domain.get(index)
  _index = Global.random.nextInt(domain.size) // TODO is this how _index should be initialized?
  if (!Global.defaultModel.contains(MixtureChoiceTemplate)) Global.defaultModel += MixtureChoiceTemplate
  // The following is done in MultinomialOutcome[This]:  type DomainType = IndexedDomain[M]; class DomainClass extends IndexedDomain[M]
  Domain.alias[This,M](mt,mm)
	private var _outcome : M#OutcomeType = _
	def outcome : M#OutcomeType = _outcome // The particular outcome that was generated from this choice of mixture component
	def setOutcome(o:M#OutcomeType) = 
		if (_outcome == null) _outcome = o
		else throw new Error
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
	  if (_outcome == null) throw new Error("No outcome yet set.")
	  choice.unsafeUngenerate(outcome) // was unsafe
	  super.setByIndex(newIndex) // this changes the value of 'choice'
	  choice.unsafeGenerate(outcome) // was unsafe
	}
  def logpr : Double = choice.unsafeLogpr(outcome) + source.logpr(index) // was unsafe
}

  
    
// TODO Consider renaming this MultinomialSample, because the instances of this class are individual samples (e.g. token).  "outcome" may indicate the value (e.g. type) 
trait MultinomialOutcome[This<:MultinomialOutcome[This] with SingleIndexed] extends SingleIndexed {
	this : This =>
  type SourceType = AbstractMultinomial[This]
	class DomainInSubclasses
	type OutcomeDomainType = This // TODO No longer necessary, I think
	var source : SourceType = _
	def setSource(m:SourceType)(implicit d:DiffList) : Unit = {
		if (m == null) throw new IllegalArgumentException("MultinomialOutcome cannot have null source")
		if (d != null) d += new MultinomialOutcomeSourceChangeDiff(source, m)
		if (source != null) source.ungenerate(this)
		source = m
		//println("Multinomial Outcome setSource on outcome "+this+" index="+index)
		source.generate(this)
	}
	def ~(m:SourceType) : this.type = {
		setSource(m)(null); 
		this 
	}
	def ~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = {
		mmc.setOutcome(this); 
		this.~(mmc.choice) // either here or in mmc.setOutcome; not sure which is more natural
	}
	case class MultinomialOutcomeSourceChangeDiff(oldSource:SourceType, newSource:SourceType) extends Diff {
		def variable = MultinomialOutcome.this
		def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
		def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
	}
}

/** A MultinomialOutcome that is also a SingleIndexedVariable */
trait MultinomialOutcomeVariable[This<:MultinomialOutcomeVariable[This] with SingleIndexedVariable] extends SingleIndexedVariable with MultinomialOutcome[This] {
  this : This =>
	/** Attach to Multinomial generator m, and also set my value to something sampled from the generator. */
	def :~(m:SourceType) : this.type = {
		_index = m.sampleValue // TODO Do this instead by setByIndex()()?  But causes some problems...
		this.~(m)
	}
	/** Attach to the Multinomial indicated by MixtureChoice mmc, and also set my value sampled from the generator. */
	def :~[M<:SourceType](mmc:MixtureChoice[M,_]) : this.type = {
		_index = mmc.choice.sampleValue // TODO Do this instead by setByIndex()()?  But causes some problems...
		this.~(mmc)
	}
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
		if (source != null) source.ungenerate(this)
		super.setByIndex(newIndex)
		if (source != null) source.generate(this)
	}
}
  
/** The outcome of a coin flip, with boolean value.  this.value:Boolean */
case class Flip extends CoordinatedBool with MultinomialOutcomeVariable[Flip]
case class Coin(p:Double, totalCount:Double) extends Multinomial[Flip](Array((1-p)*totalCount,p*totalCount)) {
	def this(p:Double) = this(p:Double, 1.0)
	def this() = this(0.5)
	assert (p >= 0.0 && p <= 1.0)
	def flip : Flip = { val f = new Flip; f.setByIndex(this.sampleValue)(null); f }
	def flip(n:Int) : Seq[Flip] = for (i <- 0 until n force) yield flip
	def pr(f:Boolean) : Double = if (f) pr(1) else pr(0)
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}

/*
class GenericMultinomialOutcome extends MultinomialOutcome[GenericMultinomialOutcome]
class GenericMultinomial extends Multinomial[GenericMultinomialOutcome] with MixtureComponent[GenericMultinomial]
class GenericMultinomialMixtureComponent extends MultinomialMixtureChoice[GenericMultinomial,GenericMultinomialOutcome,GenericMultinomialMixtureComponent]
////class MultinomialMixtureChoiceTemplate[This<:MultinomialMixtureChoice[_<:Multinomial[_<:MultinomialOutcome[_]],_<:MultinomialOutcome[_],_]](implicit m:Manifest[This]) extends TemplateWithStatistics1[This]()(m) {
////class MultinomialMixtureChoiceTemplate extends TemplateWithStatistics1[MultinomialMixtureChoice[GenericMultinomial,GenericOutcome,MultinomialMixtureChoice[_,_,_]]]()(Manifest.classType[MultinomialMixtureChoice[GenericMultinomial,GenericOutcome,_]]) {
object MultinomialMixtureChoiceTemplate extends TemplateWithStatistics1[MultinomialMixtureChoice[GenericMultinomial,GenericMultinomialOutcome,GenericMultinomialMixtureComponent]] {
  //val mmcc = classOf[MultinomialMixtureChoice[_,_,_]]; println("Template class="+nc1)
  def score(s:Stat) = {
		val mmc = s.s1
		mmc.multinomial.logpr(s.s1.outcome.index) + mmc.source.logpr(mmc.index)
	}
}
*/


/*
class GenericMixtureComponent extends MixtureComponent[GenerativeDistribution]
class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]
class TypedGenericMixtureChoice[D<:GenerativeDistribution] extends MixtureChoice[MixtureComponent[D],TypedGenericMixtureChoice[D]]
// trait MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]] extends MultinomialOutcome[This] {
class MixtureChoiceTemplate[D<:GenerativeDistribution] extends TemplateWithStatistics1[MixtureChoice[MixtureComponent[D],TypedGenericMixtureChoice[D]]] {
  def score(s:Stat) = {
		val mmc = s.s1
		mmc.choice.unsafeLogpr(mmc.outcome) + mmc.source.unsafeLogpr(mmc.choice)
	}
}
*/

class GenericMultinomialOutcome extends MultinomialOutcome[GenericMultinomialOutcome] { def index = -1 }
// TODO Is this arbitrary "2" below OK?
class GenericMixtureComponent extends DenseMultinomial[GenericMultinomialOutcome](2) with MixtureComponent[GenericMixtureComponent]
class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]
// trait MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]] extends MultinomialOutcome[This] {
object MixtureChoiceTemplate extends TemplateWithStatistics1[MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]] {
  def score(s:Stat) = { /*println("MixtureChoiceTemplate score");*/ s.s1.logpr }
}

