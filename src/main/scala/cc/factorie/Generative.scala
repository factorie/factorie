package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection of Variables and Factors relevant to generative models

// TODO Think more carefully about the hierarchy of Variables that are parameters 
trait GenerativeDistribution extends Variable {
  type VariableType <: GenerativeDistribution
  type OutcomeType <: Variable
	def estimate : Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?
	// This odd arg type below is due to:
	// http://www.nabble.com/Fwd:--lift--Lift-with-Scala-2.6.1--td14571698.html	
	//def pr[O<:OutcomeType](o:O) : Double
  // This caused the compiler to crash. For now we have the "unsafe" work-around below.
  lazy val generatedSamples = new HashSet[OutcomeType];
  var keepGeneratedSamples = true
	def generate(o:OutcomeType)(implicit d:DiffList) = 
   if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
   else if (keepGeneratedSamples) {
     generatedSamples += o
     if (d != null) d += GenerativeDistributionGenerateDiff(o)
   }
  // TODO Add Handling of DiffList!!!!
  def ungenerate(o:OutcomeType)(implicit d:DiffList) = if (keepGeneratedSamples) {
  	generatedSamples -= o
  	if (d != null) d += GenerativeDistributionUngenerateDiff(o)
  }	
  //def generate(o:OutcomeType)(implicit d:DiffList)
	//def ungenerate(o:OutcomeType)(implicit d:DiffList)
	def pr(o:OutcomeType) : Double
	def logpr(o:OutcomeType) : Double
	// Fighting with the Scala type system
	// See http://www.nabble.com/Path-dependent-type-question-td16767728.html
	def unsafeGenerate(o:Variable)(implicit d:DiffList) = generate(o.asInstanceOf[OutcomeType])
	def unsafeUngenerate(o:Variable)(implicit d:DiffList) = ungenerate(o.asInstanceOf[OutcomeType])
	def unsafePr(o:Variable) = pr(o.asInstanceOf[OutcomeType])
	def unsafeLogpr(o:Variable) = logpr(o.asInstanceOf[OutcomeType])
	case class GenerativeDistributionGenerateDiff(m:OutcomeType) extends Diff {
		def variable = GenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
		def undo = { generatedSamples -= m }
	}
 	case class GenerativeDistributionUngenerateDiff(m:OutcomeType) extends Diff {
		def variable = GenerativeDistribution.this.asInstanceOf[VariableType]
		def redo = { generatedSamples -= m }
		def undo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
	}
}

class Gaussian1(initialMean:Double, initialVariance:Double) extends GenerativeDistribution {
  type OutcomeType = Real
  private var mean = initialMean
  private var variance = initialVariance
  def sample : Double = Maths.nextGaussian(mean,variance)(Global.random)
  def sampleInto(o:Real) : Unit = o.set(sample)(null) // TODO should we put a difflist here?
  def logpr(x:Double) : Double = {
    val diff = x - mean
    return - diff * diff / (2 * variance) - 0.5 * Math.log(2 * Math.Pi * variance)
  }
  def logpr(o:Real) = logpr(o.value)
  def pr(x:Double) = Math.exp(x)
  def pr(o:Real) = pr(o.value)
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

trait AbstractDirichlet[O<:MultinomialOutcome[O]] extends GenerativeDistribution/*[Multinomial[O]]*/ {
  type OutcomeType = Multinomial[O]
  def size : Int
	def alpha(index:Int) : Double
	def alphas : Seq[Double] = for (i <- 0 until size) yield alpha(i)
	def sum : Double
	def mean(index:Int) : Double
  def apply(index:Int) = alpha(index)
	def generate(o:Multinomial[O])(implicit d:DiffList) : Unit
	def ungenerate(o:Multinomial[O])(implicit d:DiffList) : Unit
	def sample : Multinomial[O]
 	def estimate : Unit = throw new Error("Method estimate is not implemented in this class.  You must add a trait for estimation.")
  //def sample(n:Int) : Seq[Multinomial[O]] = for (i <- 0 until n) yield sample
	def sampleInto(m:Multinomial[O]) : Unit = {
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
  def pr(m:Multinomial[O]) : Double = Math.exp(logpr(m))
  //def logpr[O2<:OutcomeType](m:O2) : Double = Math.log(pr(m))
  def logpr(m:Multinomial[O]) : Double = {
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
 	def sample : Multinomial[O] = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
  override def generate(m:Multinomial[O])(implicit d:DiffList) = {
		DirichletGenerateDiff
    super.generate(m)
  }
  override def ungenerate(m:Multinomial[O])(implicit d:DiffList) = {
		DirichletUngenerateDiff
    super.ungenerate(m)
  }
  case class DirichletGenerateDiff(m:Multinomial[O])(implicit d:DiffList) extends AutoDiff {
    def variable = Dirichlet.this
    def redo = { for (i <- 0 until size) _mean(i) += m.pr(i); _meanNormalizer += 1.0 }
    def undo = { for (i <- 0 until size) _mean(i) -= m.pr(i); _meanNormalizer -= 1.0 }
  }
  case class DirichletUngenerateDiff(override val m:Multinomial[O])(implicit d:DiffList) extends DirichletGenerateDiff(m) {
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
	  for (m <- generatedSamples; i <- 0 until _mean.length) _mean(i) += m.counts(i)/m.countsTotal
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
	    val p = m.counts(i)/m.countsTotal
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
 	def sample = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
}

class UniformMultinomial[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) extends Multinomial[O](null)(m)

// Does not have its own Domain.  Size of pr is Domain of O
// TODO should this Iterate over [O] or over [O#VariableType#ValueType] ???
class Multinomial[O<:MultinomialOutcome[O]](initCounts:Seq[Double])(implicit m:Manifest[O]) extends Variable with GenerativeDistribution with Iterable[O#VariableType#ValueType] {
  def this()(implicit m:Manifest[O]) = this(null)(m)
  type OutcomeType = O
  type VariableType <: Multinomial[O];
	class DomainInSubclasses
	type OutcomeDomainType = O // TODO No longer necessary, I think
	val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance
	lazy val _counts = new Array[Double](outcomeDomain.allocSize)
	private var total : Double = 0.0
	def countsTotal = total
	def counts : RandomAccessSeq[Double] = _counts
	def setCounts(c:Seq[Double]) : Unit = {
	  assert(c.length == _counts.length)
	  total = 0.0
	  var i = 0
	  c.foreach(x => {_counts(i) = x; total += x; i += 1}) 
	}
	if (initCounts != null) setCounts(initCounts)
	def size = _counts.length
	var source : AbstractDirichlet[O] = _
	def setSource(dir:AbstractDirichlet[O])(implicit d:DiffList) : Unit = {
	  // TODO: consider not calling ungenerate and generate here.  Setting the source shouldn't change the Multinomial parameters immediately
		if (d != null) d += MultinomialSetSourceDiff(source, dir)
		if (source != null) source.ungenerate(this) 
		source = dir
		source.generate(this)
	}
	def ~[D<:Dirichlet[O]](mc:MixtureChoice[D,_]) : this.type = {
		mc.setOutcome(this); 
		this.~(mc.choice) // either here or in mmc.setOutcome; not sure which is more natural
	}
	def ~(d:AbstractDirichlet[O]) : this.type /*Multinomial[O]*/ = { setSource(d)(null); this }
	def :~(d:AbstractDirichlet[O]) : this.type = { setSource(d)(null); d.sampleInto(this); this }
	def :=~(d:AbstractDirichlet[O]) : this.type = { 
	  setSource(d)(null)
	  for (i <- 0 until _counts.length) _counts(i) = d.alpha(i)
	  total = d.sum
	  this
	}
	/** Set source, and incrementally update the parameters of this Multinomial */
	def ~:(o:O) : this.type = { o.setSource(this)(null); increment(o); this }
	def increment(o:O) = { _counts(o.index) += 1.0; total += 1.0 }
	def unincrement(o:O) = { _counts(o.index) -= 1.0; total -= 1.0 }
	override def generate(o:O)(implicit d:DiffList) = { 
		//println("Multinomial.outcomeDomain.size="+outcomeDomain.size+" generate "+o+" size="+size); Console.flush; 
		_counts(o.index) += 1.0; total += 1.0
		if (d != null) d += MultinomialGenerateDiff(o.index)
	}
	override def ungenerate(o:O)(implicit d:DiffList) = { 
		_counts(o.index) -= 1.0; assert(_counts(o.index) >= 0.0)
		total -= 1.0; assert(total >= 0.0)
		if (d != null) d += MultinomialUngenerateDiff(o.index)
	}
	def estimate : Unit = {} // Nothing to be done; constantly keeps itself estimated
	def nextOutcomeValue : O#VariableType#ValueType = outcomeDomain.get(sample) 
	def sample : Int = {
		val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = outcomeDomain.size
		while (i < size) {
			sum += pr(i)
			if (sum >= s) return i
			i += 1
		}
		return size - 1
	}
	def sample(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples force) yield sample
	def elements : Iterator[O#VariableType#ValueType] = new Iterator[O#VariableType#ValueType] {
		def hasNext = true
		def next = nextOutcomeValue
	}
	def pr(index:Int) : Double = {
		//println("Multinomial.pr "+counts(index)+" "+source(index)+" "+total+" "+source.sum)
		if (/*false &&*/ source != null)
			(_counts(index) + source.alpha(index)) / (total + source.sum)
		else
			_counts(index) / total
		}
	def pr(o:O) : Double = pr(o.index)
	def pr(os:Seq[O]) : Double = os.foldLeft(1.0)(_*pr(_))
	def prs : RandomAccessSeq[Double] = new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = _counts.size}
	def logpr(o:O) : Double = Math.log(pr(o))
	def logpr(index:Int) = Math.log(pr(index))
	def logpr(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
	def top(n:Int) = _counts.zipWithIndex.sortReverse({case (c,i)=>c}).take(n).toList.map({case (c,i)=>(outcomeDomain.get(i),i,pr(i),c)})
	def topWords(n:Int) = top(n).toList.map(_._1)
	override def toString = "Multinomial(count="+total+")"
	case class MultinomialGenerateDiff(i:Int) extends Diff {
		def variable = Multinomial.this
		def redo = { _counts(i) += 1.0; total += 1.0 }
		def undo = { _counts(i) -= 1.0; total -= 1.0 }
	}
	case class MultinomialUngenerateDiff(i:Int) extends Diff {
		def variable = Multinomial.this
		def redo = { _counts(i) -= 1.0; total -= 1.0 }
		def undo = { _counts(i) += 1.0; total += 1.0 }
	}
	case class MultinomialSetSourceDiff(oldSource:AbstractDirichlet[O], newSource:AbstractDirichlet[O]) extends Diff {
		def variable = Multinomial.this
		def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
		def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
	}
}

  
/** Trait for any distribution that might be selected as as part of a Multinomial mixture. 
Creates is own Domain.  Number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponent objects.
 Note that this is not a MultinomialOutcome, it is the *value* of MultinomialOutcome. */
trait MixtureComponent[This<:MixtureComponent[This] with GenerativeDistribution] extends TypedSingleIndexedVariable[This] with GenerativeDistribution {
	this : This =>
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


// Usage: 
// class Topic extends Multinomial[Word] with MixtureComponent[Topic]
// class Z extends MixtureChoice[Topic,Z]
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
	  choice.unsafeUngenerate(outcome)
	  super.setByIndex(newIndex) // this changes the value of 'choice'
	  choice.unsafeGenerate(outcome)
	}
  def logpr : Double = choice.unsafeLogpr(outcome) + source.logpr(index)
}

  
    
// TODO Consider renaming this MultinomialSample, because the instances of this class are individual samples (e.g. token).  "outcome" may indicate the value (e.g. type) 
trait MultinomialOutcome[This<:MultinomialOutcome[This] with SingleIndexed] extends SingleIndexed {
	this : This =>
	class DomainInSubclasses
	type OutcomeDomainType = This // TODO No longer necessary, I think
	var source : Multinomial[This] = _
	def setSource(m:Multinomial[This])(implicit d:DiffList) : Unit = {
		if (m == null) throw new IllegalArgumentException("MultinomialOutcome cannot have null source")
		if (d != null) d += new MultinomialOutcomeSourceChangeDiff(source, m)
		if (source != null) source.ungenerate(this)
		source = m
		//println("Multinomial Outcome setSource on outcome "+this+" index="+index)
		source.generate(this)
	}
	def ~(m:Multinomial[This]) : this.type = {
		setSource(m)(null); 
		this 
	}
	def ~[M<:Multinomial[This]](mmc:MixtureChoice[M,_]) : this.type = {
		mmc.setOutcome(this); 
		this.~(mmc.choice) // either here or in mmc.setOutcome; not sure which is more natural
	}
	case class MultinomialOutcomeSourceChangeDiff(oldSource:Multinomial[This], newSource:Multinomial[This]) extends Diff {
		def variable = MultinomialOutcome.this
		def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
		def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
	}
}

trait MultinomialOutcomeVariable[This<:MultinomialOutcomeVariable[This] with SingleIndexedVariable] extends SingleIndexedVariable with MultinomialOutcome[This] {
  this : This =>
	/** Attach to Multinomial generator m, and also set my value to something sampled from the generator. */
	def :~(m:Multinomial[This]) : this.type = {
		_index = m.sample // TODO Do this instead by setByIndex()()?  But causes some problems...
		this.~(m)
	}
	/** Attach to the Multinomial indicated by MixtureChoice mmc, and also set my value sampled from the generator. */
	def :~[M<:Multinomial[This]](mmc:MixtureChoice[M,_]) : this.type = {
		_index = mmc.choice.sample // TODO Do this instead by setByIndex()()?  But causes some problems...
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
	def flip : Flip = { val f = new Flip; f.setByIndex(this.sample)(null); f }
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
class GenericMixtureComponent extends Multinomial[GenericMultinomialOutcome] with MixtureComponent[GenericMixtureComponent]
class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]
// trait MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]] extends MultinomialOutcome[This] {
object MixtureChoiceTemplate extends TemplateWithStatistics1[MixtureChoice[GenericMixtureComponent,GenericMixtureChoice]] {
  def score(s:Stat) = { /*println("MixtureChoiceTemplate score");*/ s.s1.logpr }
}

