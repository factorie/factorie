/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._
import cc.factorie.util.SeqAsVector
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}


/*trait Outcome
trait DiscreteOutcome { 
  type ValueType = Int
}
trait RealOutcome {
  type ValueType = Double
}
trait PositiveRealOutcome extends RealOutcome
trait CountOutcome {
  type ValueType = Int
}*/


/*
trait OutcomeGenerating[T] {
  def sample:T
  def pr(x:T): Double
  def logpr(x:T): Double
}*/

// TODO Rename MultinomialOutcome DiscreteOutcome


/** Base of the Multinomial class hierarchy, abstract, needing only methods length, pr, and set. 
    @author Andrew McCallum */
// TODO Rename simply "Multinomial"?
@DomainInSubclasses
trait AbstractMultinomial[O<:DiscreteValue] extends GenerativeDistributionLike[AbstractMultinomial[O],O] with /*GenerativeVariable[ProportionOutcome[O]] with*/ DiscreteDistribution[O] with GeneratedProportionValue[O] {
  type VariableType <: AbstractMultinomial[O];
  //type OutcomeType = O
  //type SourceType = ProportionGenerating[O];
  type SourceType <: GenerativeDistributionLike[ProportionDistribution[O],GeneratedProportionValue[O]]; // TODO Just changed from = to <: !!!!
  //type SourceType = AbstractDirichlet[O]; // TODO Why can't I make this ProportionGenerating[O] instead of AbstractDirichlet[O]?
  //def asOutcome = this
  //def this(initCounts:Seq[Double]) = { this(initCounts.size); setCounts(initCounts) }
  def length: Int
  def apply(index:Int) = pr(index)
  def set(proportions:Seq[Double]): Unit // TODO include a DiffList here?
  def pr(index:Int) : Double
  def localPr(index:Int) = pr(index) // Alternative that should not use any source.alpha in the estimate; used in Dirichlet.estimate
  final def pr(o:O) : Double = pr(o.index)
  def proportion: RandomAccessSeq[Double] = this
  def prs: RandomAccessSeq[Double] = this //new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = count.size } // TODO Remove this method?
  def logpr(index:Int) = Math.log(pr(index))
  def logprIndices(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  def prIndices(indices:Seq[Int]) : Double = indices.foldLeft(1.0)(_*pr(_))
  def pr(outcomes:Seq[O]) : Double = prIndices(outcomes.map(_.index))
  def counts: { def apply(i:Int):Double; def size:Int } = {
    val c = new Array[Double](length)
    generatedSamples.foreach(s => c(s.index) += 1.0)
    c
  }
  def prVector: Vector = new SeqAsVector { def apply(i:Int) = pr(i); def length = size } // TODO Bad idea?  Is Vector.elements supposed to have type Iterable[(Int,Double)]? 
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit = set(s.asGenerativeDistribution.sampleProportionsWithCounts(counts))
  @deprecated def sampleInto(o:O): Unit = o match { // TODO Consider including implicit DiffList here? 
    case v:DiscreteVariable => v.setByIndex(sampleIndex)(null)
    case _ => throw new Error("Trying to sample into immutable Variable") // TODO Is this OK to just do nothing here?;
    // TODO if so, then we really don't need the separation between GenerativeObservation and GenerativeVariable!...  Or perhaps not?
  }
  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)}).filter(_.pr > 0.0)
  // TODO Put next method in superclass
  def sample(implicit d:DiffList): Unit = { if (generativeSource != null) this.sampleFrom(generativeSource) else throw new Error("Source not set") } 
  def sampleInt: Int = sampleIndex
  def sampleIndex: Int = {
    val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += pr(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  def sampleIndexIterator = new Iterator[Int] {
    def hasNext = true
    def next = sampleIndex
  }
  def maxPrIndex: Int = {
    var i = 0; var mp = pr(i); var mi = i
    while (i < size) { if (mp < pr(i)) { mp = pr(i); mi = i }; i += 1 }
    mi
  }
  def sampleIndices(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples force) yield sampleIndex
  def estimate: Unit = { throw new Error("Not yet implemented")} // TODO What to put here?
}

/** Compact representation of immutable Multinomial with equal probability for all outcomes. 
    @author Andrew McCallum */
class UniformMultinomial[O<:GeneratedDiscreteValue[O]](implicit m:Manifest[O]) extends AbstractMultinomial[O] {
  val length: Int = Domain[O](m).size
  val pr1 = 1.0/length
  final def pr(index:Int) = pr1
  def set(proportions:Seq[Double]): Unit = throw new Error("UniformMultinomial cannot be changed.")
}

/** A mixture of a fixed set of Multinomials, i.e. you cannot add or remove Multinomials from the mixture. 
    @author Andrew McCallum */
class MultinomialMixture[M<:AbstractMultinomial[O],O<:GeneratedDiscreteValue[O]](ms:Seq[M], ps:Seq[Double]) extends AbstractMultinomial[O] {
  // TODO How can I avoid needing both M and O as type parameters.  I think M should automatically specify O.
  type SourceType = GenerativeDistributionLike[ProportionDistribution[O],GeneratedProportionValue[O]];
  val components = ms.toArray
  val length: Int = components.first.length
  val proportions = new DenseCountsMultinomial(ps)
  def pr(index:Int) = {
    var p = 0.0
    for (i <- 0 until proportions.length) p += proportions.pr(i) * components(i).pr(index)
    p
  }
  def set(m:Seq[Double]): Unit = throw new Error("MultinomialMixture cannot be set.")
}

/** Simple Multinomial that stores p(x) directly in a Scalala DenseVector. 
    @author Andrew McCallum */
class DenseMultinomial[O<:GeneratedDiscreteValue[O]](proportions:Seq[Double]) extends AbstractMultinomial[O] {
  type SourceType = GenerativeDistributionLike[ProportionDistribution[O],GeneratedProportionValue[O]];
  def this(dim:Int) = this(Array.make(dim, 1.0/dim))
  def this()(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).size, 1.0/Domain[O](m).size))
  val length: Int = proportions.length
  val _pr = new DenseVector(length)
  set(proportions) //if (proportions != null)
  /** Will normalize for you, if not already normalized. */
  def set(proportions:Seq[Double]): Unit = {
    assert (proportions.length == length)
    val sum = proportions.foldLeft(0.0)(_+_)
    assert (sum > 0.0)
    for (i <- 0 until length) _pr(i) = proportions(i)/sum
  }
  def setDirac(index:Int): Unit = {
    _pr.zero
    _pr(index) = 1.0
  }
  @inline final def pr(index:Int) = _pr(index)
}

/** A Multinomial that stores its parameters as a collection of "outcome counts" and their total. 
    The counts themselves are stored in a Scalala Vector '_counts', which is abstract in this class.
    @author Andrew McCallum */
@DomainInSubclasses
trait CountsMultinomial[O<:DiscreteValue] extends AbstractMultinomial[O] {
  type VariableType <: CountsMultinomial[O];
  override type SourceType = GenerativeDistributionLike[AbstractDirichlet[O],GeneratedProportionValue[O]]
  def length: Int = counts.size 
  def smoothing = 0.0
  private var total : Double = 0.0
  def countsTotal = total
  def countTotal = total + smoothing * length
  protected val _counts: { def apply(i:Int): Double; def update(i:Int, x:Double): Unit; def size:Int }
  override def counts: { def apply(i:Int):Double; def update(i:Int,v:Double):Unit; def size:Int } = _counts // TODO Want Seq[Double], but Vector doesn't mixin Seq?!!
  def count(index:Int): Double = counts(index) + smoothing
  def increment(index:Int, incr:Double)(implicit d:DiffList) = { // TODO Scala 2.8 add incr:Double=1.0
    _counts(index) += incr; total += incr // TODO Wow, in debugging I see BoxesRunTime.unboxToInt(Object) following DenseVector.apply(Object) line 35
    if (_counts(index) < 0.0) println("CountsMultinomial "+this.getClass.getName+" counts="+_counts(index)+" incr="+incr)
    assert(_counts(index) >= 0.0)
    assert(total >= 0.0)
    if (d != null) d += new CountsMultinomialIncrementDiff(index, incr)
  }
  def increment(o:O, incr:Double)(implicit d:DiffList): Unit = increment(o.index, incr)
  def increment(s:Seq[Double])(implicit d:DiffList): Unit = {
    assert(s.length == length)
    for (i <- 0 until length) { _counts(i) += s(i); total += s(i) }
    if (d != null) d += new CountsMultinomialSeqIncrementDiff(s)
  }
  // TODO Note that this does not factor in smoothing.  I think "smoothing" should go into a trait.
  def set(c:Seq[Double]): Unit = { // TODO Add DiffList here?
    assert(c.length == _counts.size)
    total = 0.0
    // Make the assignment, attempting to preserve sparsity
    var i = 0; c.foreach(x => { if (x > 0.0 || _counts(i) != 0.0) _counts(i) = x; total += x; i += 1}) 
  }
  // Raw operations on count vector, without Diffs
  def zero(): Unit = { total = 0.0; for (i <- 0 until size) _counts(i) = 0.0 }
  def increment(m:AbstractMultinomial[O], rate:Double): Unit = { 
    total += norm(m,1)
    for (i <- 0 until size) _counts(i) += m(i) }  
  def pr(index:Int) : Double = if (countsTotal == 0) 1.0 / size else count(index) / countTotal
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * countTotal; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += count(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  override def estimate: Unit = { // TODO Think about how this might make SparseCountsMultinomial unsparse, and how to improve
    if (generatedSamples.isEmpty) throw new Error("No generated samples from which to estimate")
    if (generativeSource == null) { zero }
    else { 
      total = generativeSource.asGenerativeDistribution.alphaSum
      for (i <- 0 until size) _counts(i) = generativeSource.asGenerativeDistribution.alphaVector(i)
    }
    generatedSamples.foreach(o => {
      o match { // TODO clean this up
        case o2:GeneratedDiscreteValue[O] => o2.generativeSource match {
          case mixture:MarginalizedMixtureChoice[SourceType,O,_] =>
            for (i <- 0 until length) { _counts(i) += mixture.multinomial(i); total += 1.0 }
          case _ => { _counts(o.index) += 1.0; total += 1.0 }
        }
        case _ => { _counts(o.index) += 1.0; total += 1.0 }
      }
    })
  }
  class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))}).filter(_.pr > 0.0)
  case class CountsMultinomialIncrementDiff(index:Int, incr:Double) extends Diff {
    def variable = CountsMultinomial.this
    def undo = { _counts(index) -= incr; total -= incr }
    def redo = { _counts(index) += incr; total += incr }
  }
  case class CountsMultinomialSeqIncrementDiff(s:Seq[Double]) extends Diff {
    def variable = CountsMultinomial.this
    def undo = { for (i <- 0 until length) { _counts(i) -= s(i); total -= s(i) } }
    def redo = { for (i <- 0 until length) { _counts(i) += s(i); total += s(i) } }
  }
}

class GrowableCountsMultinomial[O<:DiscreteValue](implicit m:Manifest[O]) extends CountsMultinomial[O] {
  type VariableType <: GrowableCountsMultinomial[O];
  override protected val _counts = new {
    private val h = new it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap
    private val d = Domain[O](m)
    def size: Int = d.size
    def apply(key:Int) = h.get(key)
    def update(key:Int, value:Double): Unit = h.put(key, value)
  }
}

trait VectorCountsMultinomial[O<:GeneratedDiscreteValue[O]] extends CountsMultinomial[O] {
  type VariableType <: VectorCountsMultinomial[O];
  override protected val _counts: Vector
  override def prVector: Vector = _counts / countsTotal
}

trait SparseMultinomial[O<:GeneratedDiscreteValue[O]] extends AbstractMultinomial[O] {
  def activeIndices: scala.collection.Set[Int];
  //def sampleIndexProduct(m2:SparseMultinomial[O]): Int // TODO
}

/** A Multinomial that stores its counts in a Scalala SparseVector. 
    @author Andrew McCallum */
@DomainInSubclasses
class SparseCountsMultinomial[O<:GeneratedDiscreteValue[O]](dim:Int) extends VectorCountsMultinomial[O] with SparseMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: SparseCountsMultinomial[O]
  protected val _counts = new SparseVector(dim)
  def default = _counts.default
  def default_=(d:Double) = _counts.default = d
  def activeIndices: scala.collection.Set[Int] = _counts.activeDomain
  // def sampleIndex: Int // TODO
}

/** Multinomial for which sampling is efficient because outcomes are considered in order of highest-count first.
    This implementation is not yet finished.
    @author Andrew McCallum */
@deprecated // Not finished
abstract class SortedSparseCountsMultinomial[O<:GeneratedDiscreteValue[O]](dim:Int) extends AbstractMultinomial[O] with SparseMultinomial[O] {
  def length: Int = pos.length
  private var total: Int = 0 // total of all counts in buf
  // Make sure we have enough bits to represent the dimension of the multinomial
  private val topicMask = if (Integer.bitCount(dim) == 1) dim-1 else Integer.highestOneBit(dim) * 2 - 1
  private val topicBits = Integer.bitCount(topicMask)
  private var bufsize = 32
  private var siz = 0 // current size of buf 
  private val buf = new Array[Int](bufsize) // stores both count and topic packed into a single Int, indexed by pos
  assert (dim < Math.MAX_SHORT)
  private val pos = new Array[Short](dim); for (i <- 0 until dim) pos(i) = -1 // mapping from index to pos in count 
  private def ti(pos:Int) = buf(pos) & topicMask // topic at position 
  private def co(pos:Int) = buf(pos) >> topicBits // count at position
  def incrementCount(index:Int, incr:Int): Unit = { val p:Int = pos(index); buf(p) = (co(p) + incr) }
}

/** A Multinomial that stores its counts in a Scalala DenseVector.
    @author Andrew McCallum */
@DomainInSubclasses
class DenseCountsMultinomial[O<:GeneratedDiscreteValue[O]](dim:Int) extends VectorCountsMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: DenseCountsMultinomial[O]
  protected val _counts = new DenseVector(dim)
}

/** A Multinomial with parameters integrated out with a Dirichlet prior.  Also known as a Multivariate Polya distribution.
    @author Andrew McCallum */
// TODO Figure out how to use intead [O<:GeneratedDiscreteValue[O]], but still get O#VariableType#ValueType in "top" below
@DomainInSubclasses
class DirichletMultinomial[O<:GeneratedCategoricalValue[O]](dirichlet:AbstractDirichlet[O])(implicit m:Manifest[O]) extends DenseCountsMultinomial[O](Domain[O](m).size) {
  def this()(implicit m:Manifest[O]) = this(null.asInstanceOf[AbstractDirichlet[O]])(m)
  def this(dirichlet:AbstractDirichlet[O], initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(dirichlet)(m); set(initCounts) }
  def this(initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(null.asInstanceOf[AbstractDirichlet[O]])(m); set(initCounts) }
  type VariableType <: DirichletMultinomial[O];
  //override type SourceType = GenerativeDistributionLike[AbstractDirichlet[O],ProportionOutcome[O]]
  val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance; but otherwise 'm' would be stored for each instance anyway?
  def actualGenerativeSource = generativeSource.asGenerativeDistribution
  override def pr(index:Int) : Double = {
    //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
    if (generativeSource != null)
      (counts(index) + actualGenerativeSource.alpha(index)) / (countsTotal + actualGenerativeSource.alphaSum) // I considered +1 to both numerator and denominator in order to encourge unused mixture components to get data, but what objective does this correspond to?
    else if (countsTotal == 0) // Use super.pr here instead?  But this may be faster
      1.0 / size
    else
      counts(index) / countsTotal
  }
  override def localPr(index:Int): Double = super.pr(index)
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * (countsTotal + actualGenerativeSource.alphaSum); var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += counts(i) + actualGenerativeSource.alpha(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  /** Probability of a collection of counts; see http://en.wikipedia.org/wiki/Multivariate_Polya_distribution. */
  // TODO Not tested!
  def pr(ocounts:Vector) : Double = {
    import Maths.{gamma => g}
    assert (ocounts.size == length)
    val n = norm(ocounts,1)
    val normalizer1 = g(n) / ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2))
    val normalizer2 = g(actualGenerativeSource.alphaSum) / g(n+actualGenerativeSource.alphaSum)
    val ratio = ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2+actualGenerativeSource.alpha(e._1)/g(actualGenerativeSource.alpha(e._1))))
    normalizer1 * normalizer2 * ratio
  }
  def logpr(obsCounts:Vector): Double = Math.log(pr(obsCounts)) //indices.foldLeft(0.0)(_+logpr(_))
  override def prIndices(indices:Seq[Int]): Double = pr({val v = new SparseVector(length); indices.foreach(i => v(i) += 1.0); v})
  override def logprIndices(indices:Seq[Int]): Double = Math.log(prIndices(indices))
  override def _registerSample(o:O)(implicit d:DiffList) = { 
    super._registerSample(o) // Consider not calling super: Don't keep the outcomes in a HashMap; we have what we need in 'counts'  How much time would this save?  I tried; very little.
    postChange(o)
  }
  override def _unregisterSample(o:O)(implicit d:DiffList) = {
    super._unregisterSample(o) 
    preChange(o) // Note that if 'o' weren't a *Generated*DiscreteVariable & it changed its 'index', this will do the wrong thing! 
  }
  override def preChange(o:O)(implicit d:DiffList) = {
    o.generativeSource match {
      // TODO !!!!! This is not right.  The mixture.multinomial domain is not the domain of o !!!
      case mixture:MarginalizedMixtureChoice[SourceType,O,_] =>
        for (i <- 0 until o.domain.size) increment(i, -mixture.multinomial(i))
      case _ => increment(o.index, -1.0)
    }
  }
  override def postChange(o:O)(implicit d:DiffList) = {
    o.generativeSource match {
      // TODO !!!!! This is not right.  The mixture.multinomial domain is not the domain of o !!!
      case mixture:MarginalizedMixtureChoice[SourceType,O,_] =>
        for (i <- 0 until o.domain.size) increment(i, mixture.multinomial(i))
      case _ => increment(o.index, 1.0)
    }
  }
  // TODO Consider finding a way to put this back, in the case when O<:CategoricalValue
  //def sampleValue: O#VariableType#ValueType = outcomeDomain.get(sampleIndex)
  override def estimate: Unit = {} // Nothing to do because estimated on the fly
  class DiscretePr(override val index:Int, override val pr:Double, override val count:Double, val value:O#VariableType#ValueType) extends super.DiscretePr(index,pr,count)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i),outcomeDomain.get(i))})
  def topValues(n:Int) = top(n).toList.map(_.value)
  override def toString = "Multinomial(count="+countsTotal+")"
}

/** DiscreteValue integrated out over a Multinomial prior distribution.
    @author Andrew McCallum */
// TODO turn this into Variational representation supporting mean-field
trait MultinomialDiscrete[This<:MultinomialDiscrete[This]] extends GeneratedDiscreteVariable[This] {
  this: This =>
  final val multinomial = new DenseMultinomial[This](domainSize)
  def pr(i:Int): Double = multinomial.pr(i)
  override def sample(implicit d:DiffList): Unit = {
    super.sample(d)
    // TODO No!  The next commented line is wrong.  We want the distribution that includes the children.
    // This multinomial.set is already done in MarginalizedMixtureChoice, but it needs to be done for other cases.
    // Perhaps all GeneratedValue should have not only a 'sample' method but also a 'sampleDistribution' method
    //  for these purposes.
    // TODO Fix this!!!!
    //multinomial.set(generativeSource.asGenerativeDistribution.proportion) // TODO also create a Diff for this?
    // TODO This needs to be changed to account for children as well as the parent.
  }
  override def maximize(implicit d:DiffList): Unit = {
    super.maximize(d)
    multinomial.set(generativeSource.asGenerativeDistribution.proportion) // TODO also create a Diff for this?
  }
}

/** CategoricalValue integrated out over a Multinomial prior distribution.
    @author Andrew McCallum */
trait MultinomialCategorical[This<:MultinomialCategorical[This]] extends GeneratedCategoricalVariable[This] with MultinomialDiscrete[This] {
  this: This =>
}


// The binary special case, for convenience

/** The outcome of a coin flip, with boolean value.  */
class Flip extends CoordinatedBool with GeneratedDiscreteVariable[Flip]
// Note that no Variable should ever be a case class.
/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double, totalCount:Double) extends DenseCountsMultinomial[Flip](Array((1-p)*totalCount,p*totalCount)) {
  def this(p:Double) = this(p:Double, 1.0)
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  def flip : Flip = { val f = new Flip; f.setByIndex(this.sampleIndex)(null); f }
  def flip(n:Int) : Seq[Flip] = for (i <- 0 until n force) yield flip
  def pr(f:Boolean) : Double = if (f) pr(1) else pr(0)
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
