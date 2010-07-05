/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap,IndexedSeq}
//import cc.factorie.util.SeqAsVector
//import scalala.Scalala._
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

/*
/// Base of the Multinomial class hierarchy, abstract, needing only methods length, pr, and set. 
class Multinomial[O<:DiscreteValue] extends DiscreteDistribution[O] with GeneratedProportionValue[O] with IndexedSeq[Double] with CollapsibleDistribution[O] with QDistribution {
  type VariableType <: Multinomial[O]
  type SourceType <: ProportionDistribution[O]
  type CollapsedType = DirichletMultinomial[O]
  def newCollapsed = new DirichletMultinomial(this)
//  def newQ(implicit m:Manifest[O]) = new Dirichlet[O](1.0)(m)
  def length: Int
  def apply(index:Int) = pr(index)
  def set(cs:Seq[Double], normalize:Boolean = true): Unit // TODO include a DiffList here?
  def pr(index:Int): Double
  def localPr(index:Int) = pr(index) // Alternative that should not use any source.alpha in the estimate; used in Dirichlet.estimate
  final def pr(o:O): Double = pr(o.index)
  def proportion: Seq[Double] = this
  def prs: Seq[Double] = this //new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = count.size } // TODO Remove this method?
  def logpr(index:Int) = Math.log(pr(index))
  def logprIndices(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  def prIndices(indices:Seq[Int]) : Double = indices.foldLeft(1.0)(_*pr(_))
  def pr(outcomes:Seq[O]) : Double = prIndices(outcomes.map(_.index))
  def sampleCounts: { def apply(i:Int):Double; def length:Int } = {
    val c = new Array[Double](length)
    generatedSamples.foreach(s => c(s.index) += 1.0)
    c
  }
  //def prVector: Vector = new SeqAsVector { def apply(i:Int) = pr(i); def length = size } // TODO Bad idea?  Is Vector.elements supposed to have type Iterable[(Int,Double)]? 
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit = set(s.sampleProportions)
  //def sampleWith(s:SourceType)(implicit d:DiffList): Unit = set(s.sampleProportionsWithCounts(sampleCounts))

  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)}).filter(_.pr > 0.0)
  // TODO Put next method in superclass
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
  def sampleIndices(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples) yield sampleIndex
  def estimate: Unit = { // TODO Think about how this might make SparseCountsMultinomial unsparse, and how to improve
    if (generatedSamples.isEmpty) throw new Error("No generated samples from which to estimate")
    val cs = new Array[Double](length)
    s match {
      case d:Dirichlet[O] => for (i <- 0 until length) cs(i) = d.alpha(i)
      case _ => throw new Error("Do not know how to estimate Multinomial parameters with prior other than Dirichlet.")
    }
    weightedGeneratedSamples.foreach({case (o:O,w:Double) => cs(o.index) += w })
    Maths.normalize(cs)
    this.set(cs)
  }
}

// Compact representation of immutable Multinomial with equal probability for all outcomes. 
class UniformMultinomial[O<:DiscreteValue](val length:Int) extends Multinomial[O] {
  type VariableType <: UniformMultinomial[O]
  def this()(implicit m:Manifest[O]) = this(Domain[O](m).size)
  private val pr1 = 1.0/length
  final def pr(index:Int) = pr1
  def set(cs:Seq[Double], normalize:Boolean = true): Unit = throw new Error("UniformMultinomial cannot be changed.")
}

// A mixture of a fixed set of Multinomials, i.e. you cannot add or remove Multinomials from the mixture. 
class MultinomialMixture[M<:Multinomial[O]:ClassManifest,O<:DiscreteValue](ms:Seq[M], ps:Seq[Double]) extends Multinomial[O] {
  type VariableType <: MultinomialMixture[M,O]
  // TODO How can I avoid needing both M and O as type parameters.  I think M should automatically specify O.
  //type SourceType = ProportionDistribution[O];
  val components = ms.toArray
  val length: Int = components.first.length
  val proportions = new DenseCountsMultinomial(ps)
  def pr(index:Int) = {
    var p = 0.0
    for (i <- 0 until proportions.length) p += proportions.pr(i) * components(i).pr(index)
    p
  }
  def set(cs:Seq[Double], normalize:Boolean = true): Unit = throw new Error("MultinomialMixture cannot be changed.")
}

// Simple Multinomial that stores p(x) directly in a Scalala DenseVector. 
class DenseMultinomial[O<:DiscreteValue](proportions:Seq[Double]) extends Multinomial[O] {
  //type SourceType = ProportionDistribution[O];
  def this(dim:Int) = this(Array.make(dim, 1.0/dim))
  def this()(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).size, 1.0/Domain[O](m).size))
  val length: Int = proportions.length
  private val _pr = new DenseVector(length)
  @inline final def pr(index:Int) = _pr(index)
  set(proportions)
  def set(proportions:Seq[Double], normalize:Boolean = true): Unit = {
    assert (proportions.length == length)
    if (normalize) {
      val sum = proportions.reduceLeft(_+_)
      assert (sum > 0.0)
      for (i <- 0 until length) _pr(i) = proportions(i)/sum
    } else for (i <- 0 until length) _pr(i) = proportions(i)
  }
  def setDirac(index:Int): Unit = {
    _pr.zero
    _pr(index) = 1.0
  }
}

trait SparseMultinomial[O<:DiscreteValue] extends Multinomial[O] {
  def activeIndices: scala.collection.Set[Int];
  //def sampleIndexProduct(m2:SparseMultinomial[O]): Int // TODO
}




// Maintains an array of non-negative Double-valued counts which can be incremented.
//  Useful for Multinomials and Dirichlets.  The counts themselves are stored in '_counts',
//  which is abstract.  The method 'length' is also abstract. 
trait IncrementableCounts extends Variable {
  protected val _counts: { def apply(i:Int):Double; def update(i:Int, x:Double):Unit; def length:Int }
  def length: Int
  protected var _countsTotal: Double = 0.0
  def counts: { def apply(i:Int):Double; def update(i:Int, x:Double):Unit; def length:Int } = _counts
  def countsTotal = _countsTotal
  def count(index:Int): Double = _counts(index)
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    _counts(index) += incr; _countsTotal += incr
    if (d ne null) d += IncrementableCountsDiff(index, incr)
  }
  def increment(cs: { def apply(i:Int):Double; def length:Int })(implicit d:DiffList): Unit = {
    for (i <- 0 until cs.length) { _counts(i) += cs(i); _countsTotal += cs(i) }
    if (d ne null) d += IncrementableCountsSeqDiff(cs)
  }
  def zero(implicit d:DiffList): Unit = 
    for (i <- 0 until length) if (_counts(i) > 0.0) increment(i, -_counts(i))
  def set(cs:Seq[Double], normalize:Boolean = true): Unit = {
    zero(null); increment(cs)(null)
  }
  case class IncrementableCountsDiff(index:Int, incr:Double) extends Diff {
    def variable = IncrementableCounts.this
    def undo = { _counts(index) -= incr; _countsTotal -= incr; assert(_counts(index) >= 0.0); assert(_countsTotal >= 0.0) }
    def redo = { _counts(index) += incr; _countsTotal += incr }
  }
  case class IncrementableCountsSeqDiff(cs: { def apply(i:Int):Double; def length:Int }) extends Diff {
    def variable = IncrementableCounts.this
    def undo = { for (i <- 0 until cs.length) { _counts(i) -= cs(i); _countsTotal -= cs(i) } }
    def redo = { for (i <- 0 until cs.length) { _counts(i) += cs(i); _countsTotal += cs(i) } }
  }
}

trait ArrayIncrementableCounts extends IncrementableCounts {
  protected val _counts = new Array[Double](this.length)
}

trait DenseVectorIncrementableCounts extends IncrementableCounts {
  protected val _counts = new DenseVector(this.length) { def length = size }
}

trait SparseVectorIncrementableCounts extends IncrementableCounts {
  protected val _counts = new SparseVector(this.length) { def length = size }
}

trait HashIncrementableCounts extends IncrementableCounts {
  protected val _counts = new {
    private val h = new HashMap[Int,Double] // new it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap
    def length: Int = h.size
    def apply(key:Int): Double = h(key)
    def update(key:Int, value:Double): Unit = h.put(key, value)
  }
}



// A Multinomial that stores its parameters as a collection of "outcome counts" and their total. 
//  Only the methods '_counts' and 'length' are abstract in this class.
trait CountsMultinomial[O<:DiscreteValue] extends Multinomial[O] with IncrementableCounts {
  type VariableType <: CountsMultinomial[O]
  def pr(index:Int) = counts(index) / countsTotal
  class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))}).filter(_.pr > 0.0)
}

// A Multinomial that stores its counts in a Scalala SparseVector. 
class SparseCountsMultinomial[O<:DiscreteValue](val length:Int) extends SparseMultinomial[O] with CountsMultinomial[O] with SparseVectorIncrementableCounts {
  type Variabletype <: SparseCountsMultinomial[O]
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  def this()(implicit m:Manifest[O]) = this(Domain[O](m).size)
  type VariableType <: SparseCountsMultinomial[O]
  def default = _counts.default
  def default_=(d:Double) = {
    _countsTotal -= _counts.default
    _countsTotal += d
    _counts.default = d
  }
  def activeIndices: scala.collection.Set[Int] = _counts.activeDomain
  // def sampleIndex: Int // TODO
}

// Multinomial for which sampling is efficient because outcomes are considered in order of highest-count first.
//  This implementation is not yet finished.
@deprecated // Not finished
abstract class SortedSparseCountsMultinomial[O<:DiscreteValue](dim:Int) extends Multinomial[O] with SparseMultinomial[O] {
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

// A Multinomial that stores its counts in a Scalala DenseVector.
class DenseCountsMultinomial[O<:DiscreteValue](val length:Int) extends CountsMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: DenseCountsMultinomial[O]
  protected val _counts = new DenseVector(length) { def length = size }
  def zero = _counts.zero
}






//  A Multinomial with parameters integrated out with a Dirichlet prior.  Also known as a Multivariate Polya distribution.
//     @author Andrew McCallum 
// // TODO Figure out how to use intead [O<:GeneratedDiscreteValue[O]], but still get O#VariableType#ValueType in "top" below
// // TODO class DirichletMultinomial[O<:DiscreteValue](val dirichlet:Dirichlet[O]) extends DiscreteDistribution[O] with MarginalizingDistribution
// // TODO trait MarginalizingDistribution { val target: Variable }
// class DirichletMultinomial[O<:GeneratedDiscreteValue[O]](dirichlet:AbstractDirichlet[O])(implicit m:Manifest[O]) extends DenseCountsMultinomial[O](Domain[O](m).size) {
//   def this()(implicit m:Manifest[O]) = this(null.asInstanceOf[AbstractDirichlet[O]])(m)
//   def this(dirichlet:AbstractDirichlet[O], initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(dirichlet)(m); set(initCounts) }
//   def this(initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(null.asInstanceOf[AbstractDirichlet[O]])(m); set(initCounts) }
//   type VariableType <: DirichletMultinomial[O];
//   //override type SourceType = DistributionLike[AbstractDirichlet[O],ProportionOutcome[O]]
//   //val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance; but otherwise 'm' would be stored for each instance anyway?
//   def actualGenerativeSource: Dirichlet[O] = null //generativeSource.value
//   override def pr(index:Int) : Double = {
//     //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
//     if (generativeSource != null)
//       (counts(index) + actualGenerativeSource.alpha(index)) / (countsTotal + actualGenerativeSource.alphaSum) // I considered +1 to both numerator and denominator in order to encourge unused mixture components to get data, but what objective does this correspond to?
//     else if (countsTotal == 0) // Use super.pr here instead?  But this may be faster
//       1.0 / size
//     else
//       counts(index) / countsTotal
//   }
//   override def localPr(index:Int): Double = super.pr(index)
//   override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
//     val s = Global.random.nextDouble * (countsTotal + actualGenerativeSource.alphaSum); var sum = 0.0; var i = 0; val size = this.size
//     while (i < size) {
//       sum += counts(i) + actualGenerativeSource.alpha(i)
//       if (sum >= s) return i
//       i += 1
//     }
//     return size - 1
//   }
//    Probability of a collection of counts; see http://en.wikipedia.org/wiki/Multivariate_Polya_distribution. 
//   // TODO Not tested!
//   def pr(ocounts:Vector) : Double = {
//     import Maths.{gamma => g}
//     assert (ocounts.size == length)
//     val n = norm(ocounts,1)
//     val normalizer1 = g(n) / ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2))
//     val normalizer2 = g(actualGenerativeSource.alphaSum) / g(n+actualGenerativeSource.alphaSum)
//     val ratio = ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2+actualGenerativeSource.alpha(e._1)/g(actualGenerativeSource.alpha(e._1))))
//     normalizer1 * normalizer2 * ratio
//   }
//   def logpr(obsCounts:Vector): Double = Math.log(pr(obsCounts)) //indices.foldLeft(0.0)(_+logpr(_))
//   override def prIndices(indices:Seq[Int]): Double = pr({val v = new SparseVector(length); indices.foreach(i => v(i) += 1.0); v})
//   override def logprIndices(indices:Seq[Int]): Double = Math.log(prIndices(indices))
//   override def _registerSample(o:O)(implicit d:DiffList) = { 
//     super._registerSample(o) // Consider not calling super: Don't keep the outcomes in a HashMap; we have what we need in 'counts'  How much time would this save?  I tried; very little.
//     postChange(o)
//   }
//   override def _unregisterSample(o:O)(implicit d:DiffList) = {
//     super._unregisterSample(o) 
//     preChange(o) // Note that if 'o' weren't a *Generated*DiscreteVariable & it changed its 'index', this will do the wrong thing! 
//   }
//   override def preChange(o:O)(implicit d:DiffList) = {
//   	increment(o.index, -1.0)
//     //o.generativeSource match {
//     //  // TODO !!!!! This is not right.  The mixture.multinomial domain is not the domain of o !!!
//     //  case mixture:MarginalizedMixtureChoice[_] =>
//     //    for (i <- 0 until o.domain.size) increment(i, -mixture.multinomial(i))
//     //  case _ => increment(o.index, -1.0)
//     //}
//   }
//   override def postChange(o:O)(implicit d:DiffList) = {
//   	increment(o.index, 1.0)
//     //o.generativeSource match {
//     //  // TODO !!!!! This is not right.  The mixture.multinomial domain is not the domain of o !!!
//     //  case mixture:MarginalizedMixtureChoice[_] =>
//     //    for (i <- 0 until o.domain.size) increment(i, mixture.multinomial(i))
//     //  case _ => increment(o.index, 1.0)
//     //}
//   }
//   // TODO Consider finding a way to put this back, in the case when O<:CategoricalValue
//   //def sampleValue: O#VariableType#ValueType = outcomeDomain.get(sampleIndex)
//   override def estimate: Unit = {} // Nothing to do because estimated on the fly
//   class DiscretePr(override val index:Int, override val pr:Double, override val count:Double, val value:String) extends super.DiscretePr(index,pr,count)
//   def top(n:Int)(implicit m:Manifest[O]): Seq[DiscretePr] = {
//     val entries = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList
//     Domain.get[O](m.erasure) match {
//       case d:CategoricalDomain[_] => entries.map({case (p,i)=>new DiscretePr(i,p,counts(i),d.get(i).toString)})
//       case d:Any => entries.map({case (p,i)=>new DiscretePr(i,p,counts(i),"")})
//     }
//   }
//   def topValues(n:Int)(implicit m:Manifest[O]) = top(n).toList.map(_.value)
//   override def toString = "DirichletMultinomial="+countsTotal+")"
// }

// DiscreteValue integrated out over a Multinomial prior distribution.
//     @author Andrew McCallum 
// // TODO turn this into Variational representation supporting mean-field
// trait MultinomialDiscrete[This<:MultinomialDiscrete[This]] extends DiscreteVariable with GeneratedDiscreteVariable[This] {
//   this: This =>
//   final val multinomial = new DenseMultinomial[This](domainSize)
//   def pr(i:Int): Double = multinomial.pr(i)
//   override def sample(implicit d:DiffList): Unit = {
//     super.sample(d)
//     // TODO No!  The next commented line is wrong.  We want the distribution that includes the children.
//     // This multinomial.set is already done in MarginalizedMixtureChoice, but it needs to be done for other cases.
//     // Perhaps all GeneratedValue should have not only a 'sample' method but also a 'sampleDistribution' method
//     //  for these purposes.
//     // TODO Fix this!!!!
//     //multinomial.set(generativeSource.asDistribution.proportion) // TODO also create a Diff for this?
//     // TODO This needs to be changed to account for children as well as the parent.
//   }
//   override def maximize(implicit d:DiffList): Unit = {
//     super.maximize(d)
//     multinomial.set(generativeSource.value.proportion) // TODO also create a Diff for this?
//   }
// }

// CategoricalValue integrated out over a Multinomial prior distribution.
//trait MultinomialCategorical[T,This<:MultinomialCategorical[T,This]] extends GeneratedCategoricalVariable[T,This] with MultinomialDiscrete[This] {
//  this: This =>
//}


*/


// The binary special case, for convenience

/** The outcome of a coin flip, with boolean value.  */
class Flip(coin:Coin, value:Boolean = false) extends BooleanVariable(value) with GeneratedDiscreteVariable {
  def proportions = coin
  coin.addChild(this)(null)
}
/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double) extends DenseProportions(Seq(1.0-p, p)) {
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  def flip: Flip = { val f = new Flip(this); f.setByIndex(this.sampleInt)(null); f }
  def flip(n:Int) : Seq[Flip] = for (i <- 0 until n) yield flip
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
