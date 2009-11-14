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


/** Base of the Multinomial class hierarchy, needing only methods length, pr, and set. */
// TODO Rename simply "Multinomial"?
trait AbstractMultinomial[O<:DiscreteOutcome[O]] extends /*GenerativeVariable[ProportionOutcome[O]] with*/ DiscreteGenerating[O] with ProportionOutcome[O] {
  type VariableType <: AbstractMultinomial[O];
  //type OutcomeType = O
  //type SourceType = ProportionGenerating[O];
  type SourceType = AbstractDirichlet[O]; // TODO Why can't I make this ProportionGenerating[O] instead of AbstractDirichlet[O]?
  class DomainInSubclasses
  def asOutcome = this
  //def this(initCounts:Seq[Double]) = { this(initCounts.size); setCounts(initCounts) }
  def length: Int
  def apply(index:Int) = pr(index)
  def set(proportions:Seq[Double]): Unit // TODO include a DiffList here?
  def pr(index:Int) : Double
  def localPr(index:Int) = pr(index) // Alternative that should not use any source.alpha in the estimate; used in Dirichlet.estimate
  final def pr(o:OutcomeType) : Double = pr(o.index)
  def prs: RandomAccessSeq[Double] = this //new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = count.size } // TODO Remove this method?
  def logpr(index:Int) = Math.log(pr(index))
  def logprIndices(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  def prIndices(indices:Seq[Int]) : Double = indices.foldLeft(1.0)(_*pr(_))
  def pr(outcomes:Seq[OutcomeType]) : Double = prIndices(outcomes.map(_.index))
  def counts: { def apply(i:Int):Double; def size:Int } = {
    val c = new Array[Double](length)
    generatedSamples.foreach(s => c(s.index) += 1.0)
    c
  }
  def prVector: Vector = new SeqAsVector { def apply(i:Int) = pr(i); def length = size } // TODO Bad idea?  Is Vector.elements supposed to have type Iterable[(Int,Double)]? 
  override def sampleFrom(s:SourceType)(implicit d:DiffList): Unit = set(s.sampleProportionsWithCounts(counts))
  @deprecated def sampleInto(o:OutcomeType): Unit = o match { // TODO Consider including implicit DiffList here? 
    case v:DiscreteVariable => v.setByIndex(sampleIndex)(null)
    case _ => throw new Error("Trying to sample into immutable Variable") // TODO Is this OK to just do nothing here?;
    // TODO if so, then we really don't need the separation between GenerativeObservation and GenerativeVariable!...  Or perhaps not?
  }
  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)})
  // TODO Put next method in superclass
  def sample(implicit d:DiffList): Unit = { if (source != null) this.sampleFrom(source) else throw new Error("Source not set") } 
  def sampleIndex: Int = {
    val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += pr(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  def sampleIndices(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples force) yield sampleIndex
  def estimate: Unit = { throw new Error("Not yet implemented")} // TODO What to put here?
}

/** Compact representation of immutable Multinomial with equal probability for all outcomes. */
class UniformMultinomial[O<:DiscreteOutcome[O]](implicit m:Manifest[O]) extends AbstractMultinomial[O] {
  val length: Int = Domain[O](m).size
  val pr1 = 1.0/length
  final def pr(index:Int) = pr1
  def set(proportions:Seq[Double]): Unit = throw new Error("UniformMultinomial cannot be changed.")
}

/** A mixture of a fixed set of Multinomials, i.e. you cannot add or remove Multinomials from the mixture. */
class MultinomialMixture[M<:AbstractMultinomial[O],O<:DiscreteOutcome[O]](ms:Seq[M], ps:Seq[Double]) extends AbstractMultinomial[O] {
	// TODO How can I avoid needing both M and O as type parameters.  I think M should automatically specify O.
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

/** Simple Multinomial that represents p(x) directly. */
class DenseMultinomial[O<:DiscreteOutcome[O]](proportions:Seq[Double])(implicit m:Manifest[O]) extends AbstractMultinomial[O] {
  val length: Int = Domain[O](m).size
  val _pr = new DenseVector(length)
  if (proportions != null) set(proportions)
  /** Will normalize for you, if not already normalized. */
  def set(props:Seq[Double]): Unit = {
    assert (proportions.length == length)
    val sum = proportions.foldLeft(0.0)(_+_)
    assert (sum > 0.0)
    for (i <- 0 until length) _pr(i) = proportions(i)/sum
  }
  @inline final def pr(index:Int) = _pr(index)
}

/** A Multinomial that stores its parameters as a collection of "outcome counts" and their total. */
trait CountsMultinomial[O<:DiscreteOutcome[O]] extends AbstractMultinomial[O] {
  type VariableType <: CountsMultinomial[O];
  class DomainInSubclasses
  override type SourceType = AbstractDirichlet[O]
  def length: Int = counts.size 
  private var total : Double = 0.0
  def countsTotal = total
  protected val _counts : Vector
  override def prVector: Vector = _counts / total
  override def counts: { def apply(i:Int):Double; def update(i:Int,v:Double):Unit; def size:Int } = _counts // TODO Want Seq[Double], but Vector doesn't mixin Seq?!!
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
  def set(c:Seq[Double]): Unit = { // TODO Add DiffList here?
    assert(c.length == _counts.size)
    total = 0.0
    var i = 0; c.foreach(x => {_counts(i) = x; total += x; i += 1}) 
  }
  // Raw operations on count vector, without Diffs
  def zero: Unit = { _counts.zero; total = 0.0 }
  def increment(m:AbstractMultinomial[O], rate:Double): Unit = { total += norm(m,1); for (i <- 0 until size) _counts += m.prVector }  
  def pr(index:Int) : Double = if (countsTotal == 0) 1.0 / size else counts(index) / countsTotal
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * countsTotal; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += counts(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  override def estimate: Unit = { // TODO Think about how this might make SparseCountsMultinomial unsparse, and how to improve
    if (generatedSamples.isEmpty) throw new Error("No generated samples from which to estimate")
    if (source == null) { _counts.zero; total = 0.0 }
    else { _counts := source.alphaVector; total = source.alphaSum }
    generatedSamples.foreach(s => { _counts(s.index) += 1.0; total += 1.0 })
  }
  class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))})
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

trait SparseMultinomial[O<:DiscreteOutcome[O]] extends AbstractMultinomial[O] {
  def activeIndices: scala.collection.Set[Int];
  //def sampleIndexProduct(m2:SparseMultinomial[O]): Int // TODO
}

class SparseCountsMultinomial[O<:DiscreteOutcome[O]](dim:Int) extends CountsMultinomial[O] with SparseMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: SparseCountsMultinomial[O]
  class DomainInSubclasses
  protected val _counts = new SparseVector(dim)
  def default = _counts.default
  def default_=(d:Double) = _counts.default = d
  def activeIndices: scala.collection.Set[Int] = _counts.activeDomain
  // def sampleIndex: Int // TODO
}

@deprecated // Not finished
abstract class SortedSparseCountsMultinomial[O<:DiscreteOutcome[O]](dim:Int) extends AbstractMultinomial[O] with SparseMultinomial[O] {
	def length: Int = _count.size 
  protected var total : Double = 0.0
  protected val _count = new Array[Int](dim)
}


class DenseCountsMultinomial[O<:DiscreteOutcome[O]](dim:Int) extends CountsMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: DenseCountsMultinomial[O]
  class DomainInSubclasses
  protected val _counts = new DenseVector(dim)
}

// TODO Figure out how to use intead [O<:DiscreteOutcome[O]], but still get O#VariableType#ValueType in "top" below
class DirichletMultinomial[O<:CategoricalOutcome[O]](dirichlet:AbstractDirichlet[O])(implicit m:Manifest[O]) extends DenseCountsMultinomial[O](Domain[O](m).size) {
  def this()(implicit m:Manifest[O]) = this(null.asInstanceOf[AbstractDirichlet[O]])(m)
  def this(dirichlet:AbstractDirichlet[O], initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(dirichlet)(m); set(initCounts) }
  def this(initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(null.asInstanceOf[AbstractDirichlet[O]])(m); set(initCounts) }
  type VariableType <: DirichletMultinomial[O];
  class DomainInSubclasses
  override type SourceType = AbstractDirichlet[O]
  val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance; but otherwise 'm' would be stored for each instance anyway?
  override def pr(index:Int) : Double = {
    //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
    if (source != null)
      (counts(index) + source.alpha(index)) / (countsTotal + source.alphaSum) // I considered +1 to both numerator and denominator in order to encourge unused mixture components to get data, but what objective does this correspond to?
    else if (countsTotal == 0) // Use super.pr here instead?  But this may be faster
      1.0 / size
    else
      counts(index) / countsTotal
  }
  override def localPr(index:Int): Double = super.pr(index)
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * (countsTotal + source.alphaSum); var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += counts(i) + source.alpha(i)
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
    val normalizer2 = g(source.alphaSum) / g(n+source.alphaSum)
    val ratio = ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2+source.alpha(e._1)/g(source.alpha(e._1))))
    normalizer1 * normalizer2 * ratio
  }
  def logpr(obsCounts:Vector): Double = Math.log(pr(obsCounts)) //indices.foldLeft(0.0)(_+logpr(_))
  override def prIndices(indices:Seq[Int]): Double = pr({val v = new SparseVector(length); indices.foreach(i => v(i) += 1.0); v})
  override def logprIndices(indices:Seq[Int]): Double = Math.log(prIndices(indices))
  override def _registerSample(o:O)(implicit d:DiffList) = { 
    super._registerSample(o) // Consider not calling super: Don't keep the outcomes in a HashMap; we have what we need in 'counts'  How much time would this save?  I tried; very little.
    increment(o.index, 1.0)
  }
  override def _unregisterSample(o:O)(implicit d:DiffList) = {
    super._unregisterSample(o) 
    increment(o.index, -1.0) // Note that if 'o' weren't a DiscreteOutcome & it changed its 'index', this will do the wrong thing! 
  }
  override def preChange(o:O)(implicit d:DiffList) = {
    increment(o.index, -1.0)
  }
  override def postChange(o:O)(implicit d:DiffList) = {
    increment(o.index, 1.0)
  }
  // TODO Consider finding a way to put this back, in the case when O<:CategoricalValue
  //def sampleValue: O#VariableType#ValueType = outcomeDomain.get(sampleIndex)
  override def estimate: Unit = {} // Nothing to do because estimated on the fly
	class DiscretePr(override val index:Int, override val pr:Double, override val count:Double, val value:O#VariableType#ValueType) extends super.DiscretePr(index,pr,count)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i),outcomeDomain.get(i))})
  def topValues(n:Int) = top(n).toList.map(_.value)
  override def toString = "Multinomial(count="+countsTotal+")"
}


trait MultinomialDiscrete extends DiscreteValue // TODO turn this into Variational representation of SingleIndex with a distribution


// The binary special case, for convenience

/** The outcome of a coin flip, with boolean value.  */
class Flip extends CoordinatedBool with DiscreteOutcomeVariable[Flip]
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
