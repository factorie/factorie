/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.generative
import cc.factorie._

// Proportions is a Seq[Double] that sums to 1.0
// Discrete ~ DiscreteDistribution(Proportions)

trait ProportionsValue extends IndexedSeq[Double] {
  def sampleInt: Int = maths.nextDiscrete(this)
  def maxInt: Int = { 
    var i = 0; var maxi = 0; var maxd = 0.0
    while (i < length) { 
      if (this(i) > maxd) { maxd = this.apply(i); maxi = i }
      i += 1 
    } 
    i
  }
  def entropy: Double = maths.entropy(this)
  def klDivergence(p:ProportionsValue): Double = maths.klDivergence(this, p)
  def jsDivergence(p:ProportionsValue): Double = maths.jensenShannonDivergence(this, p)
}
class ProportionsArrayValue(value:Array[Double]) extends ProportionsValue {
  private val array = value
  final def apply(i:Int) = array(i)
  final def length = array.length
  final def update(i:Int, v:Double) = array(i) = v
}

trait Proportions extends GeneratedVar with Parameter with DiscreteGenerating with IndexedSeqEqualsEq[Double] with ProportionsValue
with VarAndValueGenericDomain[Proportions,ProportionsValue]
/*with VarAndValueGenericDomain[Proportions,Seq[Double]]*/ 
{
  //type ValueType = cc.factorie.generative.Proportions
  //def domain = GenericDomain
  // TODO We are allowing the variable to serve as its own value; odd and not necessarily recommended
  def value = this.asInstanceOf[Value]
  // TODO Is this what we want?  Not a immutable representation of the variable value, but we want this to be efficient.

  /*def apply(index:Int): Double
  def length: Int
  override def size = length
  def iterator = new Iterator[Double] {
    var i = -1
    def hasNext = i + 1 < length
    def next: Double = { i = i+1; apply(i) }
  }*/
  // TODO Remove this, now that we have IndexedSeqEqualsEq
  @deprecated("No longer necessary since Proportions is a IndexedSeq itself.")
  def asSeq: IndexedSeq[Double] = new IndexedSeq[Double] {
    def apply(i:Int) = Proportions.this.apply(i)
    def length = Proportions.this.length
  }
  override def sampleInt = maths.nextDiscrete(this)(cc.factorie.random) // TODO Avoid the inefficiency of asSeq
  def pr(index:Int) = apply(index)
  def logpr(index:Int) = math.log(apply(index))
  def maxPrIndex: Int = { var maxIndex = 0; var i = 1; while (i < length) { if (this(i) > this(maxIndex)) maxIndex =i; i += 1 }; maxIndex }
  override def toString = this.take(10).mkString(printName+"(", ",", ")")

  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)}).filter(_.pr > 0.0)
}

/** Proportions for which the indicies correspond to CategoricalValues.
    The abstract method 'categoricalDomain' must be supplied.  */
trait CategoricalProportions[A] extends Proportions {
  def categoricalDomain: CategoricalDomain[A]
  // TODO change "value:String" to "category:A"
  class DiscretePr(override val index:Int, override val pr:Double, val value:String) extends super.DiscretePr(index, pr)
  override def top(n:Int): Seq[DiscretePr] = {
    val entries = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n) //.toList
    entries.map({case (p,i)=>new DiscretePr(i, p, categoricalDomain.getCategory(i).toString)})
  }
  def topValues(n:Int) = top(n).toList.map(_.value)
  // TODO Make a topCategories method
}

trait MutableProportions extends Proportions /*with MutableGeneratedVar*/ /*with Estimation[MutableProportions]*/ {
  def set(p:ProportionsValue)(implicit d:DiffList): Unit
  //def defaultEstimator: Estimator[MutableProportions] = MutableProportionsEstimator
}

class DenseProportions(p:Seq[Double]) extends MutableProportions {
  //def this(ps:Double*) = this(ps)
  def this(dim:Int) = this(Seq.fill(dim)(1.0/dim))
  private var _p = new Array[Double](p.length)
  if (p != Nil) this := p else setUniform(null)
  @inline final def length: Int = _p.size
  @inline final def apply(index:Int) = _p(index)
  def set(p:ProportionsValue)(implicit d:DiffList): Unit = set(p.asInstanceOf[Seq[Double]])
  def set(p:Seq[Double])(implicit d:DiffList): Unit = {
    assert(p.size == _p.size, "size mismatch: new="+p.size+", orig="+_p.size)
    val newP = p.toArray // TODO Make a copy, just in case it was already an array?
    if (d ne null) d += ProportionsDiff(_p, newP)
    _p = newP
  }
  def :=(p:Seq[Double]) = set(p)(null)
  def setUniform(implicit d:DiffList): Unit = set(new UniformProportions(length))
  case class ProportionsDiff(oldP:Array[Double], newP:Array[Double]) extends Diff {
    def variable = DenseProportions.this
    def undo = _p = oldP
    def redo = _p = newP
  }
}

// TODO Change this to a Maximizer
object MutableProportionsEstimator {
  def estimate(p:MutableProportions, map:scala.collection.Map[Variable,Variable] = Map[Variable,Variable]()): Unit = {
    val e = new DenseCountsProportions(p.length)
    p.parentFactor match { case f:Dirichlet.Factor => e.set(f._2)(null); case null => {} }
    for (factor <- p.childFactors) factor match {
      case d:Discrete.Factor => e.increment(d._1.intValue, 1.0)(null)
      //case p:Proportions => forIndex(p.length)(i => e.increment(i, p(i))(null))
    }
    // TODO The above no longer works for weighted children!  
    // This will be a problem for EM.
    // Grep source for weightedGeneratedChildren to find all the places that may need fixing.
    /*for ((child, weight) <- d.weightedGeneratedChildren(map)) child match {
      case x:DiscreteVar => e.increment(x.intValue, weight)(null)
      case p:Proportions => forIndex(p.length)(i => e.increment(i, weight * p(i))(null))
    }*/
    p.set(e)(null)
  }
}


class DiracProportions(val length:Int, val peak:Int) extends Proportions {
  @inline final def apply(index:Int) = if (index == peak) 1.0 else 0.0
}

class UniformProportions(val length:Int) extends Proportions {
  @inline final def apply(index:Int) = 1.0 / length
}

class GrowableUniformProportions(val sizeProxy:Iterable[_]) extends Proportions {
  // I used to have GrowableUniformProportions(val sizeProxy:{def size:Int}), but this results in java.lang.reflect.Method.invoke at runtime
  def length = sizeProxy.size
  @inline final def apply(index:Int) = {
    val result = 1.0 / length
    assert(length > 0, "GrowableUniformProportions domain size is zero.")
    result
  }
}


class DenseCountsProportions(len:Int) extends MutableProportions {
  def this(p:Seq[Double]) = { this(p.length); this.set(p)(null) }
  protected var _counts = new Array[Double](len)
  protected var _countsTotal = 0.0
  def length = _counts.size
  def countsSeq: Seq[Double] = _counts.toSeq
  def counts(index:Int) = _counts(index)
  def countsTotal  = _countsTotal
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = { 
    _counts(index) += incr; _countsTotal += incr
    assert(_counts(index) >= 0, "counts("+index+")="+_counts(index)+" after incr="+incr)
    assert(_countsTotal >= 0, "countsTotal="+_countsTotal+" after incr="+incr)
    if (d ne null) d += DenseCountsProportionsDiff(index, incr)
  }
  def increment(incrs:Seq[Double])(implicit d:DiffList): Unit = {
    forIndex(incrs.length)(i => increment(i, incrs(i)))
  }
  def set(p:ProportionsValue)(implicit d:DiffList): Unit = { zero(); increment(p) }
  def set(p:Seq[Double])(implicit d:DiffList): Unit = { zero(); increment(p) }
  def apply(index:Int): Double = {
    if (_countsTotal == 0) 1.0 / length
    else _counts(index) / _countsTotal
  }
  def zero(): Unit = { java.util.Arrays.fill(_counts, 0.0); _countsTotal = 0.0 }
  def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
    case dcp:DenseCountsProportions if (dcp == this) => {}
  }
  def setCollapsed: Unit = {
    //parentFactor.family.resetCollapsedChild(parentFactor)
    this.zero()
    // TODO Check to make sure that both "updates" below return true indicating success
    val b1 = parentFactor.family.updateCollapsedChild(parentFactor)
    val b2 = childFactors.forall(f => f.family.updateCollapsedParents(f, 1.0))
    require(b1)
    require(b2)
    //for (factor <- childFactors) factor match { case f:Discrete.Factor => increment(f._1.intValue, 1.0)(null) }
  }
  //class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  //override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))}).filter(_.pr > 0.0)
  case class DenseCountsProportionsDiff(index:Int, incr:Double) extends Diff {
    def variable = DenseCountsProportions.this
    def undo = { _counts(index) -= incr; _countsTotal -= incr; assert(_counts(index) >= 0.0) }
    def redo = { _counts(index) += incr; _countsTotal += incr; assert(_counts(index) >= 0.0) }
  }
}

class GrowableDenseCountsProportions(val dimensionDomain:DiscreteDomain, initialCapacity:Int = 32) extends DenseCountsProportions(initialCapacity) {
  private var _size: Int = 0
  override def length: Int = math.max(_size, dimensionDomain.size) // new Exception().printStackTrace()
  override def counts(index:Int):Double = if (index < _counts.size) _counts(index) else 0.0
  protected def ensureCapacity(size:Int): Unit = if (_counts.size < size) {
    val newSize = math.max(_counts.size * 2, size)
    //println("GrowableDenseCountsProportions "+this.hashCode+" growing from "+_counts.size+" to capacity "+size+".  New size="+newSize)
    val newCounts = new Array[Double](newSize)
    Array.copy(_counts, 0, newCounts, 0, _counts.size)
    _counts = newCounts
    //println("GrowableDenseCountsProportions "+this.hashCode+" done growing.")
  }
  override def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    ensureCapacity(index+1)
    //if (index >= _size) { println("GrowableDenseCountsProportions.increment growing index="+index); _size = index + 1 }
    if (index >= _size) { _size = index + 1 }
    //if (index >= _size) { _size = 100000 }
    super.increment(index, incr)
    //super.increment(_size, incr)
  }
}


/*object DenseProportions {
  implicit val denseProportionsEstimator = new Estimator[DenseProportions] {
    def estimate(p:DenseProportions, model:Model): Unit = {
      val counts = new Array[Double](p.length)
      for (child <- p.children) child match { case child:DiscreteVar => counts(child.intValue) = counts(child.intValue) + 1.0 }
      for (i <- 0 until p.length) counts(i) /= p.children.size
      p.set(counts)(null) // TODO Should we have a DiffList here?
    }
  }
}*/


/*object DenseCountsProportions {
  implicit val denseCountsProportionsEstimator = new Estimator[DenseCountsProportions] {
    def estimate(p:DenseCountsProportions, model:Model): Unit = {
      p.zero
      for (child <- p.children) child match { case child:DiscreteVar => p.increment(child.intValue, 1.0)(null) }
    }
  }
}*/




//trait SparseVectorIncrementableCounts extends IncrementableCounts {
//  protected val _counts = new SparseVector(this.length) { def length = size }
//}



// Multinomial for which sampling is efficient because outcomes are considered in order of highest-count first.
//  This implementation is not yet finished.
/*@deprecated // Not finished
abstract class SortedSparseCountsProportions(dim:Int) extends CountsProportions {
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
}*/





