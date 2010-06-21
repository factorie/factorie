/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

// Proportions is a Seq[Double] that sums to 1.0
// Discrete ~ Multinomial(Proportions)

trait Proportions extends Parameter with IndexedSeq[Double] with DiscreteGenerating {
  def sampleInt = Maths.nextDiscrete(this)(Global.random)
  def pr(index:Int) = apply(index)
  def logpr(index:Int) = math.log(apply(index))
  def maxPrIndex: Int = { var maxIndex = 0; var i = 1; while (i < size) { if (this(i) > this(maxIndex)) maxIndex =i; i += 1 }; maxIndex }
  override def toString = mkString(printName+"(", ",", ")")

  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)}).filter(_.pr > 0.0)
}

// TODO Unused.  Remove this?
trait TypedProportions[A<:DiscreteValue] extends Proportions {
  class DiscretePr(override val index:Int, override val pr:Double, val value:String) extends super.DiscretePr(index, pr)
  def top(n:Int)(implicit m:Manifest[A]): Seq[DiscretePr] = {
    val entries = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList
    Domain.get[A](m.erasure) match {
      case d:CategoricalDomain[_] => entries.map({case (p,i)=>new DiscretePr(i, p, d.get(i).toString)})
      case d:Any => entries.map({case (p,i)=>new DiscretePr(i, p, "")})
    }
  }
  def topValues(n:Int)(implicit m:Manifest[A]) = top(n).toList.map(_.value)
}

class DenseProportions(p:Seq[Double]) extends Proportions with Estimation[DenseProportions] {
  //def this(ps:Double*) = this(ps)
  private var _p = new Array[Double](p.size)
  if (p != Nil) this := p else setUniform(null)
  @inline final def length = _p.size
  @inline final def apply(index:Int) = _p(index)
  def set(p:Seq[Double])(implicit d:DiffList): Unit = {
    assert(p.size == _p.size)
    val newP = p.toArray
    if (d ne null) d += ProportionsDiff(_p, newP)
    _p = newP
  }
  def :=(p:Seq[Double]) = set(p)(null)
  def setUniform(implicit d:DiffList): Unit = set(new UniformProportions(size))
  case class ProportionsDiff(oldP:Array[Double], newP:Array[Double]) extends Diff {
    def variable = DenseProportions.this
    def undo = _p = oldP
    def redo = _p = newP
  }
}

class UniformProportions(val length:Int) extends Proportions {
  @inline final def apply(index:Int) = 1.0 / length
}

class GrowableUniformProportions(val sizeProxy:{def size:Int}) extends Proportions {
  def length = sizeProxy.size
  @inline final def apply(index:Int) = 1.0 / length
}

class DenseCountsProportions(len:Int) extends Proportions {
  protected var _counts = new Array[Double](len)
  protected var _countsTotal = 0.0
  def length = _counts.size
  def counts(index:Int) = _counts(index)
  def countsTotal  = _countsTotal
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = { 
    _counts(index) += incr; _countsTotal += incr
    if (d ne null) d += DenseCountsProportionsDiff(index, incr)
  }
  def apply(index:Int): Double = {
    if (_countsTotal == 0) 1.0 / length
    else _counts(index) / _countsTotal
  }
  //class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  //override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))}).filter(_.pr > 0.0)
  case class DenseCountsProportionsDiff(index:Int, incr:Double) extends Diff {
    def variable = DenseCountsProportions.this
    def undo = { _counts(index) -= incr; _countsTotal -= incr; assert(_counts(index) >= 0.0) }
    def redo = { _counts(index) += incr; _countsTotal += incr; assert(_counts(index) >= 0.0) }
  }
}

class GrowableDenseCountsProportions extends DenseCountsProportions(32) {
  private var _size = 0
  override def length = _size
  override def counts(index:Int):Double = if (index < _counts.size) _counts(index) else 0.0
  protected def ensureCapacity(size:Int): Unit = if (_counts.size < size) {
    val newSize = math.max(_counts.size * 2, size)
    val newCounts = new Array[Double](newSize)
    Array.copy(_counts, 0, newCounts, 0, _counts.size)
    _counts = newCounts
  }
  override def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    ensureCapacity(index+1)
    if (index >= _size) _size = index + 1
    super.increment(index, incr)
  }
}


object DenseProportionsMaximumLikelihoodEstimator {
  implicit val multinomialMaximumLikelihoodEstimator = new Estimator[DenseProportions] {
    def estimate(p:DenseProportions, model:Model): Unit = {
      val counts = new Array[Double](p.length)
      for (child <- p.children) child match { case child:DiscreteValue => counts(child.intValue) = counts(child.intValue) + 1.0 }
      for (i <- 0 until p.length) counts(i) /= p.children.size
      p.set(counts)(null) // TODO Should we have a DiffList here?
    }
  }
}


