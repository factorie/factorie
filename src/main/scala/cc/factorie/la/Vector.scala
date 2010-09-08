/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.la

/** A vector, for holding a sequence of Doubles and performing various linear algebra operations.
    @author Andrew McCallum */
trait Vector extends scala.collection.mutable.IndexedSeq[Double] {
  def domainSize: Int
  def activeDomain: Iterable[Int]
  def dot(v:Vector): Double
  def activeElements: Iterator[(Int,Double)]
  def update(index:Int, value:Double): Unit = throw new Error("Method update not defined on class "+getClass.getName)
  def +=(v:Vector): Unit = throw new Error("Method +=(Vector) not defined on class "+getClass.getName)
  def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  def *(scalar:Double) = new VectorTimesScalar(this, scalar)
}

/** A lazy product of a Vector and a scalar.
    @author Andrew McCallum */
class VectorTimesScalar(val vector:Vector, val scalar:Double) extends Vector {
  def length = vector.length
  def domainSize: Int = vector.domainSize
  def activeDomain: Iterable[Int] = vector.activeDomain
  def dot(v:Vector): Double = vector.dot(v) * scalar
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val iter = vector.activeElements
    def hasNext = iter.hasNext
    def next = { val n = iter.next; (n._1, n._2 * scalar) }
  }
  def apply(index:Int) = vector.apply(index) * scalar
}

/** A Vector that has all zeros, except one position containing a 1.0.
    @author Andrew McCallum */
class SingletonBinaryVector(val length:Int, val singleIndex:Int) extends Vector {
  def domainSize = 1
  def activeDomain: Iterable[Int] = Seq(singleIndex)
  def apply(index:Int): Double = if (index == singleIndex) 1.0 else 0.0
  def dot(v:Vector) = v(singleIndex)
  def activeElements = Iterator.single((singleIndex, 1.0))
}

/** A Vector that has all zeros, except one position containing some arbitrary Double 'value'.
    @author Andrew McCallum */
class SingletonVector(val length:Int, val singleIndex:Int, val value:Double) extends Vector {
  var default = 0.0
  def domainSize = 1
  def activeDomain: Iterable[Int] = Seq(singleIndex)
  def apply(index:Int): Double = if (index == singleIndex) value else default
  def dot(v:Vector) = v(singleIndex) * value
  def activeElements = Iterator.single((singleIndex, value))
}

/** A Vector that may contain mostly zeros, with a few 1.0's, represented compactly in memory.
    @author Andrew McCallum */
class SparseBinaryVector(val length:Int, indices:Array[Int]) extends Vector {
  private val ind = new Array[Int](indices.size)
  Array.copy(indices, 0, ind, 0, ind.size)
  scala.util.Sorting.quickSort(ind)
  def domainSize = ind.size
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] { def apply(i:Int) = ind(i); def length = ind.size }
  private def _contains(index:Int, start:Int, end:Int): Boolean = {
    val diff = end - start
    // // println("SparseBinaryVector._contains "+index+" "+start+" "+end+" diff="+diff)
    if (diff < 2)
      return if (ind(start) == index) true else false
    val middle = start + (diff / 2)
    val midindex = ind(middle)
    if (midindex == index) return true
    else if (index < midindex) _contains(index, start, middle)
    else _contains(index, middle+1, end)
  }
  //private var lastPosition = 0 // TODO Implement this speed-up, also used in scalala
  def apply(index:Int): Double = if (_contains(index, 0, ind.size)) 1.0 else 0.0
  def activeElements = new Iterator[(Int,Double)] {
    var i = -1
    def hasNext = i < ind.size - 1
    def next = { i += 1; (ind(i), 1.0) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => {
      var i = 0; var result = 0.0
      while (i < ind.size) { result += v(ind(i)); i += 1 }; result
    }
    case v:SparseHashVector => {
      var i = 0; var result = 0.0
      while (i < ind.size) { result += v(ind(i)); i += 1 }; result
    }
    case v:SingletonBinaryVector => v dot this
    case v:SingletonVector => v dot this
    case v:VectorTimesScalar => v dot this
    case v:SparseBinaryVector => throw new Error("Not yet implemented.")
    case _ => throw new Error("SparseBinaryVector.dot does not handle "+v.getClass.getName)
  }
}

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory.
    @author Andrew McCallum */
class SparseVector(size:Int) extends SparseHashVector(size) {
  //private var used = 0
  //private var capacity = 8
  //private val indices = new Array[Int](capacity)
  //private val values = new Array[Double](capacity)
  //private def ensureCapacity(c:Int): Unit = {}
}

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory,
    implemented as a HashMap from Int indices to Double values.
    @author Andrew McCallum */
class SparseHashVector(val length:Int) extends Vector {
  var default = 0.0
  private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(index:Int) = SparseHashVector.this.default }
  def apply(index:Int) = h(index)
  def activeElements = h.iterator
  def domainSize = h.size
  def activeDomain: Iterable[Int] = h.keys
  override def update(index:Int, value:Double) = h(index) = value
  def dot(v:Vector): Double = v match {
    case dv:DenseVector => {
      var result = 0.0
      h.iterator.foreach({case(index,value) => result += dv(index) * value})
      result
    }
    case v:SparseBinaryVector => v dot this
    case v:VectorTimesScalar => v dot this
    case v:SingletonBinaryVector => v dot this
    case v:SingletonVector => v dot this
    case sv:SparseHashVector => {
      var result = 0.0
      if (v.size > this.size) h.iterator.foreach({case(index,value) => result += sv(index) * value})
      else sv.h.iterator.foreach({case(index,value) => result += h(index) * value})
      result
    }
    case _ => throw new Error("SparseHashVector.dot does not handle "+v.getClass.getName)
  }
  override def +=(v:Vector): Unit = v.activeElements.foreach({case(index,value) => h.update(index, h(index) + value)})
  override def +=(s:Double): Unit = {
    default += s
    h.keys.foreach(index => h.update(index, h(index) + s))
  }
}

/** A Vector that may contain arbitrary Double values, represented internally as an Array[Double].
    @author Andrew McCallum */
class DenseVector(val length:Int) extends Vector {
  private val a = new Array[Double](length)
  def domainSize = a.size
  def activeDomain = new Range(0, length, 1)
  def apply(index:Int): Double = a(index)
  override def update(index:Int, value:Double): Unit = a(index) = value
  def set(value:Double): Unit = java.util.Arrays.fill(a, value)
  def activeElements = new Iterator[(Int,Double)] {
    var i = -1
    def hasNext = i < DenseVector.this.length - 1
    def next = { i += 1; (i, a(i)) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => {
      var result = 0.0
      var i = 0
      while (i < a.size) { result += a(i)*v(i); i += 1 }
      result
    }
    case _ => v dot this
  }
  override def +=(v:Vector): Unit = for ((index,value) <- v.activeElements) a(index) += value
  override def +=(s:Double): Unit = {
    var i = 0
    while (i < a.size) { a(i) += s; i += 1 }
  }
}

/** Provides a convenient constructor for DenseVector objects.
    @author Andrew McCallum */
object DenseVector {
  def apply(size:Int)(default:Double) = { 
    val result = new DenseVector(size)
    result.set(default)
    result
  }
}

/*
class ConcatenatedVector(val vectors:Seq[Vector]) extends Vector {
  val domainSizes = vectors.map(_.domainSize)
  val domainSize = vectors.reduceLeft(_+_.domainSize)
  def activeDomain = Range(0, domainSize)
  def dot(v:Vector): Double = throw new Error("Not yet implemented.")
  def activeElements: Iterator[(Int,Double)] = throw new Error("Not yet implemented.")
  def update(index:Int, value:Double): Unit = {
    val maxLowerIndex = domainsSizes.find(_)
  }
}
*/
