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

package cc.factorie.la
import cc.factorie._

// TODO Rename file SparseOuterVector

/** A Vector that is represented by an array of pointers to Vectors, in which some of the pointers may be null.
    @author Andrew McCallum */

/*
abstract class SparseOuter1Vector(val i1:Int, val length1:Int) extends Vector {
  def this(i1:Int, l1:Int, initialInners:Vector*) = { this(i1, l1); throw new Error }
  def inner(i:Int): Vector
}

abstract class SparseOuter2Vector(val i1:Int, val length1:Int, val i2:Int, length2:Int) extends Vector {
  def this(i1:Int, l1:Int, i2:Int, l2:Int, initialInners:Vector*) = { this(i1, l1, i2, l2); throw new Error }
  def inner(i:Int, j:Int): Vector
}
*/

/** The result of the statistics.vector (through flatOuter) from a Factor2[DiscreteVar,DiscreteVectorVar] */
class SparseOuter1sVector1(val i1:Int, val length1:Int, val inner:Vector) extends Vector {
  val length = length1 * inner.length
  val offset = i1 * length1
  def inner(i:Int): Vector = if (i == 0) inner else throw new Error // TODO Avoid the error?
  def apply(i:Int): Double = if (i < offset || i >= offset + inner.length) 0.0 else inner(i - offset)
  def activeDomainSize = inner.activeDomainSize
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] {
    val va = inner.activeDomain.toSeq // TODO Will this be efficient?
    def length = inner.length
    def apply(i:Int) = offset + va(i)
  }
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val vi = inner.activeElements
    def hasNext = vi.hasNext
    def next: (Int,Double) = { val (i,d) = vi.next; (i + offset, d) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => inner match {
      case inner:SparseBinaryVector => { var i = 0; var result = 0.0; while (i < inner.ind.length) { result += v(inner.ind(i) + offset); i += 1 }; result }
      case inner:DenseVector => {
        var result = 0.0
        var i = 0
        val len = inner.length
        while (i < len) result += inner(i) * v(i + offset)
        result
      }
      case _ => activeElements.foldLeft(0.0)((result:Double, elt:(Int,Double)) => result + elt._2 * v(elt._1))
    }
    //case v:SparseOuter1Vector => inner.dot(v.inner(i1)) else 0.0 // TODO assert that lengths are equal?
    case v:SparseOuter1sVector1 => if (i1 == v.i1) inner.dot(v.inner) else 0.0
  }
}



// abstract class SparseOuter2Vector1(val i1:Int, val length1:Int, val i2:Int, val length2:Int)...


// TODO Remove this.
/*class InnerVector[A<:Vector](index1:Int, length1: Int, val inner:A) extends Vector {
  val length = length1 * inner.length
  val offset = index1 * length1
  def apply(i:Int): Double = if (i < offset || i >= offset + inner.length) 0.0 else inner(i - offset)
  def activeDomainSize = inner.activeDomainSize
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] {
    val va = inner.activeDomain.toSeq // TODO Will this be efficient?
    def length = inner.length
    def apply(i:Int) = offset + va(i)
  }
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val vi = inner.activeElements
    def hasNext = vi.hasNext
    def next: (Int,Double) = { val (i,d) = vi.next; (i + offset, d) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => activeElements.foldLeft(0.0)((result:Double, elt:(Int,Double)) => result + elt._2 * v(elt._1))
    //case v:SparseOuter1Vector => inner.dot(v.inner(i1)) else 0.0 // TODO assert that lengths are equal?
    //case v:SparseOuter1Vector1 => if (i1 == v.i1) inner.dot(v.inner) else 0.0
  }
}*/

/** A representation for weights of DotFamily2[DiscreteVar,DiscreteVectorVar] that can be sparse in domain of the DiscreteVar */
class SparseOuter1DenseVector1(val length1:Int, val length2:Int) extends Vector {
  private val inners = new Array[DenseVector](length1)
  def inner(i:Int) = inners(i)
  def length = length1 * length2
  def apply(i:Int): Double = { val i1 = i / length1; val i2 = i % length1; inners(i1).apply(i2) }
  def activeDomainSize = inners.filter(_ ne null).map(_.activeDomainSize).sum
  def activeDomain: Iterable[Int] = Range(0,length1).filter(inners(_) ne null).map(i => inners(i).activeDomain.map(j => j*i*length1)).flatten
  def activeElements: Iterator[(Int,Double)] = throw new Error("Not yet implemented.")
  def dot(v:Vector): Double = v match {
    // TODO Make this more efficient!!
    case v:DenseVector => v.activeElements.foldLeft(0.0)((result:Double, elt:(Int,Double)) => result + elt._2 * v(elt._1))
    case v:SparseOuter1DenseVector1 => {
      assert(v.length1 == length1)
      
      0.0
    }
  }
  override def update(index:Int, value:Double): Unit = {
    val i = index / length1
    if (inners(i) ne null) inners(i)(index % length1) = value
    else {
      val v = new DenseVector(length2)
      v(index % length1) = value
      inners(i) = v
    }
  }
  override def increment(index:Int, incr:Double): Unit = {
    val i = index / length1
    if (inners(i) ne null) inners(i)(index % length1) += incr
    else {
      val v = new DenseVector(length2)
      v(index % length1) = incr
      inners(i) = v
    }
  }
  override def +=(v:Vector): Unit = v match {
    case v:SparseVector => v.activeElements.foreach(t => increment(t._1, t._2))
    case _ => throw new Error("Not yet implemented")
  }

}

abstract class SparseOuter2DenseVector1(val length1:Int, val length2:Int, val length3:Int) extends Vector {
  private val inners = new Array[DenseVector](length1*length2)
  def inner(i:Int, j:Int) = inners(i*length2 + j)
  def length = length1 * length2 * length3
  //def apply(i:Int): Double = { val i1 = i / length1; val i2 = i % length1; inners(i1).apply(i2) }
}

