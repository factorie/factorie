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

class SparseOuter1Vector1(i1:Int, length1:Int, val inner:Vector) extends SparseOuter1Vector(i1, length1) {
  val length = length1 * inner.length
  val offset = i1 * length1
  def inner(i:Int): Vector = if (i == 0) inner else throw new Error // TODO Avoid the error?
  def apply(i:Int): Double = if (i < offset || i >= offset + innerLength) 0.0 else v(i - offset)
  def activeDomainSize = inner.activeDomainSize
  def activeDomain: Iterable[Int] = new Seq[Int] {
    val va = inner.activeDomain.toSeq // TODO Will this be efficient?
    def length = inner.length
    def apply(i:Int) = offset + va(i)
  }
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val vi = inner.activeDomain.activeElements
    def hasNext = vi.hasNext
    def next: (Int,Double) = { val (i,d) = vi.next; (i + offset, d) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => activeElements.foldLeft(0.0)((result:Double, elt:(Int,Double)) => result + elt._2 * v(elt._1))
    case v:SparseOuter1Vector => inner.dot(v.inner(i1)) else 0.0 // TODO assert that lengths are equal?
    case v:SparseOuter1Vector1 => if (i1 == v.i1) inner.dot(v.inner) else 0.0
  }
}

class SparseOuter1SparseBinaryVector1(override val i1:Int, override val length1:Int, override val inner:SparseBinaryVector) 
extends OuterSparseVector1(dim1, dim1max, v) {
  override def dot(v:Vector): Double = v match {
    case v:DenseVector => { var i = 0; var result = 0.0; while (i < inner.ind.length) { result += v(ind(i) + offset); i += 1 }; result }
  }
}

class SparseOuter1DenseVector1(override val i1:Int, override val length1:Int, v:DenseVector) extends OuterSparseVector1(dim1, dim1max, v) {
  private val innerLength = inner.length
  override def activeDomain: Iterable[Int] = new Seq[Int] {
    def length = v.length
    def apply(i:Int) = offset + i
  }
  override def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    var i = -1
    def hasNext = i < v.length
    def next: (Int,Double) = { i += 1; (i + offset, v(i)) }
  }
  def dot(v2:Vector): Double = v2 match {
    case v2:DenseVector => {
      var result = 0.0
      var i = 0
      val len = v.length
      while (i < len) result += v(i) * v2(i + offset)
      result
    }
  }

}
*/
