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

import collection.mutable.ArrayBuffer

/** A Vector that is represented by an array of pointers to Vectors, in which some of the pointers may be null.
    @author Andrew McCallum, Brian Martin */


//TODO: this should extend SparseOuter1DenseVector1, just overriding the dot? --brian
/** The result of the statistics.vector (through flatOuter) from a Factor2[DiscreteVar,DiscreteVectorVar] */
class SparseOuter1sVector1(val i1:Int, val length1:Int, val inner:Vector) extends Vector {
  val length = length1 * inner.length
  val offset = i1 * length1
  def inner(i:Int): Vector = if (i == 0) inner else throw new Error // TODO Avoid the error?
  def apply(i:Int): Double = if (i < offset || i >= offset + inner.length) 0.0 else inner(i - offset)
  def activeDomainSize: Int = inner.activeDomainSize
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

/** A representation for weights of DotFamily2[DiscreteVar,DiscreteVectorVar] that can be sparse in domain of the DiscreteVar */
class SparseOuter1DenseVector1(val length1:Int, val length2:Int) extends Vector {
  private val inners = new Array[DenseVector](length1)
  def inner(i:Int) = inners(i)
  def length = length1 * length2
  def apply(i:Int): Double = inners(i / length2).apply(i % length2)
  private var _activeSize = 0
  def activeDomainSize = _activeSize
  private var _activeDomains = ArrayBuffer[Int]() // a list of the first index of each instantiated inner
  // should this by lazier? --brian
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] {
    val result = new Array[Int](_activeSize)
    var currResultIdx = 0
    var i = 0
    var j = -1
    var currDomain = _activeDomains(i) * length2  // do the multiplication here to avoid it for every j
    while (j < length2-1 || i < _activeDomains.size-1) {
      if (j+1 == length2) { j = 0; i += 1; currDomain = _activeDomains(i) * length2 }
      else j += 1
      result(currResultIdx) = currDomain + j
      currResultIdx += 1
    }
    def length = _activeSize
    def apply(k: Int) = result(k)
  }
  // copied from activeDomain for use of the internal i's and j's to skip apply
  def activeElements: Iterator[(Int,Double)] = {
    var result = new Array[(Int, Double)](_activeDomains.size * length2)
    var currResultIdx = 0
    var i = 0
    var j = -1
    var currDomain = _activeDomains(i) // consider doing the *length2 here, and also keeping _activeDomains(i) for indexing into inners
    while (j < length2-1 || i < _activeDomains.size-1) {
      if (j+1 == length2) { j = 0; i += 1; currDomain = _activeDomains(i) }
      else j += 1
      result(currResultIdx) = (currDomain * length2 + j, inners(currDomain).apply(j))
      currResultIdx += 1
    }
    result.iterator
  }

  def dot(v:Vector): Double = v match {
    // TODO Make this more efficient!!
    case v:DenseVector => v.activeElements.foldLeft(0.0)((result:Double, elt:(Int,Double)) => result + elt._2 * v(elt._1))
    case v:SparseOuter1DenseVector1 => {
      assert(v.length1 == length1)
      
      0.0
    }
  }
  override def update(index:Int, value:Double): Unit = {
    val i = index / length2
    if (inners(i) ne null) inners(i)(index % length2) = value
    else {
      val v = new DenseVector(length2)
      _activeSize += length2
      _activeDomains.append(i)
      v(index % length2) = value
      inners(i) = v
    }
  }
  override def increment(index:Int, incr:Double): Unit = {
    val i = index / length2
    if (inners(i) ne null) inners(i)(index % length2) += incr
    else {
      val v = new DenseVector(length2)
      _activeSize += length2
      _activeDomains.append(i)
      v(index % length2) = incr
      inners(i) = v
    }
  }
  override def +=(v:Vector): Unit = v match {
    case v: SparseVector => {
      val aes = v.activeElements
      while(aes.hasNext) {
        val ae = aes.next()
        increment(ae._1, ae._2)
      }
    }
    case d: DenseVector => {
      val activeIdxs = this.activeDomain.iterator
      while (activeIdxs.hasNext) {
        val ai = activeIdxs.next
        this.increment(ai, d.apply(ai))
      }
    }
    case _ => throw new Error("Not yet implemented")
  }

}

// TODO: inline inner for readability and code dedup? --brian
class SparseOuter2DenseVector1(val length1:Int, val length2:Int, val length3:Int) extends Vector {
  private val inners = new Array[DenseVector](length1*length2)
  private val l2Timesl3 = length2 * length3
  def inner(i:Int, j:Int): DenseVector = inners(i*length2 + j)
  def length = length1 * l2Timesl3
  def apply(i:Int): Double = {
    val i1 = i / l2Timesl3
    val i2 = (i % l2Timesl3) / length3
    val i3 = i % length3
    inner(i1,i2).apply(i3) // TODO: skip the call to inner here?
  }
  private var _activeSize = 0
  def activeDomainSize: Int = _activeSize
  private var _activeDomains = ArrayBuffer[Int]() // list of indices into inners of non-null DenseVectors
  // TODO: this should throw an error if the _activeSize == 0
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] {
    val result = new Array[Int](_activeSize)
    var currResultIdx = 0
    var currDomainIdx = 0
    var j = -1
    var currDomain = _activeDomains.apply(currDomainIdx)
    while (j < (length3 - 1) || currDomainIdx < (_activeDomains.size - 1)) {
      if (j+1 == length3) {
        j = 0
        currDomainIdx += 1
        currDomain = _activeDomains(currDomainIdx)
      }
      else j += 1
      result(currResultIdx) = currDomain * length3 + j
      currResultIdx += 1
    }
    def length = _activeSize
    def apply(k: Int) = result(k)
  }
  // code copied from above for access to currDomain and j
  def activeElements: Iterator[(Int, Double)] = {
    val result = new Array[(Int, Double)](_activeSize)
    var currResultIdx = 0
    var currDomainIdx = 0
    var j = -1
    var currDomain = _activeDomains.apply(currDomainIdx)
    while (j < (length3 - 1) || currDomainIdx < (_activeDomains.size - 1)) {
      if (j+1 == length3) {
        j = 0
        currDomainIdx += 1
        currDomain = _activeDomains(currDomainIdx)
      }
      else j += 1
      val currIdx = currDomain * length3 + j
      result(currResultIdx) = (currIdx, inners(currDomain).apply(j)) // this should be the only diff with activeDomain
      currResultIdx += 1
    }
    result.iterator
  }
  def dot(v: Vector): Double = {
    throw new Error("Not yet implemented")
    0.0
  }
  override def update(index: Int, value: Double): Unit = {
    val i1 = index / l2Timesl3
    val i2 = (index % l2Timesl3) / length3
    if (inner(i1, i2) ne null)
      inners(i1 * length2 + i2).update(index % length3, value)
    else {
      val v = new DenseVector(length3)
      _activeSize += length3
      val innersIdx = i1 * length2 + i2
      _activeDomains.append(innersIdx)
      v(index % length3) = value
      inners(innersIdx) = v
    }
  }
  override def increment(index: Int, value: Double): Unit = {
    val i1 = index / l2Timesl3
    val i2 = (index % l2Timesl3) / length3
    if (inner(i1, i2) ne null)
      inners(i1 * length2 + i2).increment(index % length3, value)
    else {
      val v = new DenseVector(length3)
      _activeSize += length3
      val innersIdx = i1 * length2 + i2
      _activeDomains.append(innersIdx)
      v(index % length3) = value
      inners(innersIdx) = v
    }
  }
  override def +=(v: Vector): Unit = {
    throw new Error("Not yet implemented")
  }
}

