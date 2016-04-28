/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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
import cc.factorie.util.{IntSeq, DoubleSeq, SingletonIntSeq, RangeIntSeq, DoubleSeqIterator, SparseDoubleSeq}

trait Tensor4 extends Tensor {
  def dim1: Int
  def dim2: Int
  def dim3: Int
  def dim4: Int
  def activeDomain1: IntSeq
  def activeDomain2: IntSeq
  def activeDomain3: IntSeq
  def activeDomain4: IntSeq
  def numDimensions: Int = 4
  def activeDomains = Array(activeDomain1, activeDomain2, activeDomain3, activeDomain4)
  def dimensions = Array(dim1, dim2, dim3, dim4)
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:Tensor4 => t.dim1 == dim1 && t.dim2 == dim2 && t.dim3 == dim3 && t.dim4 == dim4
    case _ => false
  } 
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor4 => require(t.dim1 == dim1 && t.dim2 == dim2 && t.dim3 == dim3 && t.dim4 == dim4)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  def apply(i:Int, j:Int, k:Int, l:Int): Double = apply(singleIndex(i,j,k,l))
  def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = update(singleIndex(i,j,k,l), v)
  def +=(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = +=(singleIndex(i, j, k, l), v)
  @inline final def length = dim1 * dim2 * dim3 * dim4
  @inline final def singleIndex(i:Int, j:Int, k:Int, l:Int): Int = {
    if ((i < 0) || (j < 0) || (k < 0) || (l < 0)) {
      throw new IndexOutOfBoundsException("Negative indices are not allowed, ("+i+","+j+","+k+","+l+") supplied.")
    } else if ((i >= dim1) || (j >= dim2) || (k >= dim3) || (l >= dim4)) {
      throw new IndexOutOfBoundsException("Indices ("+i+","+j+","+k+","+l+") are out of bounds for Tensor4("+dim1+","+dim2+","+dim3+","+dim4+")")
    } else {
      i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l
    }
  }
  @inline final def multiIndex(i:Int): (Int, Int, Int, Int) = (i/dim2/dim3/dim4, (i/dim3/dim4)%dim2, (i/dim4)%dim3, i%dim4)
  @inline final def index1(i:Int): Int = i/dim2/dim3/dim4
  @inline final def index2(i:Int): Int = (i/dim3/dim4)%dim2
  @inline final def index3(i:Int): Int = (i/dim4)%dim3
  @inline final def index4(i:Int): Int = i%dim4
  override def copy: Tensor4 = throw new Error("Method copy not defined on class "+getClass.getName)
  override def blankCopy: Tensor4 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

trait DenseTensorLike4 extends Tensor4 with DenseTensor {
  //private var __values = new Array[Double](dim1*dim2*dim3*dim4)
  //protected def _values = __values
  //def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain4 = new RangeIntSeq(0, dim4)
  //def activeDomain = new RangeIntSeq(0, dim1*dim2*dim3*dim4)
  //def apply(i:Int): Double = __values(i)
  //override def apply(i:Int, j:Int, k:Int, l:Int): Double = __values(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l)
  //override def +=(i:Int, v:Double): Unit = __values(i) += v
  //override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); var i = 0; while (i < length) { __values(i) += ds(i); i += 1 } }
  //override def update(i:Int, v:Double): Unit = __values(i) = v
  //override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = __values(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l) = v
  //override def dot(t:DoubleSeq): Double = t match {
  //  case t:SingletonTensor4 => apply(t.singleIndex) * t.singleValue
  //  case t:SingletonBinaryTensor4 => apply(t.singleIndex)
  //  case t:DenseTensorLike4 => Tensor.dot(this, t)
  //}
}
class DenseTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int) extends DenseTensorLike4 {
  override def copy: DenseTensor4 = { val t = new DenseTensor4(dim1, dim2, dim3, dim4); System.arraycopy(_values, 0, t._values, 0, length); t }
  override def blankCopy: DenseTensor4 = new DenseTensor4(dim1, dim2, dim3, dim4)
}

trait Tensor4ElementIterator extends DoubleSeqIterator with Iterator[Tensor4ElementIterator] {
  def index: Int
  def index1: Int
  def index2: Int
  def index3: Int
  def index4: Int
  def value: Double
}

class SparseIndexedTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int) extends Tensor4 with ArraySparseIndexedTensor {
  def activeDomain1: IntSeq = throw new Error("Not yet implemented")
  def activeDomain2: IntSeq = throw new Error("Not yet implemented")
  def activeDomain3: IntSeq = throw new Error("Not yet implemented")
  def activeDomain4: IntSeq = throw new Error("Not yet implemented")
  def activeElements4: Tensor4ElementIterator = {
    _makeReadable()
    new Tensor4ElementIterator { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def index1 = SparseIndexedTensor4.this.index1(_indices(i-1))
      def index2 = SparseIndexedTensor4.this.index2(_indices(i-1))
      def index3 = SparseIndexedTensor4.this.index3(_indices(i-1))
      def index4 = SparseIndexedTensor4.this.index4(_indices(i-1))
      def value = _values(i-1)
      def next() = { i += 1; this }
    }
  }
  override def blankCopy: SparseIndexedTensor4 = new SparseIndexedTensor4(dim1, dim2, dim3, dim4)
  override def copy: SparseIndexedTensor4 = { val t = new SparseIndexedTensor4(dim1, dim2, dim3, dim4); this.copyInto(t); t }
}

class SingletonBinaryTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleIndex4:Int) extends Tensor4 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = new SingletonIntSeq(singleIndex4)
  def activeDomain = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4 + singleIndex4
  override def copy = new SingletonBinaryTensor4(dim1, dim2, dim3, dim4, singleIndex1, singleIndex2, singleIndex3, singleIndex4)
}
class MutableSingletonBinaryTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, var singleIndex1:Int, var singleIndex2:Int, var singleIndex3:Int, var singleIndex4:Int) extends Tensor4 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = new SingletonIntSeq(singleIndex4)
  def activeDomain = new SingletonIntSeq(singleIndex)
  def singleIndex = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4 + singleIndex4
  override def copy = new MutableSingletonBinaryTensor4(dim1, dim2, dim3, dim4, singleIndex1, singleIndex2, singleIndex3, singleIndex4)
}

// TODO why don't we just store "singleIndex" - less memory and simpler implementation - this just sems weird for something whose purpose is mostly to do dot products -luke
class SingletonTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleIndex4:Int, val singleValue:Double) extends Tensor4 with SingletonIndexedTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = new SingletonIntSeq(singleIndex4)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4 + singleIndex4
  override def copy = new SingletonTensor4(dim1, dim2, dim3, dim4, singleIndex1, singleIndex2, singleIndex3, singleIndex4, singleValue)
}

trait SparseBinaryTensorLike4 extends Tensor4 with ArraySparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
  def activeDomain3 = throw new Error("Not yet implemented")
  def activeDomain4 = throw new Error("Not yet implemented")
}
class SparseBinaryTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int) extends SparseBinaryTensorLike4 {
  override def blankCopy: SparseBinaryTensor4 = new SparseBinaryTensor4(dim1, dim2, dim3, dim4)
}


trait Dense3LayeredTensorLike4 extends Tensor4 with SparseDoubleSeq {
  def newTensor1: Int=>Tensor1
  def foreachActiveElement(f: (Int, Double) => Unit) = throw new Error("not implemented")
  def activeDomainSize = activeDomain.size
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain4 = new RangeIntSeq(0, dim4) // It probably could be more sparse than this
  def activeDomain = new RangeIntSeq(0, length) // Actually more sparse than this
  private val _inners = Array.fill(dim1*dim2*dim3)(newTensor1(dim4))
  override def zero() = _inners.foreach(_.zero())
  override def apply(i:Int, j:Int, k:Int, l:Int): Double = _inners(i*dim2*dim3 + j*dim3 + k).apply(l)
  def isDense = false
  def apply(i:Int): Double = apply(index1(i), index2(i), index3(i), index4(i))
  override def update(i: Int, v: Double): Unit = update(index1(i), index2(i), index3(i), index4(i), v)
  override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = _inners(i*dim2*dim3 + j*dim3 + k).update(l, v)
  protected def getInner(i:Int, j:Int, k:Int): Tensor1 = { var in = _inners(i*dim2*dim3 + j*dim3 + k); if (in eq null) { in = newTensor1(dim4); _inners(i) = in }; in }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i), index2(i), index3(i)).+=(index4(i), incr)
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
}
class Dense3LayeredTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val newTensor1:Int=>Tensor1) extends Dense3LayeredTensorLike4 {
  override def blankCopy = new Dense3LayeredTensor4(dim1, dim2, dim3, dim4, newTensor1)
}

// singletonindexed3, tensor1
trait Singleton3LayeredTensorLike4 extends Tensor4 with SparseDoubleSeq with ReadOnlyTensor {
  def singleIndex1: Int
  def singleIndex2: Int
  def singleIndex3: Int
  def activeDomainSize = inner.activeDomainSize
  def foreachActiveElement(f: (Int, Double) => Unit) = inner.foreachActiveElement((i, v) => f(singleIndex(singleIndex1, singleIndex2, singleIndex3, i),v))
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int, k:Int, l:Int): Double = if (i == singleIndex1 && j == singleIndex2 && k == singleIndex3) inner.apply(l) else 0.0
  def apply(i:Int): Double = apply(i/dim2/dim3/dim4, (i/dim3/dim4)%dim2, (i/dim4)%dim3, i%dim4)
  override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = if (i == singleIndex1 && j == singleIndex2 && k == singleIndex3) inner.update(l, v) else throw new Error("Outer indices out of bounds: "+List(i,j,k))
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner(t.singleIndex4) else 0.0
    case t:SingletonTensor4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner(t.singleIndex4) * t.singleValue else 0.0
    case t:Singleton3LayeredTensorLike4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner.dot(t.inner) else 0.0
    case t:Dense3LayeredTensor4 => t.dot(this)
  }
}
class Singleton3LayeredTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val inner:Tensor1) extends Singleton3LayeredTensorLike4
