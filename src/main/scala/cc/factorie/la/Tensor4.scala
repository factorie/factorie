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
import cc.factorie.util._

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
  def apply(i:Int, j:Int, k:Int, l:Int): Double = apply(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l)
  def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = update(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l, v)
  def +=(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = +=(singleIndex(i, j, k, l), v)
  @inline final def length = dim1 * dim2 * dim3 * dim4
  @inline final def singleIndex(i:Int, j:Int, k:Int, l:Int): Int = i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l
  @inline final def multiIndex(i:Int): (Int, Int, Int, Int) = (i/dim2/dim3/dim4, (i/dim3/dim4)%dim2, (i/dim4)%dim3, i%dim4)
  @inline final def index1(i:Int): Int = i/dim2/dim3/dim4
  @inline final def index2(i:Int): Int = (i/dim3/dim4)%dim2
  @inline final def index3(i:Int): Int = (i/dim4)%dim3
  @inline final def index4(i:Int): Int = i%dim4
}

trait DenseTensorLike4 extends Tensor4 with DenseTensor {
  private var __values = new Array[Double](dim1*dim2*dim3*dim4)
  protected def _values = __values
  def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain4 = new RangeIntSeq(0, dim4)
  def activeDomain = new RangeIntSeq(0, dim1*dim2*dim3*dim4)
  def apply(i:Int): Double = __values(i)
  override def apply(i:Int, j:Int, k:Int, l:Int): Double = __values(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l)
  override def +=(i:Int, v:Double): Unit = __values(i) += v
  override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); var i = 0; while (i < length) { __values(i) += ds(i); i += 1 } }
  override def update(i:Int, v:Double): Unit = __values(i) = v
  override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = __values(i*dim2*dim3*dim4 + j*dim3*dim4 + k*dim4 + l) = v
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonTensor4 => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryTensor4 => apply(t.singleIndex)
    case t:DenseTensorLike4 => Tensor.dot(this, t)
  }
}
class DenseTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int) extends DenseTensorLike4 {
  override def blankCopy: DenseTensor4 = new DenseTensor4(dim1, dim2, dim3, dim4)
}

class SingletonBinaryTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleIndex4:Int) extends Tensor4 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = new SingletonIntSeq(singleIndex4)
  def activeDomain = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4 + singleIndex4
}

class SingletonTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleIndex4:Int, val singleValue:Double) extends Tensor4 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = new SingletonIntSeq(singleIndex4)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4 + singleIndex4
}

trait SparseBinaryTensorLike4 extends Tensor4 with SparseBinaryTensor {
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
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain4 = new RangeIntSeq(0, dim4) // It probably could be more sparse than this
  def activeDomain = new RangeIntSeq(0, length) // Actually more sparse than this
  private var _inners = Array.fill(dim1*dim2*dim3)(newTensor1(dim3))
  override def apply(i:Int, j:Int, k:Int, l:Int): Double = _inners(i*dim2*dim3 + j*dim3 + k).apply(l)
  def isDense = false
  def apply(i:Int): Double = apply(i/dim2/dim3/dim4, (i/dim3/dim4)%dim2, (i/dim4)%dim3, i%dim4)
  override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = _inners(i*dim2*dim3 + j*dim3 + k).update(l, v)
  protected def getInner(i:Int, j:Int, k:Int): Tensor1 = { var in = _inners(i*dim2*dim3 + j*dim3 + k); if (in eq null) { in = newTensor1(dim2); _inners(i) = in }; in }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i), index2(i), index3(i)).+=(index4(i), incr)
}
class Dense3LayeredTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val newTensor1:Int=>Tensor1) extends Dense3LayeredTensorLike4 {
  override def blankCopy = new Dense3LayeredTensor4(dim1, dim2, dim3, dim4, newTensor1)
}

trait Singleton3LayeredTensorLike4 extends Tensor4 with SparseDoubleSeq {
  def singleIndex1: Int
  def singleIndex2: Int
  def singleIndex3: Int
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain4 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3*dim4 + singleIndex2*dim3*dim4 + singleIndex3*dim4; inner.activeDomain1.map(_ * offset) }
  override def apply(i:Int, j:Int, k:Int, l:Int): Double = if (i == singleIndex1 && j == singleIndex2 && k == singleIndex3) inner.apply(l) else 0.0
  def apply(i:Int): Double = apply(i/dim2/dim3/dim4, (i/dim3/dim4)%dim2, (i/dim4)%dim3, i%dim4)
  override def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = if (i == singleIndex1 && j == singleIndex2 && k == singleIndex3) inner.update(l, v) else throw new Error("Outer indices out of bounds: "+List(i,j,k))
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner(t.singleIndex4) else 0.0
    case t:SingletonTensor4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner(t.singleIndex4) * t.singleValue else 0.0
    case t:Singleton3LayeredTensorLike4 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2 && singleIndex3 == t.singleIndex3) inner.dot(t.inner) else 0.0
  }
}
class Singleton3LayeredTensor4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val inner:Tensor1) extends Singleton3LayeredTensorLike4
