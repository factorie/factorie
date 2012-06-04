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

trait Tensor2 extends Tensor {
  def dim1: Int
  def dim2: Int
  def activeDomain1: IntSeq
  def activeDomain2: IntSeq
  def numDimensions: Int = 2
  def activeDomains = Array(activeDomain1, activeDomain2)
  def dimensions = Array(dim1, dim2)
  def apply(i:Int, j:Int): Double = apply(i*dim2 + j)
  def apply(i:Int): Double //= apply(i % dim1, i / dim2)
  def update(i:Int, j:Int, v:Double): Unit = update(i*dim2 + j, v)
  def +=(i:Int, j:Int, v:Double): Unit = +=(singleIndex(i, j), v)
  @inline final def length = dim1 * dim2
  @inline final def singleIndex(i:Int, j:Int): Int = i*dim2 + j
  @inline final def multiIndex(i:Int): (Int, Int) = (i/dim2, i%dim2)
  @inline final def index1(i:Int): Int = i/dim2
  @inline final def index2(i:Int): Int = i%dim2
}


trait DenseTensorLike2 extends Tensor2 with DenseTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  //override def apply(i:Int, j:Int): Double = __values(i*dim2+j)
  //override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); var i = 0; while (i < length) { _values(i) += ds(i); i += 1 } }
  //override def update(i:Int, j:Int, v:Double): Unit = __values(i*dim2+j) = v
  override def dot(t:DoubleSeq): Double = t match {
    //case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    //case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    //case t:DenseTensorLike2 => Tensor.dot(this, t)
    case t:SingletonLayeredTensorLike2 => t dot this
    case t:SingletonBinaryLayeredTensorLike2 => t dot this
    case t:DoubleSeq => super.dot(t)
  }
}

class DenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 {
  def this(t:Tensor2) = { this(t.dim1, t.dim2); this := t }
  override def blankCopy: DenseTensor2 = new DenseTensor2(dim1, dim2)
  override def stringPrefix = "DenseTensor2"
}

// TODO Make a GrowableDenseTensor2


class SingletonBinaryTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleIndex2:Int) extends Tensor2 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2 + singleIndex2
  override def stringPrefix = "SingletonBinaryTensor2"
}

class SingletonTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleIndex2:Int, val singleValue:Double) extends Tensor2 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2 + singleIndex2
}

trait SparseBinaryTensorLike2 extends Tensor2 with SparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
}
class SparseBinaryTensor2(val dim1:Int, val dim2:Int) extends SparseBinaryTensorLike2 {
  override def blankCopy: SparseBinaryTensor2 = new SparseBinaryTensor2(dim1, dim2)
  override def stringPrefix = "SparseBinaryTensor2"
}

trait DenseLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def newTensor1:Int=>Tensor1
  private var _inners = new Array[Tensor1](dim1) // Array.fill(dim1)(newTensor1(dim2))
  def activeDomain1 = { val a = new Array[Int](dim1); var i = 0; var j = 0; while (i < dim1) { if (_inners(i) ne null) { a(j) = i; j += 1 }; i += 1 }; new TruncatedArrayIntSeq(a, j) }
  def activeDomain2 = new RangeIntSeq(0, dim2) // This could perhaps be more sparse
  def activeDomain = { val b = new IntArrayBuffer; for (i <- 0 until dim1; j <- 0 until dim2) { if (apply(i,j) != 0.0) b += singleIndex(i,j) }; new ArrayIntSeq(b.toArray) } // Not very efficient; use _inner().activeDomain intead
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = {
    for (i <- 0 until dim1; if _inners(i) ne null) _inners(i).foreachActiveElement((j,v) => f(i*dim2+j,v))
  }
  override def apply(i:Int, j:Int): Double = { val in = _inners(i); if (in ne null) in.apply(j) else 0.0 }
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  def isDense = false
  override def update(i:Int, j:Int, v:Double): Unit = getInner(i).update(j, v)
  def update(i:Int, t:Tensor1): Unit = _inners(i) = t
  protected def getInner(i:Int): Tensor1 = { var in = _inners(i); if (in eq null) { in = newTensor1(dim2); _inners(i) = in }; in }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i)).+=(index2(i), incr)
  override def +=(ds:DoubleSeq): Unit = ds match {
    case t:SingletonBinaryTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, 1.0)
    case t:SingletonTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, t.singleValue)
    case t:SingletonLayeredTensorLike2 => { getInner(t.singleIndex1) += t.inner }
    case t:SingletonBinaryLayeredTensorLike2 => { getInner(t.singleIndex1) += t.inner }
    case t:DenseLayeredTensorLike2 => { val len = t._inners.length; var i = 0; while (i < len) { if (t._inners(i) ne null) getInner(i) += t._inners(i); i += 1 } }
    case t:DoubleSeq => throw new Error("Not yet implemented for class "+t.getClass.getName)
    //case t:DoubleSeq => super.+=(ds)
  }
  override def +=(ds:DoubleSeq, f:Double): Unit = ds match {
    case t:SingletonBinaryTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, f)
    case t:SingletonTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, f * t.singleValue)
    case t:SingletonLayeredTensorLike2 => { getInner(t.singleIndex1).+=(t.inner, f) }
    case t:SingletonBinaryLayeredTensorLike2 => { getInner(t.singleIndex1).+=(t.inner, f) }
    case t:DenseLayeredTensorLike2 => { val len = t._inners.length; var i = 0; while (i < len) { if (t._inners(i) ne null) getInner(i).+=(t._inners(i), f); i += 1 } }
    case t:DoubleSeq => throw new Error("Not yet implemented for class "+t.getClass.getName)
    //case t:DoubleSeq => super.+=(ds)
  }
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryLayeredTensorLike2 => { val inner = _inners(t.singleIndex1); if (inner ne null) inner.dot(t.inner) else 0.0 } // This is a common case, and should be fast
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    case t:SingletonLayeredTensorLike2 => { val inner = _inners(t.singleIndex1); if (inner ne null) inner.dot(t.inner) * t.singleValue1 else 0.0 }
    case t:DenseTensorLike2 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
    case t:DenseLayeredTensorLike2 => { var s = 0.0; for((inner1,inner2) <- _inners zip t._inners; if (inner1 ne null); if(inner2 ne null)) s += inner1.dot(inner2); s }
  }
}
class DenseLayeredTensor2(val dim1:Int, val dim2:Int, val newTensor1:Int=>Tensor1) extends DenseLayeredTensorLike2 {
  override def blankCopy: DenseLayeredTensor2 = new DenseLayeredTensor2(dim1, dim2, newTensor1)
  override def stringPrefix = "DenseLayeredTensor2"
}


trait SingletonLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def singleIndex1: Int
  def singleValue1: Double
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1 * dim2; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int): Double = if (i == singleIndex1) inner.apply(j) * singleValue1 else 0.0
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  override def update(i:Int, j:Int, v:Double): Unit = if (i == singleIndex1) inner.update(j, v/singleValue1) else throw new Error("Outer index out of bounds: "+i)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue * singleValue1
    case t:SingletonBinaryTensor2 => apply(t.singleIndex) * singleValue1
    case t:SingletonLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) * singleValue1 else 0.0
    case t:DenseLayeredTensorLike2 => t dot this
    case t:DenseTensorLike2 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
  }
}
class SingletonLayeredTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleValue1:Double, val inner:Tensor1) extends SingletonLayeredTensorLike2

trait SingletonBinaryLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def singleIndex1: Int
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1 * dim2; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int): Double = if (i == singleIndex1) inner.apply(j) else 0.0
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  override def update(i:Int, j:Int, v:Double): Unit = if (i == singleIndex1) inner.update(j, v) else throw new Error("Outer index out of bounds: "+i)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) else 0.0
    case t:SingletonLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) * t.singleValue1 else 0.0
    case t:DenseLayeredTensorLike2 => t dot this
    case t:DenseTensorLike2 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
  }
}
class SingletonBinaryLayeredTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val inner:Tensor1) extends SingletonBinaryLayeredTensorLike2

trait SparseLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def newTensor1: Int=>Tensor1
  // TODO Finish this
}

