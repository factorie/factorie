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

trait Tensor3 extends Tensor {
  def dim1: Int
  def dim2: Int
  def dim3: Int
  def activeDomain1: IntSeq
  def activeDomain2: IntSeq
  def activeDomain3: IntSeq
  def numDimensions: Int = 3
  def activeDomains = Array(activeDomain1, activeDomain2, activeDomain3)
  def dimensions = Array(dim1, dim2, dim3)
  def apply(i:Int, j:Int, k:Int): Double = apply(i*dim2*dim3 + j*dim3 + k)
  def update(i:Int, j:Int, k:Int, v:Double): Unit = update(i*dim2*dim3 + j*dim3 + k, v)
  def +=(i:Int, j:Int, k:Int, v:Double): Unit = +=(singleIndex(i, j, k), v)
  @inline final def length = dim1 * dim2 * dim3
  @inline final def singleIndex(i:Int, j:Int, k:Int): Int = i*dim2*dim3 + j*dim3 + k 
  @inline final def multiIndex(i:Int): (Int, Int, Int) = (i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  @inline final def index1(i:Int): Int = i/dim2/dim3
  @inline final def index2(i:Int): Int = (i/dim3)%dim2
  @inline final def index3(i:Int): Int = i%dim3
}

trait DenseTensorLike3 extends Tensor3 with DenseTensor {
  //private var __values = new Array[Double](dim1*dim2*dim3)
  //protected def _values = __values
  //def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  //def activeDomain = new RangeIntSeq(0, dim1*dim2*dim3)
  //def apply(i:Int): Double = __values(i)
  //override def apply(i:Int, j:Int, k:Int): Double = __values(i*dim2*dim3 + j*dim3 + k)
  //override def update(i:Int, v:Double): Unit = __values(i) = v
  //override def update(i:Int, j:Int, k:Int, v:Double): Unit = __values(i*dim2*dim3 + j*dim3 + k) = v
  //override def +=(i:Int, v:Double): Unit = __values(i) += v
}
class DenseTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends DenseTensorLike3 {
  override def blankCopy: DenseTensor3 = new DenseTensor3(dim1, dim2, dim3)
}

class SingletonBinaryTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int) extends Tensor3 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3 + singleIndex2*dim3 + singleIndex3
}

class SingletonTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleValue:Double) extends Tensor3 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3 + singleIndex2*dim3 + singleIndex3
}

trait SparseBinaryTensorLike3 extends Tensor3 with SparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
  def activeDomain3 = throw new Error("Not yet implemented")
}
class SparseBinaryTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends SparseBinaryTensorLike3 {
  override def blankCopy: SparseBinaryTensor3 = new SparseBinaryTensor3(dim1, dim2, dim3)
}

trait Singleton2BinaryLayeredTensorLike3 extends Tensor3 with SparseDoubleSeq {
  def singleIndex1: Int
  def singleIndex2: Int
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3 + singleIndex2*dim3; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int, k:Int): Double = if (i == singleIndex1 && j == singleIndex2) inner.apply(k) else 0.0
  def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = if (i == singleIndex1 && j == singleIndex2) inner.update(k, v) else throw new Error("Outer index out of bounds: "+i)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor3 => apply(t.singleIndex)
    case t:SingletonTensor3 => apply(t.singleIndex) * t.singleValue
    case t:Singleton2BinaryLayeredTensorLike3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner.dot(t.inner) else 0.0
    case t:Singleton2LayeredTensorLike3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner.dot(t.inner) * t.singleValue1 * t.singleValue2 else 0.0
    case t:Dense2LayeredTensorLike3 => throw new Error("Not yet implemented.")
    case t:DenseTensorLike3 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
  }
}
class Singleton2BinaryLayeredTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val inner:Tensor1) extends Singleton2BinaryLayeredTensorLike3

trait Dense2LayeredTensorLike3 extends Tensor3 with SparseDoubleSeq {
  def newTensor1: Int=>Tensor1
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain = new RangeIntSeq(0, length) // Actually more sparse than this
  private var _inners = Array.fill(dim1*dim2)(newTensor1(dim3))
  override def apply(i:Int, j:Int, k:Int): Double = {
    assert(i*dim2+j < dim1*dim2, "len="+length+" dim1="+dim1+" dim2="+dim2+" dim3="+dim3+" i="+i+" j="+j+" k="+k)
    val t1 = _inners(i*dim2+j)
    if (t1 ne null) t1.apply(k) else 0.0 }
  def isDense = false
  def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = _inners(i*dim2+j).update(j, v)
  protected def getInner(i:Int, j:Int): Tensor1 = { var in = _inners(i*dim2+j); if (in eq null) { in = newTensor1(dim2); _inners(i) = in }; in }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i), index2(i)).+=(index3(i), incr)
  override def dot(ds:DoubleSeq): Double = ds match {
    case t:SingletonBinaryTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3)
    case t:SingletonTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3) * t.singleValue
    case t:Singleton2BinaryLayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) else 0.0 }
    case t:Singleton2LayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) * t.singleValue1 * t.singleValue2 else 0.0 }
    case t:SparseBinaryTensor3 => { /*println("Dense2LayeredTensorLike3 this.length="+length+" t.length="+t.length+" dims="+t.dimensions.toSeq);*/ var s = 0.0; t.foreachActiveElement((i,v) => s += apply(i)); s }
  }
}
// TODO Consider also Dense1LayeredTensor3 with an InnerTensor2
class Dense2LayeredTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val newTensor1:Int=>Tensor1) extends Dense2LayeredTensorLike3 {
  override def blankCopy = new Dense2LayeredTensor3(dim1, dim2, dim3, newTensor1)  
}

trait Singleton2LayeredTensorLike3 extends Tensor3 with SparseDoubleSeq {
  def singleIndex1: Int
  def singleIndex2: Int
  def singleValue1: Double
  def singleValue2: Double
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3 + singleIndex2*dim3; inner.activeDomain1.map(_ * offset) }
  override def apply(i:Int, j:Int, k:Int): Double = if (i == singleIndex1 && j == singleIndex2) inner.apply(k)*singleValue1*singleValue2 else 0.0
  def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = if (i == singleIndex1 && j == singleIndex2) inner.update(k, v/(singleValue1*singleValue2)) else throw new Error("Outer indices out of bounds: "+List(i,j))
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner(t.singleIndex3) else 0.0
    case t:SingletonTensor3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner(t.singleIndex3) * t.singleValue else 0.0
    case t:Singleton2LayeredTensorLike3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner.dot(t.inner) else 0.0
  }
}
class Singleton2LayeredTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val singleValue1:Double, val singleValue2:Double, val inner:Tensor1) extends Singleton2LayeredTensorLike3
