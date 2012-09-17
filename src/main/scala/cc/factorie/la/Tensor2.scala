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
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:Tensor2 => t.dim1 == dim1 && t.dim2 == dim2
    case _ => false
  } 
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor2 => require(t.dim1 == dim1 && t.dim2 == dim2)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  def apply(i:Int, j:Int): Double = apply(i*dim2 + j)
  def apply(i:Int): Double //= apply(i % dim1, i / dim2)
  def update(i:Int, j:Int, v:Double): Unit = update(i*dim2 + j, v)
  def +=(i:Int, j:Int, v:Double): Unit = +=(singleIndex(i, j), v)
  def matrixVector(t: Tensor1): Tensor1 = {
    assert(dim2 == t.dim1, "Dimensions don't match: " + dim2 + " " + t.dim1)
    val newT = new DenseTensor1(dim1)
    activeDomain1.foreach(i => activeDomain2.foreach(j => newT(i) += this(i,j)*t(j)))
    newT
  }
  @inline final def length = dim1 * dim2
  @inline final def singleIndex(i:Int, j:Int): Int = i*dim2 + j
  @inline final def multiIndex(i:Int): (Int, Int) = (i/dim2, i%dim2)
  @inline final def index1(i:Int): Int = i/dim2
  @inline final def index2(i:Int): Int = i%dim2
  override def copy: Tensor2 = throw new Error("Method copy not defined on class "+getClass.getName)
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
    case t:SingletonBinaryLayeredTensorLike2 => t dot this
    case t:SingletonLayeredTensorLike2 => t dot this
    case t:DoubleSeq => super.dot(t)
  }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryLayeredTensorLike2 => t.=+(_values, f)
    case t:SingletonLayeredTensorLike2 => t.=+(_values, f)
    //case t:DenseLayeredTensorLike2 => { val len = t.dim1; var i = 0; while (i < len) { val inner = t.inner(i); if (inner ne null) inner.=+(_values, i*dim2, f); i += 1 } }
    case t:DenseLayeredTensorLike2 => t.=+(_values, f)
    case t:DoubleSeq => super.+=(t, f)
  }
}

class DenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 {
  def this(t:Tensor2) = { this(t.dim1, t.dim2); this := t }
  def this(dim1:Int, dim2:Int, fillValue:Double) = { this(dim1, dim2); java.util.Arrays.fill(_values, fillValue) }
  override def copy: DenseTensor2 = { val t = new DenseTensor2(dim1, dim2); System.arraycopy(_values, 0, t._values, 0, length); t }
  override def blankCopy: DenseTensor2 = new DenseTensor2(dim1, dim2)
  override def stringPrefix = "DenseTensor2"
}

class GrowableDenseTensor2(d1:Int, d2:Int) extends { private var _dim1 = d1; private var _dim2 = d2 } with DenseTensorLike2 {
  println("GrowableDenseTensor2 new "+_dim1+","+_dim2)
  def dim1: Int = _dim1
  def dim2: Int = _dim2
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor2 => ensureDimensions(t.dim1, t.dim2)
    case _ => super.ensureDimensionsMatch(t)
  }
  def ensureDimensions(d1:Int, d2:Int): Unit = if (d1 > _dim1 || d2 > _dim2) {
    val newSize = d1 * d2 // math.max(_valuesSize * 2, d1 * d2)
    val oldValues = _values
    _resetValues(newSize) // allocates a new __values array of size newSize
    if (_dim1 + _dim2 > 0) for (i <- 0 until _dim1) {
      Array.copy(oldValues, i*_dim2, _values, i*d2, _dim2) // copy old values into place
      if (defaultValue != 0.0) java.util.Arrays.fill(_values, i*d2+_dim2, d2-_dim2, defaultValue) // fill in new space with default value
    }
    if (d1 > _dim1) java.util.Arrays.fill(_values, _dim1*d2, (d1-_dim1)*d2, defaultValue)
    println("GrowableDenseTensor2 grew from "+_dim1+","+_dim2+" to "+d1+","+d2)
    _dim1 = d1
    _dim2 = d2
  }
  // Currently these are the only methods that support capacity expansion
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor2 => { ensureDimensions(t.dim1, t.dim2); +=(t.singleIndex, f) }
    case t:SingletonTensor2 => { ensureDimensions(t.dim1, t.dim2); +=(t.singleIndex, f * t.singleValue) }
    case t:SparseBinaryTensor2 => { ensureDimensions(t.dim1, t.dim2); t.=+(_values, f) }
    case t:DenseTensorLike2 => { ensureDimensions(t.dim1, t.dim2); super.+=(t, f) }
    case t:DenseLayeredTensorLike2 => { ensureDimensions(t.dim1, t.dim2); val len = t.dim1; var i = 0; while (i < len) { val inner = t.inner(i); if (inner ne null) inner.=+(_values, i*dim2, f); i += 1 } }
    case t:UniformTensor2 => { ensureDimensions(t.dim1, t.dim2); super.+=(t, f) }  //val len = length; val u = t.uniformValue * f; var i = 0; while (i < len) { __values(i) += u; i += 1 }
  }
  override def copy: GrowableDenseTensor2 = { val c = new GrowableDenseTensor2(_dim1, _dim2); c := this; c }
  override def blankCopy: GrowableDenseTensor2 = new GrowableDenseTensor2(_dim1, _dim2)
} 

// TODO Make a GrowableDenseTensor2
trait SingletonBinaryTensorLike2 extends Tensor2 with SingletonBinaryTensor {
  def singleIndex1: Int
  def singleIndex2: Int
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain = new SingletonIntSeq(singleIndex)
  def singleIndex = singleIndex1*dim2 + singleIndex2
}

class SingletonBinaryTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var singleIndex2:Int) extends SingletonBinaryTensorLike2
//class MutableSingletonBinaryTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var singleIndex2:Int) extends SingletonBinaryTensorLike2

class SingletonTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleIndex2:Int, val singleValue:Double) extends Tensor2 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2 + singleIndex2
  override def copy = new SingletonBinaryTensor2(dim1, dim2, singleIndex1, singleIndex2)
}

trait SparseBinaryTensorLike2 extends Tensor2 with SparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
}
class SparseBinaryTensor2(val dim1:Int, val dim2:Int) extends SparseBinaryTensorLike2 {
  override def blankCopy: SparseBinaryTensor2 = new SparseBinaryTensor2(dim1, dim2)
  //override def stringPrefix = "SparseBinaryTensor2"
  //override def copy = new SparseBinaryTensor2(dim1, dim2, singleIndex1, singleIndex2)
}

class UniformTensor2(val dim1:Int, val dim2:Int, val uniformValue:Double) extends Tensor2 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain = new RangeIntSeq(0, length)
  override def copy = new UniformTensor2(dim1, dim2, uniformValue)
  override def +(t:Tensor): Tensor = t match {
    case t:UniformTensor2 => { require(dim1 == t.dim1 && dim2 == t.dim2); new UniformTensor2(dim1, dim2, uniformValue + t.uniformValue) }
    case t:Tensor1 => new DenseTensor2(dim1, dim2, uniformValue) + t
  }
}


trait DenseLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def newTensor1:Int=>Tensor1
  private var _inners = new Array[Tensor1](dim1) // var because may need to grow in Growable version
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
  def inner(i:Int): Tensor1 = _inners(i)
  protected def getInner(i:Int): Tensor1 = { var in = _inners(i); if (in eq null) { in = newTensor1(dim2); _inners(i) = in }; in }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = {
    val len = _inners.length; var i = 0; while (i < len) { val in = _inners(i); if (in ne null) inner(i).=+(a, offset+i*dim1, f); i += 1 }
  }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i)).+=(index2(i), incr)
  /*override def +=(ds:DoubleSeq): Unit = ds match {
    case t:SingletonBinaryTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, 1.0)
    case t:SingletonTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, t.singleValue)
    case t:SingletonLayeredTensorLike2 => { getInner(t.singleIndex1) += t.inner }
    case t:SingletonBinaryLayeredTensorLike2 => { getInner(t.singleIndex1) += t.inner }
    case t:DenseLayeredTensorLike2 => { val len = t._inners.length; var i = 0; while (i < len) { if (t._inners(i) ne null) getInner(i) += t._inners(i); i += 1 } }
    case t:DoubleSeq => throw new Error("Not yet implemented for class "+t.getClass.getName)
    //case t:DoubleSeq => super.+=(ds)
  }*/
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

// TODO Make a version of the above that uses an _innerValues: Array[Double] acting as (factor) weights multiplying the values of the inner Tensor1's?

trait SingletonLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def singleIndex1: Int
  def singleValue1: Double
  def inner: Tensor1
  def innerOffset: Int = singleIndex1 * dim2
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
  def innerOffset: Int = singleIndex1 * dim2
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
class SingletonBinaryLayeredTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var inner:Tensor1) extends SingletonBinaryLayeredTensorLike2

