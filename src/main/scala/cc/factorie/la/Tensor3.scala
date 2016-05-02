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
import cc.factorie.util.{IntSeq, DoubleSeq, RangeIntSeq, SingletonIntSeq, DoubleSeqIterator, SparseDoubleSeq}

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
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:Tensor3 => t.dim1 == dim1 && t.dim2 == dim2 && t.dim3 == dim3
    case _ => false
  } 
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor3 => require(t.dim1 == dim1 && t.dim2 == dim2 && t.dim3 == dim3)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  def apply(i:Int, j:Int, k:Int): Double = apply(singleIndex(i,j,k))
  def update(i:Int, j:Int, k:Int, v:Double): Unit = update(singleIndex(i,j,k),v)
  def +=(i:Int, j:Int, k:Int, v:Double): Unit = +=(singleIndex(i, j, k), v)
  @inline final def length = dim1 * dim2 * dim3
  @inline final def singleIndex(i:Int, j:Int, k:Int): Int = {
    if ((i < 0) || (j < 0) || (k < 0)) {
      throw new IndexOutOfBoundsException("Negative indices are not allowed, ("+i+","+j+","+k+") supplied.")
    } else if ((i >= dim1) || (j >= dim2) || (k >= dim3)) {
      throw new IndexOutOfBoundsException("Indices ("+i+","+j+","+k+") are out of bounds for Tensor3("+dim1+","+dim2+","+dim3+")")
    } else {
      i*dim2*dim3 + j*dim3 + k
    }
  }
  @inline final def multiIndex(i:Int): (Int, Int, Int) = (i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  @inline final def index1(i:Int): Int = i/dim2/dim3
  @inline final def index2(i:Int): Int = (i/dim3)%dim2
  @inline final def index3(i:Int): Int = i%dim3
  override def copy: Tensor3 = throw new Error("Method copy not defined on class "+getClass.getName)
  override def blankCopy: Tensor3 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
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
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:Dense2LayeredTensor3 => t.=+(_values, 0, f)
    case t:Tensor3 => super.+=(t, f)
    case t:Tensor => super.+=(t,f)
  }
}
class DenseTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends DenseTensorLike3 {
  override def copy = { val t = new DenseTensor3(dim1, dim2, dim3); System.arraycopy(_values, 0, t._values, 0, length); t }
  override def blankCopy = new DenseTensor3(dim1, dim2, dim3)
}

class GrowableDenseTensor3(d1:Int, d2:Int, d3:Int) extends { private var _dim1 = d1; private var _dim2 = d2; private var _dim3 = d3} with DenseTensorLike3 {
  def dim1: Int = _dim1
  def dim2: Int = _dim2
  def dim3: Int = _dim3
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor3 => ensureDimensions(t.dim1, t.dim2, t.dim3)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  def ensureDimensions(d1:Int, d2:Int, d3:Int): Unit = if (d1 > _dim1 || d2 > _dim2 | d3 > _dim3) {
    val newSize = d1 * d2 * d3 // math.max(_valuesSize * 2, d1 * d2 * d3)
    val oldValues = _values
    _resetValues(newSize) // allocates a new __values array of size newSize
    val _dim23 = _dim2 * _dim3
    val d23 = d2 * d3
    if (_dim1 + _dim2 + _dim3 > 0) for (i <- 0 until d1; j <- 0 until d2) {
      Array.copy(oldValues, i*_dim23+j*_dim3, _values, i*d23+j*d3, _dim3) // copy old values into place
      if (defaultValue != 0.0) java.util.Arrays.fill(_values, i*d23+j*d3+_dim3, d3-_dim3, defaultValue) // fill in new space with default value
    }
    if (d1 > _dim1) java.util.Arrays.fill(_values, _dim1*d23, (d1-_dim1)*d23, defaultValue)
    _dim1 = d1
    _dim2 = d2
    _dim3 = d3
  }
  // Currently these are the only methods that support capacity expansion
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); +=(t.singleIndex, f) }
    case t:SingletonTensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); +=(t.singleIndex, f * t.singleValue) }
    case t:SparseBinaryTensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); t.=+(_values, f) }
    case t:DenseTensorLike3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); super.+=(t, f) }
    //case t:Dense2LayeredTensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); { val len = t._inners.length; var i = 0; while (i < len) { val in = t._inners(i); if (in ne null) getInner(i).+=(in, f); i += 1 }} }
    //case t:Tensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); super.+=(t, f) }
    //case t:UniformTensor3 => { ensureDimensions(t.dim1, t.dim2, t.dim3); super.+=(t, f) }  //val len = length; val u = t.uniformValue * f; var i = 0; while (i < len) { __values(i) += u; i += 1 }
  }
  override def copy = { val c = new GrowableDenseTensor3(_dim1, _dim2, _dim3); c := this; c }
  override def blankCopy = new GrowableDenseTensor3(_dim1, _dim2, _dim3)
} 

/** A Tensor3 representing the outer product of a Tensor2 (e.g. DenseTensor2) and a Tensor1 (e.g. a SparseBinaryTensor1). */
class Outer2Tensor3(val tensor1:Tensor2, val tensor2:Tensor1) extends Outer2Tensor with Tensor3 {
  def dim1 = tensor1.dim1
  def dim2 = tensor1.dim2
  def dim3 = tensor2.dim1
  def activeDomain1 = tensor1.activeDomain1
  def activeDomain2 = tensor1.activeDomain2
  def activeDomain3 = tensor2.activeDomain1
  override def copy = new Outer2Tensor3(tensor1.copy, tensor2.copy)
  override def blankCopy = new Outer2Tensor3(tensor1.blankCopy, tensor2.blankCopy)
  override def foreachActiveElement(f: (Int, Double) => Unit) { tensor1.foreachActiveElement((i1, v1) => tensor2.foreachActiveElement((i2, v2) => f(singleIndex(tensor1.index1(i1), tensor1.index2(i1), i2), scale*v1*v2))) }
}

/** A Tensor3 representing the outer product of a Tensor1 (e.g. DenseTensor1) and a Tensor2 (e.g. a SparseBinaryTensor2). */
class Outer1Tensor3(val tensor1:Tensor1, val tensor2:Tensor2) extends Outer2Tensor with Tensor3 {
  def dim1 = tensor1.dim1
  def dim2 = tensor2.dim1
  def dim3 = tensor2.dim2
  def activeDomain1 = tensor1.activeDomain1
  def activeDomain2 = tensor2.activeDomain1
  def activeDomain3 = tensor2.activeDomain2
  override def copy = new Outer1Tensor3(tensor1.copy, tensor2.copy)
  override def blankCopy = new Outer1Tensor3(tensor1.blankCopy, tensor2.blankCopy)
  override def foreachActiveElement(f: (Int, Double) => Unit) { tensor1.foreachActiveElement((i1, v1) => tensor2.foreachActiveElement((i2, v2) => f(singleIndex(i1, tensor2.index1(i2), tensor2.index2(i2)), scale*v1*v2))) }
}

class SingletonBinaryTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int) extends Tensor3 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3 + singleIndex2*dim3 + singleIndex3
  override def copy = new SingletonBinaryTensor3(dim1, dim2, dim3, singleIndex1, singleIndex2, singleIndex3)
}
class MutableSingletonBinaryTensor3(val dim1:Int, val dim2:Int, val dim3:Int, var singleIndex1:Int, var singleIndex2:Int, var singleIndex3:Int) extends Tensor3 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain = new SingletonIntSeq(singleIndex)
  def singleIndex = singleIndex1*dim2*dim3 + singleIndex2*dim3 + singleIndex3
  override def copy = new MutableSingletonBinaryTensor3(dim1, dim2, dim3, singleIndex1, singleIndex2, singleIndex3)
}

class SingletonTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val singleIndex1:Int, val singleIndex2:Int, val singleIndex3:Int, val singleValue:Double) extends Tensor3 with SingletonIndexedTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = new SingletonIntSeq(singleIndex3)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2*dim3 + singleIndex2*dim3 + singleIndex3
  override def copy = new SingletonTensor3(dim1, dim2, dim3, singleIndex1, singleIndex2, singleIndex3, singleValue)
}

trait SparseBinaryTensorLike3 extends Tensor3 with ArraySparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
  def activeDomain3 = throw new Error("Not yet implemented")
  def +=(i:Int, j:Int, k:Int): Unit = _insertSortedNoDuplicates(singleIndex(i,j,k))
}
class SparseBinaryTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends SparseBinaryTensorLike3 {
  override def blankCopy: SparseBinaryTensor3 = new SparseBinaryTensor3(dim1, dim2, dim3)
}

trait Tensor3ElementIterator extends DoubleSeqIterator with Iterator[Tensor3ElementIterator] {
  def index: Int
  def index1: Int
  def index2: Int
  def index3: Int
  def value: Double
}

class SparseIndexedTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends Tensor3 with ArraySparseIndexedTensor {
  def activeDomain1: IntSeq = throw new Error("Not yet implemented")
  def activeDomain2: IntSeq = throw new Error("Not yet implemented")
  def activeDomain3: IntSeq = throw new Error("Not yet implemented")
  def activeElements3: Tensor3ElementIterator = {
    _makeReadable()
    new Tensor3ElementIterator { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def index1 = SparseIndexedTensor3.this.index1(_indices(i-1))
      def index2 = SparseIndexedTensor3.this.index2(_indices(i-1))
      def index3 = SparseIndexedTensor3.this.index3(_indices(i-1))
      def value = _values(i-1)
      def next() = { i += 1; this }
    }
  }
  override def blankCopy = new SparseIndexedTensor3(dim1, dim2, dim3)
  override def copy = { val t = new SparseIndexedTensor3(dim1, dim2, dim3); this.copyInto(t); t }
}

// singleton2, tensor1

trait Singleton2BinaryLayeredTensorLike3 extends Tensor3 with SparseDoubleSeq with ReadOnlyTensor {
  def activeDomainSize = inner.activeDomainSize
  def singleIndex1: Int
  def singleIndex2: Int
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3 + singleIndex2*dim3; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int, k:Int): Double = if (i == singleIndex1 && j == singleIndex2) inner.apply(k) else 0.0
  def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val offset = singleIndex1*dim2*dim3 + singleIndex2*dim3; inner.foreachActiveElement((i, v) => f(offset+i, v))}
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
  def activeDomainSize = activeDomain.size
  def newTensor1: Int=>Tensor1
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain = new RangeIntSeq(0, length) // Actually more sparse than this
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor3 => ensureDimensions(t.dim1, t.dim2, t.dim3)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  def ensureDimensions(d1:Int, d2:Int, d3:Int): Unit = if (d1 > dim1 || d2 > dim2) {
    val newInners = new Array[Tensor1](d1 * d2)
    for (i <- 0 until dim1)
      System.arraycopy(_inners, i*dim2, newInners, i*d2, dim2)
    throw new Error("Implementation not yet finished.")
  }
  protected val _inners = new Array[Tensor1](dim1*dim2) // Array.fill(dim1*dim2)(newTensor1(dim3)) // TODO We shouldn't pre-fill this array; leave it sparse
  override def apply(i:Int, j:Int, k:Int): Double = {
    assert(i*dim2+j < dim1*dim2, "len="+length+" dim1="+dim1+" dim2="+dim2+" dim3="+dim3+" i="+i+" j="+j+" k="+k)
    val t1 = _inners(i*dim2+j)
    if (t1 ne null) t1.apply(k) else 0.0
  }
  def isDense = false
  def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  override def update(i:Int, v: Double): Unit = update(i/dim2/dim3, (i/dim3)%dim2, i%dim3, v)
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = {
    var in = _inners(i*dim2+j)
    if (in eq null) { in = newTensor1(dim3); _inners(i*dim2+j) = in }
    in.update(k, v)
  }
  override def zero(): Unit = { val len = _inners.length; var i = 0; while (i < len) { val in = _inners(i); if (in ne null) inner(i).zero(); i += 1 }}
  /** Get the inner Tensor1 at first two dimensions index i,j.  Create it if necessary. */
  def inner(i:Int, j:Int): Tensor1 = { var in = _inners(i*dim2+j); if (in eq null) { in = newTensor1(dim3); _inners(i*dim2+j) = in }; in }
  /** Get the inner Tensor1 at first two dimensions index i.  Create it if necessary. */
  def inner(i:Int): Tensor1 = { var in = _inners(i); if (in eq null) { in = newTensor1(dim3); _inners(i) = in }; in }
  override def +=(i:Int, incr:Double): Unit = inner(index1(i), index2(i)).+=(index3(i), incr)
  override def +=(i1:Int, i2:Int, i3:Int, incr:Double): Unit = inner(i1, i2).+=(i3, incr)
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = {
    val len = _inners.length
    var i = 0
    while (i < len) {
      val in = _inners(i)
      if (in ne null) {
        inner(i).=+(a, offset+i*dim3, f)
      }
      i += 1
    }
  }
  def foreachActiveElement(f: (Int, Double) => Unit) {
    val len = _inners.length
    var i = 0
    while (i < len) {
      val in = _inners(i)
      if (in ne null) {
        in.foreachActiveElement((ii, vv) => f(i*dim3+ii, vv))
      }
      i += 1
    }
  }

  override def dot(ds:DoubleSeq): Double = ds match {
    case t:SingletonBinaryTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3)
    case t:SingletonTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3) * t.singleValue
    case t:Singleton2BinaryLayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) else 0.0 }
    case t:Singleton2LayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) * t.singleValue1 * t.singleValue2 else 0.0 }
    case t:Dense2LayeredTensor3 => { var r = 0.0; val len = _inners.length; var i = 0; while (i < len) { val in1 = _inners(i); val in2 = t._inners(i); if ((in1 ne null) && (in2 ne null)) r += in1 dot in2; i += 1 }; r }
    case t:SparseBinaryTensor3 => {
      /*println("Dense2LayeredTensorLike3 this.length="+length+" t.length="+t.length+" dims="+t.dimensions.toSeq);*/
      val tArr = t.activeDomain.array; val tLen = tArr.length; var s = 0.0; var i = 0
      while (i < tLen) { s += apply(tArr(i)); i += 1 }
      s
    }
    case t:DenseTensor => {
      var res = 0.0
      var i = 0
      while (i < _inners.length) {
        _inners(i) match {
          case in:DenseTensor =>
            var j = 0
            while (j < in.length) {
              res += t(singleIndex(i / dim2, i % dim2, j))*in(j)
              j += 1
            }
          case in:SparseIndexedTensor => in.foreachActiveElement((x,v) => res += t(singleIndex(i / dim2, i % dim2, x))*v)
        }
        i += 1
      }
      res
    }
    case t:SparseIndexedTensor => {var res = 0.0; t.foreachActiveElement((i, x) => res += this(i)*x); res}
    case t:DoubleSeq => assert(false, t.getClass.getName + " doesn't match") ; 0.0
  }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor3 => +=(t.singleIndex1, t.singleIndex2, t.singleIndex3, f)
    case t:SingletonTensor3 => +=(t.singleIndex1, t.singleIndex2, t.singleIndex3, f * t.singleValue)
    case t:Singleton2LayeredTensorLike3 => { val in = inner(t.singleIndex1, t.singleIndex2); in.+=(t.inner, f * t.singleValue1 * t.singleValue2) }
    case t:Dense2LayeredTensorLike3 => { val len = t._inners.length; var i = 0; while (i < len) { val in = t._inners(i); if (in ne null) inner(i).+=(in, f); i += 1 }}
    case t:SparseBinaryTensor3 => { var s = 0.0; t.foreachActiveElement((i,v) => +=(i, f)) }
    case t:Singleton2BinaryLayeredTensorLike3 => { val in = inner(t.singleIndex1, t.singleIndex2); in.+=(t.inner, f) }
    case t:DenseTensor => {
      var i = 0
      while (i < _inners.length) {
        inner(i) match {
          case in:DenseTensor =>
            var j = 0
            while (j < in.length) {
              in(j) += t(singleIndex(i / dim2, i % dim2, j))*f
              j += 1
            }
          case in:SparseIndexedTensor => in.foreachActiveElement((x,v) => in(x) += t(singleIndex(i / dim2, i % dim2, x))*f)
        }
        i += 1
      }
    }
    case t:SparseIndexedTensor => t.foreachActiveElement((i, x) => this(i) += f*x)
    case t:DoubleSeq => assert(false, t.getClass.getName + " doesn't match")
  }
}
// TODO Consider also Dense1LayeredTensor3 with an InnerTensor2
class Dense2LayeredTensor3(val dim1:Int, val dim2:Int, val dim3:Int, val newTensor1:Int=>Tensor1) extends Dense2LayeredTensorLike3 {
  override def blankCopy = new Dense2LayeredTensor3(dim1, dim2, dim3, newTensor1)
  override def copy = {
    val c = new Dense2LayeredTensor3(dim1, dim2, dim3, newTensor1)
    val innerCopy = _inners.map(t => if (t == null) null else t.copy)
    System.arraycopy(innerCopy, 0, c._inners, 0, innerCopy.length)
    c
  }

}

object Singleton2LayeredTensorLike3 {
  var haveWarned = false
}

// singletonindexed2, tensor1
trait Singleton2LayeredTensorLike3 extends Tensor3 with SparseDoubleSeq with ReadOnlyTensor {
  def singleIndex1: Int
  def singleIndex2: Int
  def singleValue1: Double
  def singleValue2: Double
  def foreachActiveElement(f: (Int, Double) => Unit) = inner.foreachActiveElement((i, v) => f(singleIndex(singleIndex1, singleIndex2, i), v))
  def activeDomainSize = activeDomain.length
  def inner: Tensor1
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain3 = inner.activeDomain1
  def activeDomain = { val offset = singleIndex1*dim2*dim3 + singleIndex2*dim3; inner.activeDomain1.map(_ + offset) }
  override def apply(i:Int, j:Int, k:Int): Double = if (i == singleIndex1 && j == singleIndex2) inner.apply(k)*singleValue1*singleValue2 else 0.0
  def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = if (i == singleIndex1 && j == singleIndex2) inner.update(k, v/(singleValue1*singleValue2)) else throw new Error("Outer indices out of bounds: "+List(i,j))
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner(t.singleIndex3) else 0.0
    case t:SingletonTensor3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner(t.singleIndex3) * t.singleValue else 0.0
    case t:Singleton2LayeredTensorLike3 => if (singleIndex1 == t.singleIndex1 && singleIndex2 == t.singleIndex2) inner.dot(t.inner) else 0.0
    case _: Tensor =>
      if (!Singleton2LayeredTensorLike3.haveWarned) {
        Singleton2LayeredTensorLike3.haveWarned = true
        println(s"Warning: Singleton2LayeredTensorLike3 has no dot case for ${t.getClass.getName}")
      }
      var res = 0.0; foreachActiveElement((i, v) => res += t(i)*v); res
  }
}
class Singleton2LayeredTensor3(val dim1:Int, val dim2:Int, val dim3:Int, var singleIndex1:Int, var singleIndex2:Int, var singleValue1:Double, var singleValue2:Double, var inner:Tensor1) extends Singleton2LayeredTensorLike3
