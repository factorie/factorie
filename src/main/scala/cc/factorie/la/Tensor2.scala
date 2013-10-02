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
  def diag(): Tensor1 = {
    val ret = new DenseTensor1(dim1)
    var i = 0; val len = dim1
    while (i < len) {
      ret(i) = apply(i, i)
      i += 1
    }
    ret
  }
  def apply(i:Int, j:Int): Double = apply(i*dim2 + j)
  def apply(i:Int): Double //= apply(i % dim1, i / dim2)
  def update(i:Int, j:Int, v:Double): Unit = update(i*dim2 + j, v)
  def +=(i:Int, j:Int, v:Double): Unit = +=(singleIndex(i, j), v)
  // TODO This method should have a better name -akm
  // TODO This method should be overriden in DenseLayeredTensor2 to use inner.dot for efficiency -akm
  def *(t: Tensor1): Tensor1 = {
    assert(dim2 == t.dim1, "Dimensions don't match: " + dim2 + " " + t.dim1)
    val newT = new DenseTensor1(dim1)
    activeDomain1.foreach(i => t.activeDomain1.foreach(j => newT(i) += this(i,j)*t(j)))
    newT
  }
  def leftMultiply(t: Tensor1): Tensor1 = {
    assert(dim1 == t.dim1, "Dimensions don't match: " + dim1 + " " + t.dim1)
    val newT = new DenseTensor1(dim2)
    t.foreachActiveElement((i,v) => activeDomain2.foreach(j => newT(j) += this(i,j)*t(i)))
    newT
  }
  def trace: Double = {
    assert(dim1 == dim2, "Dimensions don't match: " + dim1 + " " + dim2)
    (0 until dim1).map(n => apply(n, n)).sum
  }
  @inline final def length = dim1 * dim2
  @inline final def singleIndex(i:Int, j:Int): Int = i*dim2 + j
  @inline final def multiIndex(i:Int): (Int, Int) = (i/dim2, i%dim2)
  @inline final def index1(i:Int): Int = i/dim2
  @inline final def index2(i:Int): Int = i%dim2
  //def activeElements2: Tensor2ElementIterator
  override def copy: Tensor2 = throw new Error("Method copy not defined on class "+getClass.getName)
  override def blankCopy: Tensor2 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

trait DenseTensorLike2 extends Tensor2 with DenseTensor {
  var haveWarned = false
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  //override def apply(i:Int, j:Int): Double = __values(i*dim2+j)
  //override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); var i = 0; while (i < length) { _values(i) += ds(i); i += 1 } }
  //override def update(i:Int, j:Int, v:Double): Unit = __values(i*dim2+j) = v
  def activeElements2: Tensor2ElementIterator = {
    new Tensor2ElementIterator { // Must not change _indexs and _values during iteration!
      private var i = -1
      private val len = length
      def hasNext = i + 1 < len
      def index = i
      def index1 = DenseTensorLike2.this.index1(i)
      def index2 = DenseTensorLike2.this.index2(i)
      def value = apply(i)
      def next() = { i += 1; this }
    }
  }
  
  def toTensor1: DenseTensor1 = new DenseTensor1(_values)
  
  override def dot(t:DoubleSeq): Double = t match {
    //case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    //case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    //case t:DenseTensorLike2 => Tensor.dot(this, t)
    case t: SingletonBinaryLayeredTensorLike2 => t dot this
    case t: SingletonLayeredTensorLike2 => t dot this
    case t: DoubleSeq => super.dot(t)
  }
  /*
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryLayeredTensorLike2 => t.=+(_values, f)
    case t:SingletonLayeredTensorLike2 => t.=+(_values, f)
    //case t:DenseLayeredTensorLike2 => { val len = t.dim1; var i = 0; while (i < len) { val inner = t.inner(i); if (inner ne null) inner.=+(_values, i*dim2, f); i += 1 } }
    case t:DenseLayeredTensorLike2 => t.=+(_values, f)
    case t:SparseIndexedTensor2 => t.=+(_values, f)
    case t:DoubleSeq => super.+=(t, f)
  }
  */
  /*def +=(ds: DoubleSeq, factor: DoubleSeq, scalar: Double): Unit = (ds, factor) match {
    case (ds: SparseIndexedTensor2, factor: DenseTensor2) =>
      ds._makeReadable
      val indices = ds._indices
      val values = ds._values
      val numIndices = ds.activeDomainSize
      var i = 0
      while (i < numIndices) {
        val curIdx = indices(i)
        _values(curIdx) += values(i) * factor._values(curIdx) * scalar
        i += 1
      }
    case (ds:SparseDoubleSeq, fac) => { ds.foreachActiveElement((i,v) => +=(i,v*fac(i)*scalar)) }
    case (ds: DenseTensor, fa: DenseTensor) =>
      val dsA = ds.asArray
      val faA = fa.asArray
      var i = 0
      while (i < dsA.length) {
        this.+=(i, dsA(i)*fa(i)*scalar)
        i += 1
      }
    case (ds:DoubleSeq, fac) => {
      if (!haveWarned) {
        haveWarned = true
        println("DenseTensorLike2 dot unsupported type: " + ds.getClass.getName + " " + fac.getClass.getName)
      }
      val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, factor(i)*ds(i)*scalar); i += 1 }}
  }*/
}

class DenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 {
  def this(t:Tensor2) = { this(t.dim1, t.dim2); this := t }
  def this(values:Seq[Seq[Double]]) = { this(values.size, values.head.size); for (i <- 0 until dim1; j <- 0 until dim2) update(i, j, values(i)(j)) } // TODO Not very efficient
  def this(values:Array[Array[Double]]) = { this(values.size, values.head.size); for (i <- 0 until dim1; j <- 0 until dim2) update(i, j, values(i)(j)) } // TODO Not very efficient
  def this(dim1:Int, dim2:Int, fillValue:Double) = { this(dim1, dim2); java.util.Arrays.fill(_values, fillValue) }
  override def copy: DenseTensor2 = { val t = new DenseTensor2(dim1, dim2); System.arraycopy(_values, 0, t._values, 0, length); t }
  override def blankCopy: DenseTensor2 = new DenseTensor2(dim1, dim2)
  override def stringPrefix = "DenseTensor2"
  override def *(t: Tensor1): Tensor1 = {
//    assert(dim2 == t.dimensions.reduce(_ * _), "Dimensions don't match: " + dim2 + " " + t.dimensions)
    val newT = new DenseTensor1(dim1)
    val newArray = newT.asArray
    t match {
      case t: DenseTensor =>
        val tArr = t.asArray
        var col = 0
        while (col < tArr.length) {
          val v = tArr(col)
          var row = 0
          while (row < dim1) {
            val offset = row * dim2
            newArray(row) += (_values(offset + col) * v)
            row += 1
          }
          col += 1
        }
      case t: SparseBinaryTensor =>
        val tActiveDomainSize = t.activeDomainSize
        val tIndices = t._indices
        var ti = 0
        while (ti < tActiveDomainSize) {
          val col = tIndices(ti)
          var row = 0
          while (row < dim1) {
            val offset = row * dim2
            newArray(row) += _values(offset + col)
            row += 1
          }
          ti += 1
        }
      case t: SparseIndexedTensor =>
        val tActiveDomainSize = t.activeDomainSize
        val tIndices = t._indices
        val tValues = t._values
        var ti = 0
        while (ti < tActiveDomainSize) {
          val col = tIndices(ti)
          val v = tValues(ti)
          var row = 0
          while (row < dim1) {
            val offset = row * dim2
            newArray(row) += (_values(offset + col) * v)
            row += 1
          }
          ti += 1
        }
      case _ =>
        if (!haveWarned) {
          haveWarned = true
          println("Warning: unknown tensor type for *: " + t.getClass.getName)
        }
        t.foreachActiveElement((col, v) => {
          var row = 0
          while (row < dim1) {
            val offset = row * dim2
            newArray(row) += (_values(offset + col) * v)
            row += 1
          }
        })
    }
    newT
  }

  override def leftMultiply(t: Tensor1): Tensor1 = {
    val res = new DenseTensor1(dim2)
    t match {
      case t: DenseTensor =>
        var i = 0
        while (i < dim1) {
          var j = 0
          while (j < dim2) {
            res(j) += this(i,j)*t(i)
            j += 1
          }
          i += 1
        }
      case t: SparseBinaryTensor =>
        val len = t.activeDomainSize
        val indices = t._indices
        var i = 0
        while (i < len) {
          var j = 0
          while (j < dim2) {
            res(j) += this(indices(i), j)
            j += 1
          }
          i += 1
        }
      case t: SparseIndexedTensor =>
        val len = t.activeDomainSize
        val indices = t._indices
        val values = t._values
        var i = 0
        while (i < len) {
          var j = 0
          while (j < dim2) {
            res(j) += values(i) * this(indices(i), j)
            j += 1
          }
          i += 1
        }
      case _ =>
        if (!haveWarned) {
          haveWarned = true
          println("DenseTensor2 unrecognized leftMultiply type: " + t.getClass.getName)
        }
        super.leftMultiply(t)
    }
    res
  }
}

class GrowableDenseTensor2(d1:Int, d2:Int) extends { private var _dim1 = d1; private var _dim2 = d2 } with DenseTensorLike2 {
  // println("GrowableDenseTensor2 new "+_dim1+","+_dim2)
  def dim1: Int = _dim1
  def dim2: Int = _dim2
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor2 => ensureDimensions(t.dim1, t.dim2)
    case _ => super.ensureDimensionsMatch(t)
  }
  def ensureDimensions(d1:Int, d2:Int): Unit = {
    if (d1 > _dim1 || d2 > _dim2) {
      // grow in both dimensions, only in the specified amount
      val newSize = d1 * d2 // math.max(_dim1 * _dim2 * 4, d1 * d2)
      val oldValues = _values
      _resetValues(newSize) // allocates a new __values array of size newSize
      if (_dim1 + _dim2 > 0) for (i <- 0 until _dim1) {
        Array.copy(oldValues, i*_dim2, _values, i*d2, _dim2) // copy old values into place
        if (defaultValue != 0.0) java.util.Arrays.fill(_values, i*d2+_dim2, d2-_dim2, defaultValue) // fill in new space with default value
      }
      if (d1 > _dim1) java.util.Arrays.fill(_values, _dim1*d2, (d1-_dim1)*d2, defaultValue)
      // println("GrowableDenseTensor2 grew from "+_dim1+","+_dim2+" to "+d1+","+d2)
      _dim1 = d1
      _dim2 = d2
    }
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
  def activeElements2: Tensor2ElementIterator = {
    new Tensor2ElementIterator { // Must not change _indexs and _values during iteration!
      private var i = -1
      def hasNext = i < 0
      def index = singleIndex
      def index1 = singleIndex1
      def index2 = singleIndex2
      def value = 1.0
      def next() = { i += 1; this }
    }
  }
}

class SingletonBinaryTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var singleIndex2:Int) extends SingletonBinaryTensorLike2 {
  override def copy = new SingletonBinaryTensor2(dim1, dim2, singleIndex1, singleIndex2)
}
//class MutableSingletonBinaryTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var singleIndex2:Int) extends SingletonBinaryTensorLike2

class SingletonTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleIndex2:Int, val singleValue:Double) extends Tensor2 with SingletonIndexedTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = new SingletonIntSeq(singleIndex2)
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  val singleIndex = singleIndex1*dim2 + singleIndex2
  def activeElements2: Tensor2ElementIterator = {
    new Tensor2ElementIterator { // Must not change _indexs and _values during iteration!
      private var i = -1
      def hasNext = i < 0
      def index = singleIndex
      def index1 = singleIndex1
      def index2 = singleIndex2
      def value = 1.0
      def next() = { i += 1; this }
    }
  }
  override def copy = new SingletonTensor2(dim1, dim2, singleIndex1, singleIndex2, singleValue)
}

trait SparseBinaryTensorLike2 extends Tensor2 with ArraySparseBinaryTensor {
  def activeDomain1 = throw new Error("Not yet implemented")
  def activeDomain2 = throw new Error("Not yet implemented")
  def +=(i:Int, j:Int): Unit = _insertSortedNoDuplicates(singleIndex(i,j))
  def activeElements2: Tensor2ElementIterator = {
    _makeReadable()
    new Tensor2ElementIterator { // Must not change _indexs and _values during iteration!
      private var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def index1 = SparseBinaryTensorLike2.this.index1(_indices(i-1))
      def index2 = SparseBinaryTensorLike2.this.index2(_indices(i-1))
      def value = 1.0
      def next() = { i += 1; this }
    }
  }
}
class SparseBinaryTensor2(val dim1:Int, val dim2:Int) extends SparseBinaryTensorLike2 {
  override def blankCopy: SparseBinaryTensor2 = new SparseBinaryTensor2(dim1, dim2)
  //override def stringPrefix = "SparseBinaryTensor2"
  //override def copy = new SparseBinaryTensor2(dim1, dim2, singleIndex1, singleIndex2)
}

trait Tensor2ElementIterator extends DoubleSeqIterator with Iterator[Tensor2ElementIterator] {
  def index: Int
  def index1: Int
  def index2: Int
  def value: Double
}

class SparseIndexedTensor2(val dim1:Int, val dim2:Int) extends Tensor2 with ArraySparseIndexedTensor {
  def activeDomain1: IntSeq = new RangeIntSeq(0, dim1) // TODO Implement this so that it is really sparse -akm 
  def activeDomain2: IntSeq = throw new Error("Not yet implemented")
  def activeElements2: Tensor2ElementIterator = {
    _makeReadable()
    new Tensor2ElementIterator { // Must not change _indexs and _values during iteration!
      private var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def index1 = SparseIndexedTensor2.this.index1(_indices(i-1))
      def index2 = SparseIndexedTensor2.this.index2(_indices(i-1))
      def value = _values(i-1)
      def next() = { i += 1; this }
    }
  }
  override def blankCopy: SparseIndexedTensor2 = new SparseIndexedTensor2(dim1, dim2)
  override def copy: SparseIndexedTensor2 = { val t = new SparseIndexedTensor2(dim1, dim2); this.copyInto(t); t }
}

///** A Tensor2 that has dense storage, but a sparse activeDomain. */
//// TODO I think we should not keep this because there could be non-zero values in the sparse holes,
//// and we could get some very confusing results.
//class SparseDenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 {
//  override val activeDomain = new cc.factorie.util.SortedIntSetBuffer 
//  override def activeDomain1 = throw new Error("Not yet implemented")
//  override def activeDomain2 = throw new Error("Not yet implemented")
//  
//}

trait Outer2Tensor extends ReadOnlyTensor with SparseDoubleSeq {
  def activeDomainSize = tensor1.activeDomainSize * tensor2.activeDomainSize
  var scale = 1.0
  override def *=(d: Double) = scale *= d

  def tensor1: Tensor
  def tensor2: Tensor

// TODO TensorN likes to extend all this stuff so we can't provide good defaults if we want TensorNs that are outers (which we do) -luke
//  def length: Int = tensor1.length * tensor2.length
//  def numDimensions: Int = tensor1.numDimensions + tensor2.numDimensions
//  def dimensions: Array[Int] = tensor1.dimensions ++ tensor2.dimensions
//  def activeDomains: Array[IntSeq] = tensor1.activeDomains ++ tensor2.activeDomains
//  def trace: Double = {
//    assert(tensor1.length == tensor2.length)
//    tensor1 dot tensor2
//  }

  @inline final def singleFlatIndex(i:Int, j:Int): Int = i*tensor2.length + j
  def activeDomain = new Outer2IntSeq(tensor1.length, tensor2.length, tensor1.activeDomain, tensor2.activeDomain)
  def isDense: Boolean = false
  def apply(i: Int): Double = scale*tensor1(i / tensor2.length) * tensor2(i % tensor2.length)
  override def twoNormSquared = scale*scale*tensor1.twoNormSquared * tensor2.twoNormSquared
  override def =+(a: Array[Double], offset: Int, vv: Double): Unit = {
    val v = scale*vv
    // note that this is different from the singleIndex in Tensor2, as these are not dimensions but lengths of the whole tensors
    @inline def singleIndex(i: Int, j: Int): Int = i * tensor2.length + j
    if (offset != 0) super.=+(a, offset, v)
    else (tensor1, tensor2) match {
      case (t1: DenseTensor, t2: DenseTensor) =>
        val t2Size = t2.size
        val t1Size = t1.size
        val t1Values = t1.asArray
        val t2Values = t2.asArray
        var idx1 = 0
        while (idx1 < t1Size) {
          val v1 = t1Values(idx1) * v
          val offset = t2Size * idx1
          var idx2 = 0
          while (idx2 < t2Size) {
            val v2 = t2Values(idx2)
            a(offset + idx2) += (v1 * v2)
            idx2 += 1
          }
          idx1 += 1
        }
      case (t1: DenseTensor, t2: SparseTensor) =>
        val t2Size = t2.size
        val t1Size = t1.size
        val t1Values = t1.asArray
        val t2ActiveDomainSize = t2.activeDomainSize
        val t2Indices = t2._indices
        val t2Values = t2._valuesSeq
        var idx1 = 0
        while (idx1 < t1Size) {
          val v1 = t1Values(idx1) * v
          val offset = t2Size * idx1
          var t2i = 0
          while (t2i < t2ActiveDomainSize) {
            a(offset + t2Indices(t2i)) += (v1 * t2Values(t2i))
            t2i += 1
          }
          idx1 += 1
        }
      case (t1: SparseTensor, t2: DenseTensor) =>
        val t1Size = t1.activeDomainSize
        val t1Values = t1._valuesSeq
        val t1Indices = t1._indices
        val t2ActiveDomainSize = t2.length
        val t2Arr = t2.asArray
        var i = 0
        while (i < t1Size) {
          var j = 0
          while (j < t2ActiveDomainSize) {
            a(singleIndex(t1Indices(i),j)) += (t1Values(i) * t2Arr(j))*v
            j += 1
          }
          i += 1
        }
      case (t1: SingletonTensor, t2: SparseTensor) =>
        val t2Size = t2.activeDomainSize
        val t2IndexSeq = t2._indices
        val t2Values = t2._valuesSeq
        var i = 0
        while (i < t2Size) {
          a(singleIndex(t1.singleIndex,t2IndexSeq(i))) += v*t1.singleValue*t2Values(i)
          i += 1
        }
      case (t1: SparseTensor, t2: SparseTensor) =>
        val t1Size = t1.activeDomainSize
        val t1Indices = t1._indices
        val t1Values = t2._valuesSeq
        val t2Size = t2.activeDomainSize
        val t2IndexSeq = t2._indices
        val t2Values = t2._valuesSeq
        var i = 0
        while (i < t1Size) {
          var j = 0
          while (j < t2Size) {
            a(singleIndex(t1Indices(i),t2IndexSeq(j))) += v*t1Values(i)*t2Values(j)
            j += 1
          }
          i += 1
        }
      case (t1, t2) =>
        if (!Outer2Tensor.hasWarned) {
          Outer2Tensor.hasWarned = true
          println("Unrecognized types in Outer2Tensor =+: " + t1.getClass.getName + " " + t2.getClass.getName)
        }
        val t2Size = t2.size
        t1.foreachActiveElement((idx1, v1p) => {
          val v1 = v1p * v
          val offset = t2Size * idx1
          t2.foreachActiveElement((idx2, v2) => {
            a(offset + idx2) += (v1 * v2)
          })
        })
    }
  }
  def dot(ds: DoubleSeq): Double = scale * (ds match {
    case dt: DenseTensor =>
      (tensor1, tensor2) match {
        // NOTE if we added singletontensor/dense, this would cover all the singleton layered tensor stuff if we made those outer2tensors
        case (t1: SingletonBinaryTensor, t2: SingletonBinaryTensor) => dt(singleFlatIndex(t1.singleIndex,t2.singleIndex))
        case (t1: SingletonBinaryTensor, t2: SingletonIndexedTensor) => dt(singleFlatIndex(t1.singleIndex,t2.singleIndex))*t2.singleValue
        case (t1: SingletonTensor, t2: SparseTensor) =>
          val len = t2.activeDomainSize
          val indices = t2._indices
          val values = t2._valuesSeq
          var i = 0
          var dot = 0.0
          val arr = dt.asArray
          while (i < len) {
            dot += arr(singleFlatIndex(t1.singleIndex, indices(i)))*values(i)
            i += 1
          }
          dot*t1.singleValue
        case (t1: SparseTensor, t2: SingletonTensor) =>
          val len = t1.activeDomainSize
          val indices = t1._indices
          val values = t1._valuesSeq
          var i = 0
          var dot = 0.0
          while (i < len) {
            dot += dt(singleFlatIndex(indices(i), t2.singleIndex))*values(i)
            i += 1
          }
          dot*t2.singleValue
        case (t1: DenseTensor, t2: SparseTensor) =>
          val len = t2.activeDomainSize
          val indices = t2._indices
          val values = t2._valuesSeq
          var i = 0
          var dot = 0.0
          val t1Arr = t1.asArray
          while (i < t1Arr.length) {
            var j = 0
            while (j < len) {
              dot += dt(singleFlatIndex(i, indices(j)))*t1(i)*values(j)
              j += 1
            }
            i += 1
          }
          dot
        case (t1: SparseTensor, t2: DenseTensor) =>
          val len = t1.activeDomainSize
          val indices = t1._indices
          val values = t1._valuesSeq
          var i = 0
          var dot = 0.0
          val t2Arr = t2.asArray
          while (i < len) {
            var j = 0
            while (j < t2Arr.length) {
              dot += dt(singleFlatIndex(indices(i), j))*values(i)*t2Arr(j)
              j += 1
            }
            i += 1
          }
          dot
        case (t1: SparseTensor, t2: SparseTensor) =>
          var dot = 0.0
          val len1 = t1.activeDomainSize
          val indices1 = t1._indices
          val values1 = t2._valuesSeq
          val len2 = t2.activeDomainSize
          val indices2 = t2._indices
          val values2 = t2._valuesSeq
          var i = 0
          while (i < len1) {
            var j = 0
            while (j < len2) {
              dot += dt(singleFlatIndex(indices1(i), indices2(j)))*values1(i)*values2(j)
              j += 1
            }
            i += 1
          }
          dot
        case (t1: DenseTensor, t2: DenseTensor) =>
          var dot = 0.0
          var i = 0
          val arr1 = t1.asArray
          val len1 = t1.length
          val arr2 = t2.asArray
          val len2 = t2.length
          while (i < len1) {
            var j = 0
            while (j < len2) {
              dot += dt(singleFlatIndex(i,j))*arr1(i)*arr2(j)
              j += 1
            }
            i += 1
          }
          dot
      }
      case t: Outer2Tensor =>
        val firstDot = tensor1 dot t.tensor1
        t.scale * (if (firstDot == 0.0) 0.0 else firstDot * (tensor2 dot t.tensor2))
      // this obviously won't work if tensor1 and tensor2 aren't rank-1, still neat
      case t: Tensor2 => tensor1 dot (t * tensor2.asInstanceOf[Tensor1])
    })
}


/** A Tensor2 representing the outer product of a Tensor1 (e.g. DenseTensor1) and a Tensor1 (e.g. a SparseBinaryTensor1). */
class Outer1Tensor2(val tensor1:Tensor1, val tensor2:Tensor1) extends Outer2Tensor with Tensor2 {
  // we can dot against other outer tensors here efficiently i think
  def dim1 = tensor1.dim1
  def dim2 = tensor2.dim1
  def activeDomain1 = tensor1.activeDomain1
  def activeDomain2 = tensor2.activeDomain1
  override def copy = new Outer1Tensor2(tensor1.copy, tensor2.copy)
  override def blankCopy = new Outer1Tensor2(tensor1.blankCopy, tensor2.blankCopy)
  override def foreachActiveElement(f: (Int,Double) => Unit) = tensor1.foreachActiveElement((i1, v1) => tensor2.foreachActiveElement((i2, v2) => f(singleIndex(i1,i2), v1*v2)))
  def toTensor1: DenseTensor1 = new DenseTensor1(Array.tabulate(length)(apply))
}

object Outer2Tensor {
  var hasWarned = false
}

class UniformTensor2(val dim1:Int, val dim2:Int, val uniformValue:Double) extends Tensor2 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain = new RangeIntSeq(0, length)
  override def copy = new UniformTensor2(dim1, dim2, uniformValue)
  override def +(t:Tensor): Tensor = t match {
    case t:UniformTensor2 => { require(dim1 == t.dim1 && dim2 == t.dim2); new UniformTensor2(dim1, dim2, uniformValue + t.uniformValue) }
    case t:Tensor2 => { val t2 = new DenseTensor2(dim1, dim2, uniformValue); t2 += t; t2 }
  }
}


trait DenseLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq {
  def newTensor1:Int=>Tensor1
  private val _inners = new Array[Tensor1](dim1)
  override def zero() { _inners.foreach(i => if (i != null) i.zero()) }
  def activeDomain1 = { val a = new Array[Int](dim1); var i = 0; var j = 0; while (i < dim1) { if (_inners(i) ne null) { a(j) = i; j += 1 }; i += 1 }; new TruncatedArrayIntSeq(a, j) }
  def activeDomain2 = new RangeIntSeq(0, dim2) // This could perhaps be more sparse
  def activeDomain = { val b = new IntArrayBuffer; for (i <- 0 until dim1; j <- 0 until dim2) { if (apply(i,j) != 0.0) b += singleIndex(i,j) }; new ArrayIntSeq(b.toArray) } // Not very efficient; use _inner().activeDomain intead
  override def activeDomainSize = { _inners.filter(_ ne null).map(_.activeDomainSize).sum }
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = {
    for (i <- 0 until dim1; if _inners(i) ne null) _inners(i).foreachActiveElement((j,v) => f(i*dim2+j,v))
  }
  override def apply(i:Int, j:Int): Double = { val in = _inners(i); if (in ne null) in.apply(j) else 0.0 }
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  override def update(i: Int, d: Double) = update(i/dim2, i%dim2, d)
  def isDense = false
  override def update(i:Int, j:Int, v:Double): Unit = getInner(i).update(j, v)
  def update(i:Int, t:Tensor1): Unit = _inners(i) = t
  def inner(i:Int): Tensor1 = getInner(i)
  protected def getInner(i:Int): Tensor1 = { var in = _inners(i); if (in eq null) { in = newTensor1(dim2); _inners(i) = in }; in }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = {
    val len = _inners.length; var i = 0; while (i < len) { val in = _inners(i); if (in ne null) inner(i).=+(a, offset+i*dim1, f); i += 1 }
  }
  override def *(other: Tensor1): Tensor1 = {
    val out = new DenseTensor1(dim1)
    for (i <- 0 until dim1) {
      out(i) = inner(i) dot other
    }
    out
  }
  override def leftMultiply(other: Tensor1): Tensor1 = {
    val out = new DenseTensor1(dim2)
    other.foreachActiveElement((i, v) => if (_inners(i) ne null) out += (inner(i),v))
    out
  }
  override def +=(i:Int, incr:Double): Unit = getInner(index1(i)).+=(index2(i), incr)
  override def +=(ds:DoubleSeq, f:Double): Unit = ds match {
    case t:SingletonBinaryTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, f)
    case t:SingletonTensor2 => getInner(t.singleIndex1).+=(t.singleIndex2, f * t.singleValue)
    case t:SingletonLayeredTensorLike2 => { getInner(t.singleIndex1).+=(t.inner, f) }
    case t:SingletonBinaryLayeredTensorLike2 => { getInner(t.singleIndex1).+=(t.inner, f) }
    case t:DenseLayeredTensorLike2 => { val len = t._inners.length; var i = 0; while (i < len) { if (t._inners(i) ne null) getInner(i).+=(t._inners(i), f); i += 1 } }
    case t:Outer1Tensor2 => { val ff = f*t.scale; val t1 = t.tensor1; val l1 = t1.length; var i = 0; while (i < l1) { if (t1(i) != 0.0) { getInner(i).+=(t.tensor2, ff) }; i += 1 }}
    case t:TensorTimesScalar => this += (t.tensor, f * t.scalar)
    case t:DenseTensor => { val arr = t.asArray; var i = 0; while(i < arr.length) {this(i) += arr(i)*f ; i += 1}}
    case t:SparseIndexedTensor => { val len = t.activeDomainSize; val indices = t._indices; val values = t._values; var i = 0; while (i < len) { this(indices(i)) += values(i)*f ; i += 1}  }
    case t:Dense2LayeredTensor3 => { t.foreachActiveElement((i, v) => this(i) += v*f) }
    case t:DoubleSeq => throw new Error("Not yet implemented for class "+t.getClass.getName)
  }
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryLayeredTensorLike2 => { val inner = _inners(t.singleIndex1); if (inner ne null) inner.dot(t.inner) else 0.0 } // This is a common case, and should be fast
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    case t:SingletonLayeredTensorLike2 => { val inner = _inners(t.singleIndex1); if (inner ne null) inner.dot(t.inner) * t.singleValue1 else 0.0 }
    case t:DenseTensor => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
    case t:DenseLayeredTensorLike2 => { var s = 0.0; for((inner1,inner2) <- _inners zip t._inners; if inner1 ne null; if inner2 ne null) s += inner1.dot(inner2); s }
    case t:SparseIndexedTensor => {val len = t.activeDomainSize; val indices = t._indices; val values = t._values; var res = 0.0; var i = 0; while (i < len) { res += this(indices(i))*values(i) ; i += 1}; res}
    case t:Dense2LayeredTensor3 => { var sum = 0.0; t.foreachActiveElement((i, v) => sum += this(i)*v); sum}
    case t:Outer1Tensor2 => {
      (t.tensor1,t.tensor2) match {
        case (t1: DenseTensor, t2: DenseTensor) => var sum = 0.0; foreachActiveElement((i, v) => sum += t(i)*v); sum
        case (t1: DenseTensor, t2: SparseTensor) =>
          var sum = 0.0
          for (i <- activeDomain1) {
            sum += t1(i) * (inner(i) dot t2)
          }
          sum
        case (t1: SparseTensor, t2: Tensor) =>
          var sum = 0.0
          for ((i,v) <- t1.activeElements) {
            if (_inners(i) ne null) {
              sum += v * (inner(i) dot t2)
            }
          }
          sum
      }
    }
    case _ => throw new Error(t.getClass.getName + " doesn't have a match")
  }
}

class DenseLayeredTensor2(val dim1:Int, val dim2:Int, val newTensor1:Int=>Tensor1) extends DenseLayeredTensorLike2 {
  def this(dim1:Int, dim2:Int) = this(dim1, dim2, new SparseTensor1(_)) // TODO Keep methods like this, or avoid the magic of filling in the last argument?
  override def blankCopy: DenseLayeredTensor2 = new DenseLayeredTensor2(dim1, dim2, newTensor1)
  override def stringPrefix = "DenseLayeredTensor2"
}

// TODO Make a version of the above that uses an _innerValues: Array[Double] acting as (factor) weightsSet multiplying the values of the inner Tensor1's?

trait SingletonLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq with ReadOnlyTensor {
  def singleIndex1: Int
  def singleValue1: Double
  def inner: Tensor1
  def innerOffset: Int = singleIndex1 * dim2
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = inner.activeDomain1
  def activeDomain = { val offset = innerOffset; inner.activeDomain1.map((i: Int) => i + offset) }
  override def apply(i:Int, j:Int): Double = if (i == singleIndex1) inner(j) * singleValue1 else 0.0
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  override def foreachActiveElement(f: (Int, Double) => Unit): Unit = {
    val offset = innerOffset
    val value = singleValue1
    inner.foreachActiveElement((i, v) => f(offset + i, v * value))
  }
  override def update(i:Int, j:Int, v:Double): Unit = if (i == singleIndex1) inner.update(j, v/singleValue1) else throw new Error("Outer index out of bounds: "+i)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue * singleValue1
    case t:SingletonBinaryTensor2 => apply(t.singleIndex) * singleValue1
    case t:SingletonLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) * singleValue1 else 0.0
    case t:DenseLayeredTensorLike2 => t dot this
    case t:DenseTensorLike2 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
  }
}
class SingletonLayeredTensor2(val dim1:Int, val dim2:Int, val singleIndex1:Int, val singleValue1:Double, val inner:Tensor1) extends SingletonLayeredTensorLike2 {
  def activeDomainSize = inner.activeDomainSize
}

trait SingletonBinaryLayeredTensorLike2 extends Tensor2 with SparseDoubleSeq with ReadOnlyTensor {
  def singleIndex1: Int
  def inner: Tensor1
  def innerOffset: Int = singleIndex1 * dim2
  def isDense = false
  def activeDomain1 = new SingletonIntSeq(singleIndex1)
  def activeDomain2 = inner.activeDomain1
  def activeDomain = { val offset = innerOffset; inner.activeDomain1.map((i: Int) => i + offset) }
  override def apply(i:Int, j:Int): Double = if (i == singleIndex1) inner(j) else 0.0
  def apply(i:Int): Double = apply(i/dim2, i%dim2)
  override def update(i:Int, j:Int, v:Double): Unit = if (i == singleIndex1) inner.update(j, v) else throw new Error("Outer index out of bounds: "+i)
  override def foreachActiveElement(f: (Int, Double) => Unit): Unit = {
    val offset = innerOffset
    inner.foreachActiveElement((i, v) => f(offset + i, v))
  }
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor2 => apply(t.singleIndex)
    case t:SingletonTensor2 => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) else 0.0
    case t:SingletonLayeredTensorLike2 => if (singleIndex1 == t.singleIndex1) inner.dot(t.inner) * t.singleValue1 else 0.0
    case t:DenseLayeredTensorLike2 => t dot this
    case t:DenseTensorLike2 => { var s = 0.0; this.foreachActiveElement((i,v) => s += t(i)*v); s }
  }
}
class SingletonBinaryLayeredTensor2(val dim1:Int, val dim2:Int, var singleIndex1:Int, var inner:Tensor1) extends SingletonBinaryLayeredTensorLike2 {
  def activeDomainSize = inner.activeDomainSize
  override def copy = new SingletonBinaryLayeredTensor2(dim1, dim2, singleIndex1, inner)
}

