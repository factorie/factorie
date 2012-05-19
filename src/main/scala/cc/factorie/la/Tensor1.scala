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

trait Tensor1 extends Tensor {
  def dim1: Int
  def activeDomain1: IntSeq
  def activeDomain: IntSeq = activeDomain1
  def numDimensions: Int = 1
  def activeDomains = Array(activeDomain1)
  def dimensions = Array(dim1)
  @inline final def length: Int = dim1
  override def stringPrefix = "Tensor1"
}

trait DenseTensorLike1 extends Tensor1 with DenseTensor {
  private var __values = new Array[Double](dim1)
  protected def _values = __values
  protected def _valuesSize: Int = __values.size
  // Used by subclass GrowableDenseTensor1
  protected def ensureCapacity(size:Int): Unit = if (__values.size < size) {
    val newSize = math.max(__values.size * 2, size)
    val newCounts = new Array[Double](newSize)
    Array.copy(_values, 0, newCounts, 0, __values.size)
    __values = newCounts
  }
  protected def _setArray(a:Array[Double]): Unit = { assert(a.length == dim1); __values = a }
  def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  //def activeDomain = activeDomain1
  def apply(i:Int) = __values(i)
  override def asArray = __values
  override def +=(i:Int, incr:Double): Unit = __values(i) += incr
  override def :=(ds:DoubleSeq): Unit = ds match {
    case ds:DenseTensorLike1 => System.arraycopy(__values, 0, ds.__values, 0, length)
    case ds:DoubleSeq => super.:=(ds)
  }
//  override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); ds match {
//    case t:Tensor => 
//      if (t.isDense) { var i = 0; while (i < length) { __values(i) += ds(i); i += 1 } } 
//    case ds:DoubleSeq => { var i = 0; while (i < length) { __values(i) += ds(i); i += 1 } } 
//  }
  override def zero(): Unit = java.util.Arrays.fill(__values, 0.0)
  override def update(i:Int, v:Double): Unit = __values(i) = v
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonTensor => apply(t.singleIndex) * t.singleValue
    case t:SingletonBinaryTensor => apply(t.singleIndex)
    case t:DenseTensorLike1 => Tensor.dot(this, t)
    case t:SparseBinaryTensorLike1 => t dot this
    case t:SparseIndexedTensor1 => t dot this
    case t:UniformTensor => sum * t.uniformValue
  }
}
class DenseTensor1(val dim1:Int) extends DenseTensorLike1 {
  def this(t:DoubleSeq) = { this(t.length); this := t }
  def this(a:Array[Double]) = { this(a.length); this := a }
  override def copy: DenseTensor1 = { val c = new DenseTensor1(dim1); System.arraycopy(_values, 0, c._values, 0, length); c }
  override def blankCopy: DenseTensor1 = new DenseTensor1(dim1)
}
// TODO Consider something like the following for Scala 2.10:
// implicit class DenseTensor1(override val asArray:Array[Double]) extends DenseTensorLike1 {
//   _setArray(asArray)  
// }

trait GrowableDenseTensorLike1 extends DenseTensorLike1 {
  def sizeProxy: Iterable[Any]
  private var _size: Int = 0
  def dim1: Int = math.max(_size, sizeProxy.size)
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  // This is currently the only method that supports capacity expansion
  override def +=(index:Int, incr:Double): Unit = {
    ensureCapacity(index+1)
    if (index >= _size) { _size = index + 1 }
    super.+=(index, incr)
  }
}
class GrowableDenseTensor1(val sizeProxy:Iterable[Any]) extends DenseTensorLike1 with GrowableDenseTensorLike1 {
  override def copy: GrowableDenseTensor1 = { val c = new GrowableDenseTensor1(sizeProxy); c := this; c }
  override def blankCopy: GrowableDenseTensor1 = new GrowableDenseTensor1(sizeProxy)
}


class SingletonTensor1(val dim1:Int, val singleIndex:Int, val singleValue:Double) extends Tensor1 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor1 => if (singleIndex == t.singleIndex) singleValue else 0.0
    case t:SingletonTensor1 => if (singleIndex == t.singleIndex) singleValue * t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex) * singleValue
  }
} 

trait SingletonBinaryTensorLike1 extends Tensor1 with SingletonBinaryTensor {
  def singleIndex: Int
  def activeDomain1 = new SingletonIntSeq(singleIndex)
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor1 => if (singleIndex == t.singleIndex) 1.0 else 0.0
    case t:SingletonTensor1 => if (singleIndex == t.singleIndex) 1.0 * t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex) * 1.0
  }
}
class SingletonBinaryTensor1(val dim1:Int, val singleIndex:Int) extends SingletonBinaryTensorLike1 {
  override def copy: SingletonBinaryTensor1 = new SingletonBinaryTensor1(dim1, singleIndex)
}
class GrowableSingletonBinaryTensor1(val sizeProxy:Iterable[Any], val singleIndex:Int) extends SingletonBinaryTensorLike1 {
  def dim1 = sizeProxy.size
}

class UniformTensor1(val dim1:Int, val uniformValue:Double) extends Tensor1 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
}
class GrowableUniformTensor1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends Tensor1 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  //def activeDomain = activeDomain1
  def dim1 = sizeProxy.size
}



trait SparseBinaryTensorLike1 extends cc.factorie.util.ProtectedIntArrayBuffer with Tensor1 {
  def activeDomain1 = new ArrayIntSeq(_array)
  //def activeDomain = activeDomain1
  def isDense = false
  @inline final def apply(index:Int): Double = if (_indexOfSorted(index) >= 0) 1.0 else 0.0
  @inline final def contains(index:Int): Boolean = _containsSorted(index)
  override def sum: Double = _length.toDouble
  override def max: Double = if (_length > 0) 1.0 else 0.0
  override def min: Double = if (_length == 0) 0.0 else 1.0
  override def indexOf(d:Double): Int = if (d != 0.0 && d != 1.0) -1 else if (d == 1.0) { if (_length == 0) -1 else _apply(0) } else { if (_length == 0) 0 else throw new Error("Not yet implemented") }
  override def maxIndex: Int = if (_length == 0) 0 else _apply(0)
  override def containsNaN: Boolean = false
  def +=(i:Int): Unit = _insertSorted(i)
  def -=(i:Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSorted(is(j)); j += 1} }
  def ++=(is:Iterable[Int]): Unit = { _ensureCapacity(_length + is.size); is.foreach(_insertSorted(_)) }

  override def update(i:Int, v:Double): Unit = {
    if (i < 0 || i >= length) throw new Error("Tensor index out of range: "+i)
    if (v == 1.0) this += i else if (v == 0.0) this -= i else throw new Error(getClass.getName+" cannot update with values other than 0.0 or 1.0.")
  }
  /** In SparseBinary, this is equivalent to update(i,v) */
  override def +=(i:Int, v:Double): Unit = update(i, v)
  override def zero(): Unit = _clear() // TODO I think _clear should be renamed _zero -akm
  override def dot(v:DoubleSeq): Double = v match {
    case t:SingletonBinaryTensor1 => if (contains(t.singleIndex)) 1.0 else 0.0
    case t:SingletonTensor1 => if (contains(t.singleIndex)) t.singleValue else 0.0
    // TODO Any other special cases here?
    case ds:DoubleSeq => { var result = 0.0; var i = 0; while (i < _length) { result += ds(_apply(i)); i += 1 }; result }
  }
}
class SparseBinaryTensor1(val dim1:Int) extends SparseBinaryTensorLike1 {
  def this(t:Tensor) = { this(t.length); throw new Error("Not yet implemented.") }
  override def blankCopy: SparseBinaryTensor1 = new SparseBinaryTensor1(dim1)

}
class GrowableSparseBinaryTensor1(val sizeProxy:Iterable[Any]) extends SparseBinaryTensorLike1 {
  def dim1: Int = sizeProxy.size
  override def blankCopy: GrowableSparseBinaryTensor1 = new GrowableSparseBinaryTensor1(sizeProxy)
}

// Just aliases
class SparseTensor1(dim1:Int) extends SparseIndexedTensor1(dim1) {
  override def blankCopy: SparseTensor1 = new SparseTensor1(dim1)
}
class GrowableSparseTensor1(val sizeProxy:Iterable[Any]) extends SparseIndexedTensor1(sizeProxy) {
  override def blankCopy: GrowableSparseTensor1 = new GrowableSparseTensor1(sizeProxy)
}

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory,
    implemented as a HashMap from Int indices to Double values.
    @author Andrew McCallum */
class SparseHashTensor1(val dim1:Int) extends Tensor1 {
  def isDense = false
  var default = 0.0
  private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(index:Int) = SparseHashTensor1.this.default }
  def apply(index:Int) = h(index)
  override def update(index:Int, value:Double) = {
    assert(index < length, "index %d should be less than length %d".format(index, length))
    if(value == default) h.remove(index)
    else h(index) = value
  }
  override def activeElements = h.iterator
  override def activeDomainSize = h.size
  def activeDomain1: IntSeq = new SeqIntSeq(h.keys.toIndexedSeq) // TODO This is currently really inefficient
  //def activeDomain = activeDomain1
  override def foreachActiveElement(f: (Int,Double)=>Unit): Unit = h.foreach(t => f(t._1, t._2))
  override def +=(index:Int, incr:Double): Unit = {
    assert(index < length, "index %d should be less than length %d".format(index, length))
    h(index) = h(index) + incr
  }
  override def zero(): Unit = h.clear()
  override def dot(v:DoubleSeq): Double = v match {
    case t:SparseBinaryTensor1 => t dot this
    case v:TensorTimesScalar => v dot this
    case v:SingletonBinaryTensor1 => v dot this
    case v:SingletonTensor1 => v dot this
    case sv:SparseHashTensor1 => {
      var result = 0.0
      if (v.size > this.size) h.iterator.foreach({case(index,value) => result += sv(index) * value})
      else sv.h.iterator.foreach({case(index,value) => result += h(index) * value})
      result
    }
    case dv:DoubleSeq => {
      var result = 0.0
      h.iterator.foreach({case(index,value) => result += dv(index) * value})
      result
    }
  }
  def +=(v:Tensor1): Unit = v.foreachActiveElement({case(index,value) => +=(index, value)}) //h.update(index, h(index) + value)})
  override def +=(s:Double): Unit = {
    default += s
    h.keys.foreach(index => +=(index, s)) //h.update(index, h(index) + s))
  }

  override def toString = getClass.getName + "(" + "len=" + length + " (" + h.mkString("[", ", ", "]") + "))"
}

class SparseIndexedTensor1(len:Int) extends Tensor1 {
  def this(sizeProxy:Iterable[Any]) = { this(-1); _sizeProxy = sizeProxy }
  def isDense = false
  private val _length: Int = len
  private var _sizeProxy: Iterable[Any] = null
  private var _values: Array[Double] = new Array[Double](4)
  private var _indexs: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
  private var _npos = 0 // the number of positions in _values and _indices that are actually being used
  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
  private def setCapacity(cap:Int): Unit = {
    assert(cap >= _npos)
    val newInd = new Array[Int](cap)
    val newVal = new Array[Double](cap)
    System.arraycopy(_indexs, 0, newInd, 0, _npos)
    System.arraycopy(_values, 0, newVal, 0, _npos)
    _indexs = newInd; _values = newVal
  }
  private def ensureCapacity(cap:Int): Unit = if (_indexs.length < cap) setCapacity(math.max(cap, _indexs.length + _indexs.length/2))
  def trim: Unit = setCapacity(_npos)
  
  def dim1: Int = if (_length < 0) _sizeProxy.size else _length
  override def activeDomainSize: Int = { makeReadable; _npos }
  def activeDomain1: IntSeq = { makeReadable ; new TruncatedArrayIntSeq(_indexs, _npos) } // TODO Consider making more efficient
  //def activeDomain = activeDomain1
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { var i = 0; while (i < _npos) { f(_indexs(i), _values(i)); i += 1 } }
  override def activeElements: Iterator[(Int,Double)] = {
    makeReadable
    new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _npos
      def next = { i += 1 ; (_indexs(i-1), _values(i-1)) }
    }
  }
  override def zero(): Unit = _npos = 0

  /** Return the position at which index occurs, or -1 if index does not occur. */
  def position(index:Int): Int = {
    makeReadable
    var i = 0; var ii = 0
    while (i < _npos) { ii = _indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    //while (i < _npos) { if (_indexs(i) == index) return i; i += 1 }
    -1
  }
  def position(index:Int, start:Int): Int = { // Just linear search for now; consider binary search with memory of last position
    makeReadable
    var i = start; var ii = 0
    while (i < _npos) { ii = _indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    -1
  }

  def apply(index:Int): Double = {
    // makeReadable is called in this.position
    val pos = position(index)
    if (pos < 0) 0.0 else _values(pos)
  }

  override def dot(v:DoubleSeq): Double = {
    makeReadable
    v match {
      case v:SingletonBinaryTensor1 => apply(v.singleIndex)
      case v:SingletonTensor1 => apply(v.singleIndex) * v.singleValue
      case v:SparseIndexedTensor1 => {
        val v1 = if (this._npos < v._npos) this else v
        val v2 = if (v._npos< this._npos) v else this
        var i = 0; var j = -1; var j2 = 0
        var result = 0.0
        while (i < v1._npos) {
          j2 = v2.position(v1._indexs(i), j+1)
          if (j2 >= 0) { result += v1._values(i) * v2._values(j2); j = j2 }
          i += 1
        }
        result
      }
      case v:DoubleSeq => { var result = 0.0; var p = 0; while (p < _npos) { result += v(_indexs(p)) * _values(p); p += 1 }; result }
    }
  }
  
  // Consider using bit shifting and only one array for this!
  // How many bits are in the mantissa of a Double?  Enough to also keep the index?
  
  // Sort _indexs & _values between start and end; does not modify positions outside that range.
  // Return the number of duplicate indices.  
  @inline private def sort(start:Int, end:Int): Int = {
    throw new Error("Not yet implemented")
    var cp = start
    while (cp < end) {
      val ci = _indexs(cp)
      val cv = _values(cp)
      var i = cp - 1
      while (i >= 0 && _indexs(i) >= ci) {
        val tmpi = 
        i -= 1
      }
    }
    0
  }
  
  override def toString = "SparseIndexedTensor1 npos="+_npos+" sorted="+_sorted+" ind="+_indexs.mkString(",")+" val="+_values.mkString(",")
  
  @inline private def makeReadable: Unit = {
    var cp = _sorted // "current position", the position next to be placed into sorted order
    while (cp < _npos) {
      //println("cp="+cp)
      val ci = _indexs(cp) // "current index", the index next to be placed into sorted order.
      val cv = _values(cp) // "current value"
      var i = _sorted - 1
      //println("i="+i)
      // Find the position at which the current index/value belongs
      while (i >= 0 && _indexs(i) >= ci) i -= 1
      i += 1
      // Put it there, shifting to make room if necessary
      //println("Placing at position "+i)
      if (_indexs(i) == ci) { if (i != cp) _values(i) += cv else _sorted += 1 }
      else insert(i, ci, cv, incrementNpos=false, incrementSorted=true)
      //println("sorted="+_sorted)
      cp += 1
    }
    _npos = _sorted
    if (_npos * 1.5 > _values.length) trim
  }
  
  // Caller is responsible for making sure there is enough capacity
  @inline private def insert(position:Int, index:Int, value:Double, incrementNpos:Boolean, incrementSorted:Boolean): Unit = {
    if (_npos - position > 0) {
      System.arraycopy(_values, position, _values, position+1, _sorted-position)
      System.arraycopy(_indexs, position, _indexs, position+1, _sorted-position)
    }
    _indexs(position) = index
    _values(position) = value
    if (incrementNpos) _npos += 1
    if (incrementSorted) _sorted += 1
  }

  override def update(index:Int, value:Double): Unit = {
    val p = position(index)
    if (p >= 0) _values(p) = value
    else +=(index, value) 
  }
  // Efficiently support multiple sequential additions
  override def +=(index:Int, incr:Double): Unit = {
    ensureCapacity(_npos+1)
    _indexs(_npos) = index
    _values(_npos) = incr
    _npos += 1
  }
  
  def +=(v:Tensor1): Unit = v.foreachActiveElement((i,v) => +=(i, v)) 
  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  
  override def clone: SparseIndexedTensor1 = {
    val v: SparseIndexedTensor1 = if (_sizeProxy eq null) new SparseIndexedTensor1(_length) else new SparseIndexedTensor1(_sizeProxy)
    makeReadable
    v._npos = _npos
    v._sorted = _sorted
    v._values = _values.clone
    v._indexs = _indexs.clone
    // TODO Deal with _positions
    v
  }

}



