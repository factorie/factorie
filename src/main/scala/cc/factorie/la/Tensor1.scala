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
  tensor1 =>
  def dim1: Int
  def activeDomain1: IntSeq
  def activeDomain: IntSeq = activeDomain1
  def numDimensions: Int = 1
  def activeDomains = Array(activeDomain1)
  override def sparseCopy: Tensor1 = { new SparseTensor1(dim1) }
  def dimensions = Array(dim1)
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:Tensor1 => t.dim1 == dim1
    case _ => false
  } 
  def ensureDimenionsMatch(t:Tensor): Unit = t match {
    case t:Tensor1 => require(t.dim1 == dim1)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  // FIXME: should "activeDomains" be a "def" there or a "val"?
  def reshape(dim: Array[Int]) : Tensor = {
    assert(dim.fold(1)((a,b) => a*b) == dim1)
    new Tensor {
      def dimensions = dim
      def activeDomain = tensor1.activeDomain
      def apply(i: Int) = tensor1(i)
      def length = tensor1.length
      def isDense = tensor1.isDense
      def numDimensions = dimensions.length
      def activeDomains = dimensions.map(d => new RangeIntSeq(0, d)).toArray
    }
  }
  @inline final def length: Int = dim1
  override def copy: Tensor1 = throw new Error("Method copy not defined on class "+getClass.getName)
  override def stringPrefix = "Tensor1"
}

object Tensor1 {
  def apply(values:Double*): DenseTensor1 = new DenseTensor1(values.toArray)
}

trait DenseTensorLike1 extends Tensor1 with DenseTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  override def activeDomain = activeDomain1
  override def dot(t:DoubleSeq): Double = t match {
    //case t:SingletonBinaryTensor => apply(t.singleIndex)
    //case t:SingletonTensor => apply(t.singleIndex) * t.singleValue
    //case t:DenseTensorLike1 => Tensor.dot(this, t)
    case t:SparseBinaryTensorLike1 => t dot this
    case t:SparseIndexedTensor1 => t dot this
    case t:DoubleSeq => super.dot(t)
  }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    //case t:SingletonBinaryTensorLike1 => __values(t.singleIndex) += 1.0
    //case t:SingletonTensor1 => __values(t.singleIndex) += t.singleValue
    //case t:SparseBinaryTensorLike1 => t.=+(_values, f)
    case t:SparseIndexedTensor1 => t.=+(_values, f)
    case t:DoubleSeq => super.+=(t, f)
  }
}
class DenseTensor1(val dim1:Int) extends DenseTensorLike1 {
  def this(t:DoubleSeq) = { this(t.length); this := t }
  def this(a:Array[Double]) = { this(a.length); this := a }
  def this(dim1:Int, fillValue:Double) = { this(dim1); java.util.Arrays.fill(_values, fillValue) }
  override def copy: DenseTensor1 = { val c = new DenseTensor1(dim1); System.arraycopy(_values, 0, c._values, 0, length); c }
  override def blankCopy: DenseTensor1 = new DenseTensor1(dim1)
}
// TODO Consider something like the following for Scala 2.10:
// implicit class DenseTensor1(override val asArray:Array[Double]) extends DenseTensorLike1 {
//   _setArray(asArray)  
// }

class GrowableDenseTensor1(initialSize:Int) extends { private var _dim1 = initialSize } with DenseTensorLike1 {
  def dim1: Int = _dim1
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  override def ensureDimenionsMatch(t:Tensor): Unit = t match {
    case t:Tensor1 => ensureDimensions(t.dim1)
    case _ => super.ensureDimenionsMatch(t)
  }
  def ensureDimensions(d1:Int): Unit = if (d1 > _dim1) {
    val newSize = d1 //math.max(_valuesSize * 2, d1)
    _dim1 = d1
    val oldValues = _values
    _resetValues(newSize) // allocates a new array of size newSize
    Array.copy(oldValues, 0, _values, 0, oldValues.size)
    if (defaultValue != 0.0) java.util.Arrays.fill(_values, oldValues.size, newSize, defaultValue)
  }
  // Currently these two are the only methods that support capacity expansion
  override def +=(index:Int, incr:Double): Unit = {
    ensureDimensions(index+1)
    super.+=(index, incr)
  }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor1 => +=(t.singleIndex, f)
    case t:SingletonTensor1 => +=(t.singleIndex, f * t.singleValue)
    case t:SparseBinaryTensorLike1 => { ensureDimensions(t.maxIndex+1); t.=+(_values, f) }
    case t:DenseTensorLike1 => { ensureDimensions(t.length); super.+=(t, f) }
    case t:SparseIndexedTensor1 => { ensureDimensions(t.length); super.+=(t, f) }
    case t:UniformTensor1 => { ensureDimensions(t.length); super.+=(t, f) }  //val len = length; val u = t.uniformValue * f; var i = 0; while (i < len) { __values(i) += u; i += 1 }
  }
  override def copy: GrowableDenseTensor1 = { val c = new GrowableDenseTensor1(_dim1); c := this; c }
  override def blankCopy: GrowableDenseTensor1 = new GrowableDenseTensor1(_dim1)
}

class ProxyGrowableDenseTensor1(val sizeProxy:Iterable[Any]) extends GrowableDenseTensor1(sizeProxy.size) {
  override def dim1 = math.max(super.dim1, sizeProxy.size)
  override def copy: ProxyGrowableDenseTensor1 = { val c = new ProxyGrowableDenseTensor1(sizeProxy); c := this; c }
  override def blankCopy: GrowableDenseTensor1 = new ProxyGrowableDenseTensor1(sizeProxy)
}

/** A Tensor representation of a single scalar (Double) value */
// TODO In Scala 2.10 this could be an implicit class
class ScalarTensor(var singleValue:Double) extends Tensor1 {
  def dim1 = 1
  def activeDomain1 = new SingletonIntSeq(0)
  def isDense = false
  def apply(i:Int): Double = if (i == 0) singleValue else throw new Error
}

/** A one-dimensional one-hot Tensor. */
class SingletonTensor1(val dim1:Int, val singleIndex:Int, val singleValue:Double) extends Tensor1 with SingletonTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex)
} 

/** A one-dimensional one-hot Tensor with hot value 1.0. */
trait SingletonBinaryTensorLike1 extends Tensor1 with SingletonBinaryTensor {
  def activeDomain1 = new SingletonIntSeq(singleIndex)
}
/** A one-dimensional one-hot Tensor with hot value 1.0. */
class SingletonBinaryTensor1(val dim1:Int, var singleIndex:Int) extends SingletonBinaryTensorLike1 {
  override def copy: SingletonBinaryTensor1 = new SingletonBinaryTensor1(dim1, singleIndex)
}

/** A one-dimensional one-hot Tensor with hot value 1.0. */
class GrowableSingletonBinaryTensor1(val sizeProxy:Iterable[Any], var singleIndex:Int) extends SingletonBinaryTensorLike1 {
  def dim1 = sizeProxy.size
}

class UniformTensor1(val dim1:Int, val uniformValue:Double) extends Tensor1 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  override def copy = new UniformTensor1(dim1, uniformValue)
  override def +(t:Tensor): Tensor = t match {
    case t:UniformTensor1 => new UniformTensor1(dim1, uniformValue + t.uniformValue)
    case t:Tensor1 => new DenseTensor1(dim1, uniformValue) + t
  }
}
class UnaryTensor1(dim1:Int) extends UniformTensor1(dim1, 1.0) {
  override def copy = new UnaryTensor1(dim1)
}
class GrowableUniformTensor1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends Tensor1 with UniformTensor {
  def activeDomain1 = new RangeIntSeq(0, dim1)
  //def activeDomain = activeDomain1
  def dim1 = sizeProxy.size
  override def copy = new GrowableUniformTensor1(sizeProxy, uniformValue)
}


// TODO Use SparseBinaryTensor here
trait SparseBinaryTensorLike1 extends cc.factorie.util.ProtectedIntArrayBuffer with Tensor1 {
  def activeDomain1 = new TruncatedArrayIntSeq(_array, _length)
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { var i = 0; while (i < _length) { f(_array(i), 1.0); i += 1 } }
  override def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
    var i = 0
    def hasNext = i < _length
    def next = { i += 1 ; (_array(i-1), 1.0) }
  }
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
  def +=(i:Int): Unit = _insertSortedNoDuplicates(i)
  //override def =+(a:Array[Double]): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)) += 1.0; i += 1 } }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)+offset) += f; i += 1 } }
  def -=(i:Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSortedNoDuplicates(is(j)); j += 1} }
  def ++=(is:IntSeq): Unit = ++=(is.asArray)
  def ++=(is:Iterable[Int]): Unit = { _ensureCapacity(_length + is.size); is.foreach(_insertSortedNoDuplicates(_)) }
  def toIntArray: Array[Int] = _toArray
  def asIntArray: Array[Int] = _asArray
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
  override def copy = {
    val newT = new GrowableSparseBinaryTensor1(sizeProxy)
    this.foreachActiveElement((i, v) => newT(i) = v)
    newT
  }
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
  //override def toString = getClass.getName + "(" + "len=" + length + " (" + h.mkString("[", ", ", "]") + "))"
}

// Pull this out into SparseIndexedTensor
class SparseIndexedTensor1(len:Int) extends Tensor1 {
  def this(sizeProxy:Iterable[Any]) = { this(-1); _sizeProxy = sizeProxy }
  def isDense = false
  private val _length: Int = len
  private var _sizeProxy: Iterable[Any] = null
  var _values: Array[Double] = new Array[Double](4)
  var _indexs: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
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
  override def sum: Double = { var s = 0.0; var i = 0; while (i < _npos) { s += _values(i); i += 1 }; s }

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
  
  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensorLike1 => +=(t.singleIndex, f)
    case t:SingletonTensor1 => +=(t.singleIndex, f * t.singleValue)
    case t:SparseBinaryTensorLike1 => { val a = t.asIntArray; val len = a.length; var i = 0; while (i < len) { +=(a(i), f); i += 1 }}
    case t:SparseIndexedTensor1 => { val len = t._npos; var i = 0; while (i < len) { +=(t._indexs(i), f * t._values(i)); i += 1 }}
  }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { var i = 0; while (i < _npos) { a(_indexs(i)+offset) += f * _values(i); i += 1 }}
  
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



