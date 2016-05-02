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
import cc.factorie.util.{IntSeq, SparseDoubleSeq, DoubleSeq, RangeIntSeq, DenseDoubleSeq, SingletonIntSeq, SeqIntSeq, DoubleSeqIterator}

trait Tensor1 extends Tensor {
  tensor1 =>
  def dim1: Int
  def activeDomain1: IntSeq = activeDomain
  def activeDomain: IntSeq
  def numDimensions: Int = 1
  def activeDomains = Array(activeDomain1)
  def dimensions = Array(dim1)
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:Tensor1 => t.dim1 == dim1
    case _ => false
  } 
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor1 => require(t.dim1 == dim1)
    case _ => throw new Error("Tensor ranks do not match.")
  }
  // FIXME: should "activeDomains" be a "def" there or a "val"?
  def reshape(dim: Array[Int]) : Tensor = {
    assert(dim.fold(1)((a,b) => a*b) == dim1)
    val self = this
    new Tensor with ReadOnlyTensor with SparseDoubleSeq {
      def foreachActiveElement(f: (Int, Double) => Unit) = self.foreachActiveElement(f)
      def activeDomainSize = self.activeDomainSize
      def dimensions = dim
      def activeDomain = tensor1.activeDomain
      def apply(i: Int) = tensor1(i)
      def length = tensor1.length
      def isDense = tensor1.isDense
      def numDimensions = dimensions.length
      def dot(s: DoubleSeq) = self.dot(s)
      def activeDomains = dimensions.map(d => new RangeIntSeq(0, d)).toArray
      def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
      def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
    }
  }
  override def *(f: Double): Tensor1 = super.*(f).asInstanceOf[Tensor1]
  override def /(f: Double): Tensor1 = super./(f).asInstanceOf[Tensor1]
  def +(t: Tensor1): Tensor1 = super.+(t).asInstanceOf[Tensor1]
  def -(t: Tensor1): Tensor1 = super.-(t).asInstanceOf[Tensor1]
  // TODO: * could be either dot or outer.  Resolve 1xN vs Nx1 status of Tensor1
  // I think it should mean * since that is consistent with matrix-vector being "*" -luke
  // def *(t: Tensor1): Double = this dot t
  //... or it could be Hadamard product
  def *(t: Tensor2): Tensor1 = t.leftMultiply(this)
  @inline final def length: Int = dim1
  override def copy: Tensor1 = throw new Error("Method copy not defined on class "+getClass.getName)
  override def blankCopy: Tensor1 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
  override def stringPrefix = "Tensor1"
}

object Tensor1 {
  def apply(values:Double*): DenseTensor1 = new DenseTensor1(values.toArray)
}

trait DenseTensorLike1 extends Tensor1 with DenseTensor {
  //def activeDomain = new RangeIntSeq(0, dim1)
  //override def activeDomain = activeDomain1
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
  // TODO I added it, but I'm not sure we should we have this method, or whether we should implement it in the Tensor trait.
  // See also comments about "def *" in Tensor1 above.  -akm
  /** Return the Hadamard product */ 
  def *(t:DenseTensor1): DenseTensor1 = {
    val result = this.copy // TODO We should arrange to get rid of this cast.
    val a = result.asArray
    val b = t.asArray
    val len = length; var i = 0; while (i < len) { a(i) *= b(i); i += 1 }
    result
  }
}
// TODO Consider something like the following for Scala 2.10:
// implicit class DenseTensor1(override val asArray:Array[Double]) extends DenseTensorLike1 {
//   _setArray(asArray)  
// }

class GrowableDenseTensor1(initialSize:Int) extends { private var _dim1 = initialSize } with DenseTensorLike1 {
  def dim1: Int = _dim1
  override def apply(index:Int):Double = if (index < _valuesSize) _values(index) else 0.0
  override def ensureDimensionsMatch(t:Tensor): Unit = t match {
    case t:Tensor1 => ensureDimensions(t.dim1)
    case _ => super.ensureDimensionsMatch(t)
  }
  def ensureDimensions(d1:Int): Unit = {
    if (d1 > _dim1) {
      if (d1 > _valuesSize) {
        val newSize = math.max(_valuesSize * 2, d1)
        val oldValues = _values
        _resetValues(newSize) // allocates a new array of size newSize
        Array.copy(oldValues, 0, _values, 0, oldValues.size)
        if (defaultValue != 0.0) java.util.Arrays.fill(_values, oldValues.size, newSize, defaultValue)
      }
      _dim1 = d1
    }
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
    case _ => {t.foreachActiveElement((i, d) => +=(i,d*f))}
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
class ScalarTensor(var singleValue:Double) extends Tensor1 with DenseDoubleSeq {
  def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
  def dim1 = 1
  def activeDomainSize = activeDomain.size
  def activeDomain = new SingletonIntSeq(0)
  def isDense = false
  def update(i: Int, v: Double) = if (i == 0) { singleValue = v} else throw new Error
  def dot(s: DoubleSeq) = if (s.length == 1) { singleValue*s(0)} else throw new Error
  def +=(i: Int, v: Double) = if (i == 0) { singleValue += v} else throw new Error
  def +=(t: Tensor, v: Double) = if (t.length == 1) { singleValue += v*t(0)} else throw new Error
  def zero() = singleValue = 0
  def apply(i:Int): Double = if (i == 0) singleValue else throw new Error
}

/** A one-dimensional one-hot Tensor. */
class SingletonTensor1(val dim1:Int, val singleIndex:Int, val singleValue:Double) extends SingletonIndexedTensor with Tensor1 {
  def activeDomain = new SingletonIntSeq(singleIndex)
} 

/** A one-dimensional one-hot Tensor with hot value 1.0. */
trait SingletonBinaryTensorLike1 extends Tensor1 with SingletonBinaryTensor {
  def activeDomain = new SingletonIntSeq(singleIndex)
}
/** A one-dimensional one-hot Tensor with hot value 1.0. */
class SingletonBinaryTensor1(val dim1:Int, var singleIndex:Int) extends SingletonBinaryTensorLike1 {
  override def copy: SingletonBinaryTensor1 = new SingletonBinaryTensor1(dim1, singleIndex)
}

/** A one-dimensional one-hot Tensor with hot value 1.0. */
class GrowableSingletonBinaryTensor1(val sizeProxy:Iterable[Any], var singleIndex:Int) extends SingletonBinaryTensorLike1 {
  def dim1 = sizeProxy.size
}

/** A Tensor1 of arbitrary fixed length whose value at all indices is uniformValue. */
class UniformTensor1(val dim1:Int, var uniformValue:Double) extends Tensor1 with UniformTensor {
  def activeDomain = new RangeIntSeq(0, dim1)
  override def copy = new UniformTensor1(dim1, uniformValue)
  override def +(t:Tensor): Tensor = t match {
    case t:UniformTensor1 => new UniformTensor1(dim1, uniformValue + t.uniformValue)
    case t:Tensor1 => new DenseTensor1(dim1, uniformValue) + t
  }
  override def *=(d: Double) = uniformValue *= d
}
/** A Tensor1 of arbitrary fixed length containing all 1.0. */
class UnaryTensor1(dim1:Int) extends UniformTensor1(dim1, 1.0) {
  override def copy = new UnaryTensor1(dim1)
}
/** A Tensor1 of mutable increasing length whose value at all indices is uniformValue. */
class GrowableUniformTensor1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends UniformTensor with Tensor1 {
  def activeDomain = new RangeIntSeq(0, dim1)
  //def activeDomain = activeDomain1
  def dim1 = sizeProxy.size
  override def copy = new GrowableUniformTensor1(sizeProxy, uniformValue)
}

trait SparseBinaryTensorLike1 extends Tensor1 with ArraySparseBinaryTensor { }

class SparseBinaryTensor1(val dim1:Int) extends SparseBinaryTensorLike1 {
  def this(t:Tensor) = { this(t.length); throw new Error("Not yet implemented.") }
  override def blankCopy: SparseBinaryTensor1 = new SparseBinaryTensor1(dim1)
  override def copy = {
    val newT = new SparseBinaryTensor1(dim1)
    this.foreachActiveElement((i, v) => newT(i) = v)
    newT
  }
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
class SparseTensor1(dim1:Int) extends SparseIndexedTensor1(dim1)
class GrowableSparseTensor1(sizeProxy:Iterable[Any]) extends GrowableSparseIndexedTensor1(sizeProxy)

trait SparseHashTensorLike extends Tensor with SparseDoubleSeq {
  self =>
  def isDense = false
  var default:Double = 0.0
  private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(index:Int) = self.default }
  def apply(index:Int) = h(index)
  override def update(index:Int, value:Double) = {
    assert(index < length, "index %d should be less than length %d".format(index, length))
    if(value == default) h.remove(index)
    else h(index) = value
  }
  override def activeElements = h.iterator
  override def activeDomainSize = h.size
  def activeDomain: IntSeq = new SeqIntSeq(h.keys.toIndexedSeq) // TODO This is currently really inefficient
  //def activeDomain = activeDomain1
  override def foreachActiveElement(f: (Int,Double)=>Unit): Unit = h.foreach(t => f(t._1, t._2))
  override def +=(index:Int, incr:Double): Unit = {
    assert(index < length, "index %d should be less than length %d".format(index, length))
    val newCt =  h(index) + incr
    if (newCt == 0.0)
      h.remove(index)
    else
      h(index) = newCt
  }
  override def zero(): Unit = h.clear()
  override def dot(v:DoubleSeq): Double = v match {
    case t:SparseBinaryTensor1 => t dot this
    case v:TensorTimesScalar => v dot this
    case v:SingletonBinaryTensor1 => v dot this
    case v:SingletonTensor1 => v dot this
    case sv:SparseHashTensorLike => {
      var result = 0.0
      if (v.size > this.size) activeElements.foreach({case(index,value) => result += sv(index) * value})
      else sv.activeElements.foreach({case(index,value) => result += h(index) * value})
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
}

/** A Tensor1 that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory,
    implemented as a HashMap from Int indices to Double values.
    @author Andrew McCallum */
class SparseHashTensor1(val dim1:Int) extends SparseHashTensorLike with Tensor1

/** Growable Version of SparseHashTensor
    @author Dirk Weissenborn */
class GrowableSparseHashTensor1(val sizeProxy:Iterable[Any]) extends SparseHashTensorLike with Tensor1 {
  def dim1 = sizeProxy.size
}

trait Tensor1ElementIterator extends DoubleSeqIterator with Iterator[Tensor1ElementIterator] {
  def index: Int
  def value: Double
}


class SparseIndexedTensor1(val dim1:Int) extends Tensor1 with ArraySparseIndexedTensor {
  def activeElements1: Tensor1ElementIterator = {
    _makeReadable()
    new Tensor1ElementIterator { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def value = _values(i-1)
      def next() = { i += 1; this }
    }
  }
  override def blankCopy: SparseIndexedTensor1 = new SparseIndexedTensor1(dim1)
  override def copy: SparseIndexedTensor1 = { val t = new SparseIndexedTensor1(dim1); this.copyInto(t); t }
}

class GrowableSparseIndexedTensor1(val sizeProxy:Iterable[Any]) extends Tensor1 with ArraySparseIndexedTensor {
  def dim1 = sizeProxy.size
  def activeElements1: Tensor1ElementIterator = {
    _makeReadable()
    new Tensor1ElementIterator { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _unsafeActiveDomainSize
      def index = _indices(i-1)
      def value = _values(i-1)
      def next() = { i += 1; this }
    }
  }
  override def blankCopy: GrowableSparseIndexedTensor1 = new GrowableSparseIndexedTensor1(sizeProxy)
  override def copy: GrowableSparseIndexedTensor1 = { val t = new GrowableSparseIndexedTensor1(sizeProxy); this.copyInto(t); t }
}


//// TODO Pull this out into SparseIndexedTensor
//class SparseIndexedTensor1b(len:Int) extends Tensor1 {
//  def this(sizeProxy:Iterable[Any]) = { this(-1); _sizeProxy = sizeProxy }
//  def isDense = false
//  private val _length: Int = len
//  private var _sizeProxy: Iterable[Any] = null
//  private var __values: Array[Double] = new Array[Double](4)
//  private var __indexs: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
//  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
//  private var _npos = 0 // the number of positions in _values and _indices that are actually being used
//  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
//  private def setCapacity(cap:Int): Unit = {
//    assert(cap >= _npos)
//    val newInd = new Array[Int](cap)
//    val newVal = new Array[Double](cap)
//    System.arraycopy(__indexs, 0, newInd, 0, _npos)
//    System.arraycopy(__values, 0, newVal, 0, _npos)
//    __indexs = newInd; __values = newVal
//  }
//  private def ensureCapacity(cap:Int): Unit = if (__indexs.length < cap) setCapacity(math.max(cap, __indexs.length + __indexs.length/2))
//  def _values = __values
//  def _indices = __indexs
//  def trim: Unit = setCapacity(_npos)
//  def dim1: Int = if (_length < 0) _sizeProxy.size else _length
//  override def activeDomainSize: Int = { makeReadable; _npos }
//  def activeDomain: IntSeq = { makeReadable ; new TruncatedArrayIntSeq(__indexs, _npos) } // TODO Consider making more efficient
//  //def activeDomain = activeDomain1
//  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { var i = 0; while (i < _npos) { f(__indexs(i), __values(i)); i += 1 } }
//  override def activeElements: Iterator[(Int,Double)] = {
//    makeReadable
//    new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
//      var i = 0
//      def hasNext = i < _npos
//      def next = { i += 1 ; (__indexs(i-1), __values(i-1)) }
//    }
//  }
//  override def zero(): Unit = _npos = 0
//  override def sum: Double = { var s = 0.0; var i = 0; while (i < _npos) { s += __values(i); i += 1 }; s }
//
//  /** Return the position at which index occurs, or -1 if index does not occur. */
//  def position(index:Int): Int = {
//    makeReadable
//    var i = 0; var ii = 0
//    while (i < _npos) { ii = __indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
//    //while (i < _npos) { if (_indexs(i) == index) return i; i += 1 }
//    -1
//  }
//  def position(index:Int, start:Int): Int = { // Just linear search for now; consider binary search with memory of last position
//    makeReadable
//    var i = start; var ii = 0
//    while (i < _npos) { ii = __indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
//    -1
//  }
//
//  def apply(index:Int): Double = {
//    // makeReadable is called in this.position
//    val pos = position(index)
//    if (pos < 0) 0.0 else __values(pos)
//  }
//
//  override def dot(v:DoubleSeq): Double = {
//    makeReadable
//    v match {
//      case v:SingletonBinaryTensor1 => apply(v.singleIndex)
//      case v:SingletonTensor1 => apply(v.singleIndex) * v.singleValue
//      case v:SparseIndexedTensor1b => {
//        val v1 = if (this._npos < v._npos) this else v
//        val v2 = if (v._npos< this._npos) v else this
//        var i = 0; var j = -1; var j2 = 0
//        var result = 0.0
//        while (i < v1._npos) {
//          j2 = v2.position(v1.__indexs(i), j+1)
//          if (j2 >= 0) { result += v1.__values(i) * v2.__values(j2); j = j2 }
//          i += 1
//        }
//        result
//      }
//      case v:DoubleSeq => { var result = 0.0; var p = 0; while (p < _npos) { result += v(__indexs(p)) * __values(p); p += 1 }; result }
//    }
//  }
//  
//  // Consider using bit shifting and only one array for this!
//  // How many bits are in the mantissa of a Double?  Enough to also keep the index?
//  
//  // Sort _indexs & _values between start and end; does not modify positions outside that range.
//  // Return the number of duplicate indices.  
//  @inline private def sort(start:Int, end:Int): Int = {
//    throw new Error("Not yet implemented")
//    var cp = start
//    while (cp < end) {
//      val ci = __indexs(cp)
//      val cv = __values(cp)
//      var i = cp - 1
//      while (i >= 0 && __indexs(i) >= ci) {
//        val tmpi = 
//        i -= 1
//      }
//    }
//    0
//  }
//  
//  override def toString = "SparseIndexedTensor1 npos="+_npos+" sorted="+_sorted+" ind="+__indexs.mkString(",")+" val="+__values.mkString(",")
//  
//  @inline private def makeReadable: Unit = {
//    var cp = _sorted // "current position", the position next to be placed into sorted order
//    while (cp < _npos) {
//      //println("cp="+cp)
//      val ci = __indexs(cp) // "current index", the index next to be placed into sorted order.
//      val cv = __values(cp) // "current value"
//      var i = _sorted - 1
//      //println("i="+i)
//      // Find the position at which the current index/value belongs
//      while (i >= 0 && __indexs(i) >= ci) i -= 1
//      i += 1
//      // Put it there, shifting to make room if necessary
//      //println("Placing at position "+i)
//      if (__indexs(i) == ci) { if (i != cp) __values(i) += cv else _sorted += 1 }
//      else insert(i, ci, cv, incrementNpos=false, incrementSorted=true)
//      //println("sorted="+_sorted)
//      cp += 1
//    }
//    _npos = _sorted
//    if (_npos * 1.5 > __values.length) trim
//  }
//  
//  // Caller is responsible for making sure there is enough capacity
//  @inline private def insert(position:Int, index:Int, value:Double, incrementNpos:Boolean, incrementSorted:Boolean): Unit = {
//    if (_npos - position > 0) {
//      System.arraycopy(__values, position, __values, position+1, _sorted-position)
//      System.arraycopy(__indexs, position, __indexs, position+1, _sorted-position)
//    }
//    __indexs(position) = index
//    __values(position) = value
//    if (incrementNpos) _npos += 1
//    if (incrementSorted) _sorted += 1
//  }
//
//  override def update(index:Int, value:Double): Unit = {
//    val p = position(index)
//    if (p >= 0) __values(p) = value
//    else +=(index, value) 
//  }
//  // Efficiently support multiple sequential additions
//  override def +=(index:Int, incr:Double): Unit = {
//    ensureCapacity(_npos+1)
//    __indexs(_npos) = index
//    __values(_npos) = incr
//    _npos += 1
//  }
//  
//  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
//  override def +=(t:DoubleSeq, f:Double): Unit = t match {
//    case t:SingletonBinaryTensorLike1 => +=(t.singleIndex, f)
//    case t:SingletonTensor1 => +=(t.singleIndex, f * t.singleValue)
//    case t:SparseBinaryTensorLike1 => { val a = t.asIntArray; val len = a.length; var i = 0; while (i < len) { +=(a(i), f); i += 1 }}
//    case t:SparseIndexedTensor1b => { val len = t._npos; var i = 0; while (i < len) { +=(t.__indexs(i), f * t.__values(i)); i += 1 }}
//    case t:DenseTensor1 => { val l = t.length; var i = 0; while (i < l) { val v = t(i); if (v != 0.0) +=(i, f * v); i += 1 }}
//    case t: TensorTimesScalar => this += (t.tensor, f * t.scalar)
//    case _ => super.+=(t, f)
//  }
//  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { var i = 0; while (i < _npos) { a(__indexs(i)+offset) += f * __values(i); i += 1 }}
//  
//  override def clone: SparseIndexedTensor1b = {
//    val v: SparseIndexedTensor1b = if (_sizeProxy eq null) new SparseIndexedTensor1b(_length) else new SparseIndexedTensor1b(_sizeProxy)
//    makeReadable
//    v._npos = _npos
//    v._sorted = _sorted
//    v.__values = __values.clone
//    v.__indexs = __indexs.clone
//    // TODO Deal with _positions
//    v
//  }
//
//}



