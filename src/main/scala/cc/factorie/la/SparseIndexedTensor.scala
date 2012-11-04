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

/** A sparse Tensor that stores an array of indices having non-zero values and an aligned sized array storing those values. */
trait SparseIndexedTensor extends Tensor {
  def isDense = false
  // In subclasses either _length should be set > 0 or _sizeProxy should be set non-null, but not both.
  //private var _length: Int = 0
  //private var _sizeProxy: Iterable[Any] = null
  private var __values: Array[Double] = new Array[Double](4)
  private var __indices: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
  private var __npos = 0 // the number of positions in _values and _indices that are actually being used
  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
  
  // TODO Avoid making these public?  But used in BP now. -akm
  def _values = __values
  def _indices = __indices
  def _npos = __npos
  private def setCapacity(cap:Int): Unit = {
    assert(cap >= __npos)
    val newInd = new Array[Int](cap)
    val newVal = new Array[Double](cap)
    System.arraycopy(__indices, 0, newInd, 0, __npos)
    System.arraycopy(__values, 0, newVal, 0, __npos)
    __indices = newInd; __values = newVal
  }
  private def ensureCapacity(cap:Int): Unit = if (__indices.length < cap) setCapacity(math.max(cap, __indices.length + __indices.length/2))
  def trim: Unit = setCapacity(__npos)
  
  // TODO There must already be functions somewhere that do this.
  private def copyarray(a:Array[Double]): Array[Double] = { if (a eq null) return null; val r = new Array[Double](a.length); System.arraycopy(a, 0, r, 0, a.length); r } 
  private def copyarray(a:Array[Int]): Array[Int] = { if (a eq null) return null; val r = new Array[Int](a.length); System.arraycopy(a, 0, r, 0, a.length); r } 
  def copyInto(t:SparseIndexedTensor): Unit = { t.__values = copyarray(__values); t.__indices = copyarray(__indices); t._positions = copyarray(_positions); t.__npos = __npos; t._sorted = _sorted }
  
  //def length: Int = if (_sizeProxy ne null) _sizeProxy.size else _length
  override def activeDomainSize: Int = { makeReadable; __npos }
  def activeDomain: IntSeq = { makeReadable ; new TruncatedArrayIntSeq(__indices, __npos) } // TODO Consider making more efficient
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { var i = 0; while (i < __npos) { f(__indices(i), __values(i)); i += 1 } }
  override def activeElements: Iterator[(Int,Double)] = {
    makeReadable
    new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < __npos
      def next = { i += 1 ; (__indices(i-1), __values(i-1)) }
    }
  }
  override def zero(): Unit = __npos = 0
  override def sum: Double = { var s = 0.0; var i = 0; while (i < __npos) { s += __values(i); i += 1 }; s }

  /** Return the position at which index occurs, or -1 if index does not occur. */
  def position(index:Int): Int = {
    makeReadable
    var i = 0; var ii = 0
    while (i < __npos) { ii = __indices(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    //while (i < _npos) { if (_indexs(i) == index) return i; i += 1 }
    -1
  }
  def position(index:Int, start:Int): Int = { // Just linear search for now; consider binary search with memory of last position
    makeReadable
    var i = start; var ii = 0
    while (i < __npos) { ii = __indices(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    -1
  }

  def apply(index:Int): Double = {
    // makeReadable is called in this.position
    val pos = position(index)
    if (pos < 0) 0.0 else __values(pos)
  }

  override def dot(v:DoubleSeq): Double = {
    makeReadable
    v match {
      case v:SingletonBinaryTensor1 => apply(v.singleIndex)
      case v:SingletonTensor1 => apply(v.singleIndex) * v.singleValue
      case v:SparseIndexedTensor => {
        val v1 = if (this.__npos < v.__npos) this else v
        val v2 = if (v.__npos< this.__npos) v else this
        var i = 0; var j = -1; var j2 = 0
        var result = 0.0
        while (i < v1.__npos) {
          j2 = v2.position(v1.__indices(i), j+1)
          if (j2 >= 0) { result += v1.__values(i) * v2.__values(j2); j = j2 }
          i += 1
        }
        result
      }
      case v:DoubleSeq => { var result = 0.0; var p = 0; while (p < __npos) { result += v(__indices(p)) * __values(p); p += 1 }; result }
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
      val ci = __indices(cp)
      val cv = __values(cp)
      var i = cp - 1
      while (i >= 0 && __indices(i) >= ci) {
        val tmpi = 
        i -= 1
      }
    }
    0
  }
  
  override def toString = "SparseIndexedTensor npos="+__npos+" sorted="+_sorted+" ind="+__indices.mkString(",")+" val="+__values.mkString(",")
  
  def _makeReadable: Unit = makeReadable
  @inline private def makeReadable: Unit = {
    var cp = _sorted // "current position", the position next to be placed into sorted order
    while (cp < __npos) {
      //println("cp="+cp)
      val ci = __indices(cp) // "current index", the index next to be placed into sorted order.
      val cv = __values(cp) // "current value"
      var i = _sorted - 1
      //println("i="+i)
      // Find the position at which the current index/value belongs
      while (i >= 0 && __indices(i) >= ci) i -= 1
      i += 1
      // Put it there, shifting to make room if necessary
      //println("Placing at position "+i)
      if (__indices(i) == ci) { if (i != cp) __values(i) += cv else _sorted += 1 }
      else insert(i, ci, cv, incrementNpos=false, incrementSorted=true)
      //println("sorted="+_sorted)
      cp += 1
    }
    __npos = _sorted
    if (__npos * 1.5 > __values.length) trim
  }
  
  // Caller is responsible for making sure there is enough capacity
  @inline private def insert(position:Int, index:Int, value:Double, incrementNpos:Boolean, incrementSorted:Boolean): Unit = {
    if (__npos - position > 0) {
      System.arraycopy(__values, position, __values, position+1, _sorted-position)
      System.arraycopy(__indices, position, __indices, position+1, _sorted-position)
    }
    __indices(position) = index
    __values(position) = value
    if (incrementNpos) __npos += 1
    if (incrementSorted) _sorted += 1
  }

  override def update(index:Int, value:Double): Unit = {
    val p = position(index)
    if (p >= 0) __values(p) = value
    else +=(index, value) 
  }
  // Efficiently support multiple sequential additions
  override def +=(index:Int, incr:Double): Unit = {
    ensureCapacity(__npos+1)
    __indices(__npos) = index
    __values(__npos) = incr
    __npos += 1
  }
  
  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensorLike1 => +=(t.singleIndex, f)
    case t:SingletonTensor1 => +=(t.singleIndex, f * t.singleValue)
    case t:SparseBinaryTensorLike1 => { val a = t.asIntArray; val len = a.length; var i = 0; while (i < len) { +=(a(i), f); i += 1 }}
    case t:SparseIndexedTensor => { val len = t.__npos; var i = 0; while (i < len) { +=(t.__indices(i), f * t.__values(i)); i += 1 }}
  }
  /** Increment Array "a" with the contents of this Tensor, but do so at "offset" into array and multiplied by factor "f". */
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { var i = 0; while (i < __npos) { a(__indices(i)+offset) += f * __values(i); i += 1 }}
  
  override def expNormalize(): Double = {
    var max = Double.MinValue
    var i = 0; 
    while (i < __npos) { if (max < __values(i)) max = __values(i); i += 1 }
    var sum = 0.0
    i = 0
    while (i < __npos) {
      val e = math.exp(__values(i) - max)  //update(i, math.exp(apply(i) - max))
      __values(i) = e
      sum += e
      i += 1
    }
    i = 0
    while (i < __npos) {
      __values(i) /= sum
      i += 1
    }
    sum
  }

  
  // TODO Use copyInto instead?
  def cloneFrom(t:SparseIndexedTensor): Unit = {
    makeReadable
    //t._length = _length
    //t._sizeProxy = _sizeProxy
    t.__npos = __npos
    t._sorted = _sorted
    t.__values = __values.clone
    t.__indices = __indices.clone
    // TODO Deal with _positions, once is it implemented
  }
}

