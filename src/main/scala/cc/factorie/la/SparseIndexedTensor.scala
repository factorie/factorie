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

trait SparseIndexedTensor extends Tensor {
  def isDense = false
  // In subclasses either _length should be set > 0 or _sizeProxy should be set non-null, but not both.
  private var _length: Int = 0
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
  
  def length: Int = if (_length < 0) _sizeProxy.size else _length
  override def activeDomainSize: Int = { makeReadable; _npos }
  def activeDomain: IntSeq = { makeReadable ; new TruncatedArrayIntSeq(_indexs, _npos) } // TODO Consider making more efficient
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
      case v:SparseIndexedTensor => {
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
  
  override def toString = "SparseIndexedTensor npos="+_npos+" sorted="+_sorted+" ind="+_indexs.mkString(",")+" val="+_values.mkString(",")
  
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
    case t:SparseIndexedTensor => { val len = t._npos; var i = 0; while (i < len) { +=(t._indexs(i), f * t._values(i)); i += 1 }}
  }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { var i = 0; while (i < _npos) { a(_indexs(i)+offset) += f * _values(i); i += 1 }}
  
  def cloneFrom(t:SparseIndexedTensor): Unit = {
    makeReadable
    t._length = _length
    t._sizeProxy = _sizeProxy
    t._npos = _npos
    t._sorted = _sorted
    t._values = _values.clone
    t._indexs = _indexs.clone
    // TODO Deal with _positions, once is it implemented
  }
}

