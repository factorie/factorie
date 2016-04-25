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
package cc.factorie.util
import scala.collection.{IndexedSeq, Seq}
import scala.compat.Platform.arraycopy

/** Embeds an efficient array of Ints, with convenient _append, _prepend, etc. */
trait ProtectedIntArrayBuffer { 
  protected def _initialCapacity = 4
  private var _arr = new Array[Int](_initialCapacity)
  private var _size = 0
  private var _lastIndex: Int = 0
  /** Reallocate the array to be exactly 'cap' or _initialCapacity, whichever is bigger. */
  @inline final protected def _setCapacity(cap:Int): Unit = {
    if (_arr.length != cap) {
      require(cap >= _size && cap >= 0)
      val newCapacity = if (cap < _initialCapacity) _initialCapacity else cap
      val newArray = new Array[Int](newCapacity)
      if (_size > 0) arraycopy(_arr, 0, newArray, 0, _size)
      _arr = newArray
    }
  }
  protected def _capacityGrowthFactor: Double = 1.5
  @inline final protected def _ensureCapacity(cap:Int): Unit = 
    if (cap > _arr.length) _setCapacity(math.max(cap, (_arr.length * _capacityGrowthFactor).toInt))
  /** If _size is sufficiently smaller than the array length so that substantial space would be saved, then
      reallocate the array length to be max(_size,_initialCapacity), and return true.  Otherwise return false. */
  protected def _considerShrinkingCapacity(): Boolean = if (_size > _initialCapacity && _arr.length > _size * 2) { _setCapacity(_size); true } else false
  protected def _trimCapacity(): Unit = _setCapacity(_size) // What if _size == 0?
  protected def _reduceToSize(newSize:Int): Unit = { _size = newSize; if (!_considerShrinkingCapacity) { var i = _arr.length-1; while (i >= _size) { _arr(i) = 0; i -= 1 } } }
  @inline final protected def _length = _size
  @inline final protected def _apply(index:Int): Int = _arr(index)
  protected def _foreach[U](f:Int=>U): Unit = { var i = 0; while (i < _size) { f(_arr(i)); i += 1 } }
  @inline final protected def _update(index:Int, value:Int): Unit = _arr(index) = value
  @inline final protected def _increment(index:Int, incr:Int): Unit = { _ensureCapacity(index+1); _arr(index) += incr; if (_size < index+1) _size = index+1 }
  @inline final protected def _append(elem: Int): this.type = { _ensureCapacity(_size + 1); _arr(_size) = elem; _size += 1; this }
  protected def _copyToArray(a:Array[Int]): Unit = arraycopy(_arr, 0, a, 0, _size)
  protected def _mapToArray[A](a:Array[A], f:Int=>A): Unit = { var i = 0; while (i < _size) { a(i) = f(_arr(i)); i += 1 } }
  protected def _asSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    final def length = _size
    final def apply(i:Int) = _arr(i)
  }
  protected def _toSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    private val arr = new Array[Int](_size); arraycopy(_arr, 0, arr, 0, _size)
    final def length = arr.length
    final def apply(i:Int) = arr(i)
  }
  protected def _array: Array[Int] = _arr // Careful.  _array.length may not equal _length
  @inline final protected def _asArray: Array[Int] = // Carefully, dangerous to access directly 
    if (_size == _arr.length) _arr 
    else { val a = new Array[Int](_size); arraycopy(_arr, 0, a, 0, _size); a }
  protected def _toArray: Array[Int] = { val a = new Array[Int](_size); arraycopy(_arr, 0, a, 0, _size); a }
  protected def _asIntSeq: IntSeq = new TruncatedArrayIntSeq(_arr, _size)
  protected def _takeAsIntSeq(len:Int): IntSeq = new TruncatedArrayIntSeq(_arr, math.min(len, _size))
  protected def _sum: Int = { var s = 0; var i = 0; while (i < _size) { s += _arr(i); i += 1 }; s }
  /** Return the index containing the value i, or -1 if i is not found. */
  protected def _indexOf(i:Int): Int = { var j = 0; while (j < _size) { if (_arr(j) == i) return j; j += 1 }; -1 }
  /** Return the index containing the value i, or -1 if i is not found.  Do so more efficiently by assuming that the contents are sorted in ascending order. 
      Look by starting near the last index as which a search was successful. */
  protected def _indexOfSorted(i:Int): Int = {
    if (_size == 0) return -1
    if (_lastIndex >= _size) _lastIndex = 0
    if (_arr(_lastIndex) == i) _lastIndex
    else if (_arr(_lastIndex) < i) { 
      var j = _lastIndex+1
      while (j < _size && _arr(j) < i) { j += 1 }
      if (j < _size && _arr(j) == i) { _lastIndex = j; j } else -1 
    } else {
      var j = _lastIndex-1
      while (j >= 0 && _arr(j) > i) { j -= 1 }
      if (j >= 0 && _arr(j) == i) { _lastIndex = j; j } else -1
    }
  }
  /** Return the index at which value i should be inserted in order to maintain sorted order.  
      This assumes that the existing elements already already sorted.  If value i is already present, return its index. */
  protected def _indexForInsertSorted(i:Int): Int = {
    if (_size == 0) return 0
    if (_lastIndex >= _size) _lastIndex = 0
    var j = 0
    if (_arr(_lastIndex) == i) j = _lastIndex
    else if (_arr(_lastIndex) < i) j = _positionLte(i, _lastIndex+1, _size)
    else j = _positionLte(i, 0, _lastIndex)
    _lastIndex = j
    j
  }
  protected def _indexForInsertSortedLinear(i:Int): Int = {
    if (_size == 0) return 0
    if (_lastIndex >= _size) _lastIndex = 0
    var j = 0
    if (_arr(_lastIndex) == i) 
      j = _lastIndex
    else if (_arr(_lastIndex) < i) { 
      j = _lastIndex+1
      while (j < _size && _arr(j) < i) { j += 1 }
    } else {
      j = _lastIndex-1
      while (j > 0 && _arr(j) < i) { j -= 1 }
    }
    _lastIndex = j
    j
  }
  /** Search the array '_arr' for the index at which value x could be inserted in sorted order.
      @param start the lowest index to consider
      @param end one plus the highest index that already contains data
      @return the index into '_arr' such that _arr(index) == x, 
      or ind(index-1) < x < ind(index)
      or index == end.
      */
  private def _positionLte(x:Int, start:Int, end:Int): Int = {
    val diff = end - start
    if (diff == 0) return start
    if (diff == 1) return if (_arr(start) >= x) start else end
    val middle = start + (diff / 2)
    val midval = _arr(middle)
    if (midval == x) middle
    else if (x < midval) _positionLte(x, start, middle)
    else _positionLte(x, middle+1, end)
  }
  /** Return true iff the integer 'index' is contained in _arr between positions 'start' and 'end-1' inclusive.  Look by recursive binary search.  */
  private def _containsSorted(x:Int, start:Int, end:Int): Boolean = {
    // /println("SparseBinaryVector._contains "x+" "+start+" "+end+" diff="+diff)
    val diff = end - start
    if (diff == 0) return false
    if (diff == 1) return _arr(start) == x
    val middle = start + (diff / 2)
    val midval = _arr(middle)
    if (midval == x) true
    else if (x < midval) _containsSorted(x, start, middle)
    else _containsSorted(x, middle+1, end)
  }
  protected def _containsSorted(x:Int): Boolean = _indexOfSorted(x) != -1 // _containsSorted(x, 0, _size)

  protected def _clear(): Unit = { _arr = new Array[Int](_initialCapacity); _size = 0; _lastIndex = 0 }
  protected def _sizeHint(len: Int) = if (len >= _size && len >= 1) _setCapacity(len)
  protected def _set(elts: Array[Int]): Unit = { _ensureCapacity(elts.length); arraycopy(elts, 0, _arr, 0, elts.length); _size = elts.length }
  protected def _set(elts: Seq[Int]): Unit = { _ensureCapacity(elts.length); var i = elts.length; while (i >= 0) { _arr(i) = elts(i); i += 1 }; _size = elts.length }
  protected def _fill(elt:Int): Unit = { var i = 0; while (i < _size) { _arr(i) = elt; i += 1 } }
  protected def _appendAll(elts: Array[Int]): Unit = {
    _ensureCapacity(_size + elts.length)
    arraycopy(elts, 0, _arr, _size, elts.length)
    _size += elts.length
    _setCapacity(_size) // assuming won't soon be adding more, save memory & make _array more efficient
  }
  protected def _appendAll(elts: TraversableOnce[Int]): Unit = {
    val n = elts.size
    _ensureCapacity(_size + n)
    elts.foreach(i => { _arr(_size) = i; _size += 1 })
    _setCapacity(_size) // assuming won't soon be adding more, save memory & make _array more efficient
  }
  protected def _prepend(elt: Int): Unit = {
    _ensureCapacity(_size + 1)
    arraycopy(_arr, 0, _arr, 1, _size)
    _arr(0) = elt
    _size += 1
  }
  protected def _prependAll(elts: TraversableOnce[Int]): Unit = _insertAll(0, elts.toTraversable)
  protected def _insert(index: Int, elt:Int): Unit = {
    _ensureCapacity(_size + 1)
    if (index < _size) arraycopy(_arr, index, _arr, index + 1, _size - index)
    _arr(index) = elt
    _size += 1
  }
  protected def _insertSorted(elt:Int): Unit = {
    val index = _indexForInsertSorted(elt)
    //assert(index >= 0 && index <= _size, index)
    _insert(index, elt)
  }
  protected def _insertSortedNoDuplicates(elt:Int): Unit = {
    val index = _indexForInsertSorted(elt)
    if (index >= _size || _arr(index) != elt) _insert(index, elt)
  }
  // TODO Make another version of this that works on IntSeq instead of Traversable[Int] 
  protected def _insertAll(index: Int, seq: scala.collection.Traversable[Int]): Unit = {
    if (index < 0 || index > _size) throw new IndexOutOfBoundsException(index.toString)
    val xs = seq.toList
    val len = xs.length
    _ensureCapacity(_size + len)
    arraycopy(_arr, index, _arr, index+len, _size-index)
    xs.copyToArray(_arr, index)
    _size += len
  }
  protected def _insertAllSorted(seq: scala.collection.Traversable[Int]): Unit = throw new Error("Not yet implemented.")
  protected def _remove(index: Int, count: Int) {
    require(count >= 0, "removing non-positive number of elements")
    if (index < 0 || index > _size - count) throw new IndexOutOfBoundsException(index.toString)
    arraycopy(_arr, index + count, _arr, index, _size - (index + count))
    _reduceToSize(_size - count)
  }
  protected def _remove(index: Int): Unit = _remove(index, 1)
}

class IntArrayBuffer extends ProtectedIntArrayBuffer with MutableIntSeq {
  def this(initialCapacity:Int) = { this(); _setCapacity(initialCapacity) }
  def apply(index:Int): Int = _apply(index)
  def update(index:Int, value:Int): Unit = _update(index, value)
  def length: Int = _length
  def toArray = _toArray
  override def _array: Array[Int] = super._array // Careful.  _array.length may not equal length
  def +=(i:Int): Unit = _append(i)
  def ++=(is:Array[Int]): Unit = _appendAll(is)
  def ++=(is:Seq[Int]): Unit = _appendAll(is)
  def +=:(i:Int): Unit = _prepend(i)
  def insert(index:Int, elt:Int): Unit = _insert(index, elt)
  override def _rawArray = _array
}

class SortedIntArrayBuffer extends ProtectedIntArrayBuffer with IntSeq {
  def apply(index:Int): Int = _apply(index)
  def length: Int = _length
  def setCapacity(c:Int) = _setCapacity(c)
  def toArray = _toArray
  override def _array: Array[Int] = super._array // Careful.  _array.length may not equal length
  def +=(i:Int): Unit = _insertSorted(i)
  def -=(i:Int): Unit = { val index = _indexForInsertSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSorted(is(j)); j += 1} }
  def ++=(is:Seq[Int]): Unit = { _ensureCapacity(_length + is.length); is.foreach(_insertSorted(_)) }
  override def _rawArray = _array
}

/** Like SortedIntArrayBuffer, but with no duplicate entries. */
class SortedIntSetBuffer extends ProtectedIntArrayBuffer with IntSeq {
  def apply(index:Int): Int = _apply(index)
  def length: Int = _length
  def setCapacity(c:Int) = _setCapacity(c)
  def toArray = _toArray
  override def _array: Array[Int] = super._array // Careful.  _array.length may not equal length
  def +=(i:Int): Unit = _insertSortedNoDuplicates(i)
  def -=(i:Int): Unit = { val index = _indexForInsertSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSortedNoDuplicates(is(j)); j += 1} }
  def ++=(is:Seq[Int]): Unit = { _ensureCapacity(_length + is.length); is.foreach(_insertSortedNoDuplicates(_)) }
  override def _rawArray = _array
}

