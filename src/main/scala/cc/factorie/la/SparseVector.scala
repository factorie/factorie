///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie.la
//import cc.factorie._
//
///** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory.
//    @author Andrew McCallum */
//// For now, just alias to SparseHashVector.  Later perhaps this will be different
//class SparseVector(size:Int) extends SparseHashVector(size) {
//  def this(size:Int, occurrences:Seq[Int]) = { this(size); occurrences.foreach(increment(_, 1.0)) }
//  //private var used = 0
//  //private var capacity = 8
//  //private val indices = new Array[Int](capacity)
//  //private val values = new Array[Double](capacity)
//  //private def ensureCapacity(c:Int): Unit = {}
//}
//
//class SparseIndexedVector(len:Int) extends Vector {
//  def this(sizeProxy:Iterable[Any]) = { this(-1); _sizeProxy = sizeProxy }
//  private val _length: Int = len
//  private var _sizeProxy: Iterable[Any] = null
//  private var _values: Array[Double] = new Array[Double](4)
//  private var _indexs: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
//  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
//  private var _npos = 0 // the number of positions in _values and _indices that are actually being used
//  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
//  private def setCapacity(cap:Int): Unit = {
//    assert(cap >= _npos)
//    val newInd = new Array[Int](cap)
//    val newVal = new Array[Double](cap)
//    System.arraycopy(_indexs, 0, newInd, 0, _npos)
//    System.arraycopy(_values, 0, newVal, 0, _npos)
//    _indexs = newInd; _values = newVal
//  }
//  private def ensureCapacity(cap:Int): Unit = if (_indexs.length < cap) setCapacity(math.max(cap, _indexs.length + _indexs.length/2))
//  def trim: Unit = setCapacity(_npos)
//  
//  def length: Int = if (_length < 0) _sizeProxy.size else _length
//  def activeDomainSize: Int = { makeReadable; _npos }
//  def activeDomain: Iterable[Int] = { makeReadable ; _indexs.take(_npos) } // TODO Consider making more efficient
//  def activeElements = {
//    makeReadable
//    new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
//      var i = 0
//      def hasNext = i < _npos
//      def next = { i += 1 ; (_indexs(i-1), _values(i-1)) }
//    }
//  }
//
//  /** Return the position at which index occurs, or -1 if index does not occur. */
//  def position(index:Int): Int = { // Just linear search for now; consider binary search with memory of last position
//    makeReadable
//    var i = 0
//    while (i < _npos) { if (_indexs(i) == index) return i; i += 1 }
//    -1
//  }
//
//  def apply(index:Int): Double = {
//    // makeReadable is called in this.position
//    val pos = position(index)
//    if (pos < 0) 0.0 else _values(pos)
//  }
//
//  def dot(v:Vector): Double = {
//    makeReadable
//    v match {
//      case v:SingletonBinaryVector => apply(v.singleIndex)
//      case v:SingletonVector => apply(v.singleIndex) * v.doubleValue
//      case v:DenseVector => { var result = 0.0; var p = 0; while (p < _npos) { result += v(_indexs(p)) * _values(p); p += 1 }; result }
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
//      val ci = _indexs(cp)
//      val cv = _values(cp)
//      var i = cp - 1
//      while (i >= 0 && _indexs(i) >= ci) {
//        val tmpi = 
//        i -= 1
//      }
//    }
//    0
//  }
//  
//  override def toString = "SparseIndexedVector npos="+_npos+" sorted="+_sorted+" ind="+_indexs.mkString(",")+" val="+_values.mkString(",")
//  
//  @inline private def makeReadable: Unit = {
//    var cp = _sorted // "current position", the position next to be placed into sorted order
//    while (cp < _npos) {
//      //println("cp="+cp)
//      val ci = _indexs(cp) // "current index", the index next to be placed into sorted order.
//      val cv = _values(cp) // "current value"
//      var i = _sorted - 1
//      //println("i="+i)
//      // Find the position at which the current index/value belongs
//      while (i >= 0 && _indexs(i) >= ci) i -= 1
//      i += 1
//      // Put it there, shifting to make room if necessary
//      //println("Placing at position "+i)
//      if (_indexs(i) == ci) { if (i != cp) _values(i) += cv else _sorted += 1 }
//      else insert(i, ci, cv, incrementNpos=false, incrementSorted=true)
//      //println("sorted="+_sorted)
//      cp += 1
//    }
//    _npos = _sorted
//    if (_npos * 1.5 > _values.length) trim
//  }
//  
//  // Caller is responsible for making sure there is enough capacity
//  @inline private def insert(position:Int, index:Int, value:Double, incrementNpos:Boolean, incrementSorted:Boolean): Unit = {
//    if (_npos - position > 0) {
//      System.arraycopy(_values, position, _values, position+1, _sorted-position)
//      System.arraycopy(_indexs, position, _indexs, position+1, _sorted-position)
//    }
//    _indexs(position) = index
//    _values(position) = value
//    if (incrementNpos) _npos += 1
//    if (incrementSorted) _sorted += 1
//  }
//
//  override def update(index:Int, value:Double): Unit = {
//    val p = position(index)
//    if (p >= 0) _values(p) = value
//    else increment(index, value) 
//  }
//  // Efficiently support multiple sequential additions
//  override def increment(index:Int, incr:Double): Unit = {
//    ensureCapacity(_npos+1)
//    _indexs(_npos) = index
//    _values(_npos) = incr
//    _npos += 1
//  }
//  
//  override def +=(v:Vector): Unit = v.activeElements.foreach(iv => increment(iv._1, iv._2)) 
//  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
//  
//  override def clone: SparseIndexedVector = {
//    val v: SparseIndexedVector = if (_sizeProxy eq null) new SparseIndexedVector(_length) else new SparseIndexedVector(_sizeProxy)
//    makeReadable
//    v._npos = _npos
//    v._sorted = _sorted
//    v._values = _values.clone
//    v._indexs = _indexs.clone
//    // TODO Deal with _positions
//    v
//  }
//
//  override def flatOuter(that:Vector): Vector = {
//    makeReadable
//    that match {
//      case v: SingletonBinaryVec => {
//        val vlength = v.length
//        val vi = v.singleIndex
//        val result = this.clone
//        var i = 0
//        while (i < _npos) {
//          result._indexs(i) = result._indexs(i) * vlength + vi
//          i += 1
//        }
//        result
//      }
//      // TODO Add more cases
//    }
//  }
//
//}