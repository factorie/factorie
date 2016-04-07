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
import cc.factorie.util.{SparseDoubleSeq, DoubleSeq, ArrayDoubleSeq, IntSeq, TruncatedArrayIntSeq, FastSorting}

/** A sparse Tensor that stores an array of indices having non-zero values and an aligned sized array storing those values. */

trait SparseTensor extends SparseDoubleSeq with Tensor {
  def isDense = false
  def _makeReadable(): Unit
  // unsafe - call makeReadable first
  def _unsafeActiveDomainSize: Int
  // unsafe - call makeReadable first (if you call this before making readable, you can get a reference to the wrong array)
  def _indices: Array[Int]
  // unsafe - call makeReadable first
  // this has to be a DoubleSeq and not an Array[Int] so we can efficiently return a UniformTensor for binary tensor values
  def _valuesSeq: DoubleSeq
  def sizeHint(size: Int): Unit
}

trait SparseIndexedTensor extends SparseTensor {
  // unsafe - call makeReadable first
  def _values: Array[Double]
  // unsafe - call makeReadable first
  def _valuesSeq = new ArrayDoubleSeq(_values)
}

trait ArraySparseIndexedTensor extends SparseIndexedTensor {
  // In subclasses either _length should be set > 0 or _sizeProxy should be set non-null, but not both.
  //private var _length: Int = 0
  //private var _sizeProxy: Iterable[Any] = null

  // TODO Alex and I confirmed that private var access in traits still has getter and setter methods - we need to find out a better way to do this -luke
  private var __values: Array[Double] = new Array[Double](4)
  private var __indices: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
  private var __npos = 0 // the number of positions in _values and _indices that are actually being used
  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
  
  // TODO Avoid making these public?  But used in BP now. -akm
  def _values = __values
  def _indices = __indices
  def _unsafeActiveDomainSize: Int = __npos

  private def setCapacity(cap:Int): Unit = {
    require(cap >= __npos)
    __indices = java.util.Arrays.copyOf(__indices, cap)
    __values = java.util.Arrays.copyOf(__values, cap)
  }
  def ensureCapacity(cap:Int): Unit = if (__indices.length < cap) setCapacity(math.max(cap, __indices.length + __indices.length/2))
  def trim(): Unit = setCapacity(__npos)

  // unsafe - call makeReadable first
  // TODO There must already be functions somewhere that do this.
  private def copyarray(a:Array[Double]): Array[Double] = { if (a eq null) null else java.util.Arrays.copyOf(a, a.length) }
  private def copyarray(a:Array[Int]): Array[Int] = { if (a eq null) null else java.util.Arrays.copyOf(a, a.length) }
  def copyInto(t:SparseIndexedTensor): Unit = t match {
    case t: ArraySparseIndexedTensor =>
      t.__values = copyarray(__values); t.__indices = copyarray(__indices); t._positions = copyarray(_positions); t.__npos = __npos; t._sorted = _sorted
  }
  
  //def length: Int = if (_sizeProxy ne null) _sizeProxy.size else _length
  override def activeDomainSize: Int = { makeReadable(); __npos }
  def activeDomain: IntSeq = { makeReadable() ; new TruncatedArrayIntSeq(__indices, __npos) } // TODO Consider making more efficient
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { makeReadable(); var i = 0; while (i < __npos) { f(__indices(i), __values(i)); i += 1 } }
  override def activeElements: Iterator[(Int,Double)] = {
    makeReadable()
    new Iterator[(Int,Double)] { // Must not change _indices and _values during iteration!
      var i = 0
      def hasNext = i < __npos
      def next() = { i += 1 ; (__indices(i-1), __values(i-1)) }
    }
  }
  // TODO need to assert that _sorted < __npos always. Add a "checkInvariants" method?? -luke
  override def zero(): Unit = { __npos = 0; _sorted = 0 }
  override def sum: Double = { var s = 0.0; var i = 0; while (i < __npos) { s += __values(i); i += 1 }; s }

  /** Return the position at which index occurs, or -1 if index does not occur. */
  def position(index:Int): Int = {
    makeReadable()
    val pos = java.util.Arrays.binarySearch(__indices, 0, _sorted, index)
    if (pos >= 0) pos else -1
  }
  def position(index:Int, start:Int): Int = { // Just linear search for now; consider binary search with memory of last position
    makeReadable()
    val pos = java.util.Arrays.binarySearch(__indices, start, _sorted, index)
    if (pos >= 0) pos else -1
  }

  override def toArray: Array[Double] = { val arr = new Array[Double](length); foreachActiveElement((i, v) =>arr(i) = v); arr }

  def apply(index:Int): Double = {
    // makeReadable is called in this.position
    val pos = position(index)
    if (pos < 0) 0.0 else __values(pos)
  }

  override def twoNormSquared: Double = {
    makeReadable()
    val l = __npos; var result = 0.0; var i = 0
    while (i < l) {
      val v = __values(i)
      result += v * v
      i += 1
    }
    result
  }

  override def oneNorm: Double = {
    val len = activeDomainSize
    _values.take(len).map(_.abs).sum
  }

  override def dot(v:DoubleSeq): Double = {
    makeReadable()
    v match {
      // TODO add fast implementations for Dense here! it's better to keep things off of dense since it's easy to dot against -luke
      case v:SingletonBinaryTensor => apply(v.singleIndex)
      case v:SingletonIndexedTensor => apply(v.singleIndex) * v.singleValue
      case v:ArraySparseIndexedTensor => {
        v._makeReadable()
        val v1 = if (this.__npos <= v.__npos) this else v
        val v2 = if (v.__npos < this.__npos) this else v
        var i = 0; var j = -1; var j2 = 0
        var result = 0.0
        while (i < v1.__npos) {
          j2 = v2.position(v1.__indices(i), j+1)
          if (j2 >= 0) { result += v1.__values(i) * v2.__values(j2); j = j2 }
          i += 1
        }
        result
      }
      case v:SparseBinaryTensor => {
        v._makeReadable()
        var i = 0
        val len = v.activeDomainSize
        val indices = v._indices
        var result = 0.0
        var pos = 0
        while (i < len && pos >= 0) {
          val posTmp = position(indices(i), pos)
          if (posTmp >= 0) {
            pos = posTmp
            result += __values(pos)
          }
          i += 1
        }
        result
      }
      case t: DenseTensor => {
        val tArr = t.asArray
        val len = activeDomainSize
        val indices = _indices
        val values = _values
        var i = 0
        var dot = 0.0
        while (i < len) {
          dot += tArr(indices(i)) * values(i)
          i += 1
        }
        dot
      }
      case t: Tensor =>
        if (!SparseIndexedTensor.hasLogged) {
          SparseIndexedTensor.hasLogged = true
          println("Warning: SparseIndexedTensor slow dot with type " + t.getClass.getName)
        }
        var dot = 0.0
        t.foreachActiveElement((i, v) => dot += apply(i)*v)
        dot
    }
  }

  override def toString = "SparseIndexedTensor npos="+__npos+" sorted="+_sorted+" ind="+__indices.mkString(",")+" val="+__values.mkString(",")

  def _makeReadable(): Unit = makeReadable()

  final private def doTheSort(): Array[Int] = {

    val newIndices = new Array[Int](__npos)
    val len = __npos; var i = 0
    while (i < len) { newIndices(i) = i; i += 1}
    FastSorting.quickSort(keys = java.util.Arrays.copyOf(__indices, __npos), values = newIndices)
    newIndices
  }

  final private def makeReadableEmpty(): Unit = {
    // We can assume that the "readable" part of the vector is empty, and hence we can just sort everything
    val sortedIndices = doTheSort()
    val newIndices = new Array[Int](__indices.length)
    val newValues = new Array[Double](__indices.length)
    var prevIndex = __indices(sortedIndices(0))
    newIndices(0) = prevIndex
    newValues(0) = __values(sortedIndices(0))
    var i = 1
    var j = 0
    while (i < __npos) {
      val idx = sortedIndices(i)
      if (prevIndex != __indices(idx)) {
        j += 1
        newIndices(j) = __indices(idx)
        prevIndex = __indices(idx)
      }
      newValues(j) += __values(idx)
      i += 1
    }
    _sorted = j+1
    __indices = newIndices
    __values = newValues
  }

  final private def makeReadableIncremental(): Unit = {
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
  }

  final private def makeReadable(): Unit = {
    if (_sorted == __npos) return
    if ((_sorted <= 10) && (__npos > 0)) {
      makeReadableEmpty()
    } else {
      makeReadableIncremental()
    }
    __npos = _sorted
    // if (__npos * 1.5 > __values.length) trim()
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
    if (_sorted == 0) {
      +=(index,value)
    } else {
      val p = position(index)
      if (p >= 0) __values(p) = value
      else +=(index, value)
    }
  }
  // Efficiently support multiple sequential additions
  override def +=(index:Int, incr:Double): Unit = if (incr != 0.0) {
    ensureCapacity(__npos+1)
    __indices(__npos) = index
    __values(__npos) = incr
    __npos += 1
  }
  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor => +=(t.singleIndex, f)
    case t:SingletonIndexedTensor => +=(t.singleIndex, f * t.singleValue)
    case t:SparseBinaryTensor => { val len = t.activeDomainSize; val a = t._indices; var i = 0; while (i < len) { +=(a(i), f); i += 1 }}
    case t:SparseIndexedTensor => { val len = t.activeDomainSize; val as = t._indices; val vs = t._values; var i = 0; while (i < len) { +=(as(i), f * vs(i)); i += 1 }}
    case t:DenseTensor => { val arr = t.asArray; var i = 0; while (i < arr.length) {this += (i, arr(i)*f)  ; i += 1} }
    case t:Outer2Tensor => {
      val ff = f*t.scale
      (t.tensor1, t.tensor2) match {
        case (t1: DenseTensor, t2: SparseBinaryTensor) =>
          var i = 0
          val arr = t1.asArray
          while (i < arr.length) {
            val indices = t2._indices
            var j = 0
            while (j < t2.activeDomainSize) {
              this += (t.singleFlatIndex(i, indices(j)), f*t1(i))
              j += 1
            }
            i += 1
          }
        case (t1: DenseTensor, t2: SparseTensor) =>
          var i = 0
          val arr = t1.asArray
          while (i < arr.length) {
            val len = t2.activeDomainSize
            val indices = t2._indices
            val values = t2._valuesSeq
            var j = 0
            while (j < len) {
              this += (t.singleFlatIndex(i, indices(j)), ff*t1(i)*values(j))
              j += 1
            }
            i += 1
          }
        case (t1: SingletonBinaryTensor, t2: DenseTensor) => {
          val i0 = t1.singleIndex
          val arr = t2.asArray
          var i = 0
          while (i < arr.length) {
            this += (t.singleFlatIndex(i0, i), ff*arr(i))
            i += 1
          }
        }
        case (t1: SparseTensor, t2: DenseTensor) =>
          var j = 0
          val arr = t2.asArray
          while (j < arr.length) {
            val len = t1.activeDomainSize
            val indices = t1._indices
            val values = t1._valuesSeq
            var i = 0
            while (i < len) {
              this += (t.singleFlatIndex(indices(i), j), ff*t2(j)*values(i))
              i += 1
            }
            j += 1
          }
        case (t1: SingletonTensor, t2: SparseTensor) => {
          val i0 = t1.singleIndex
          // Have to call activeDomainSize/makeReadable before reading out array of indices or this will have mismatched indices and values -luke
          val len = t2.activeDomainSize
          val arr = t2._indices
          val values = t2._valuesSeq
          var i = 0
          while (i < len) {
            val singleidx = t.singleFlatIndex(i0, arr(i))
            this += (singleidx, ff*t1.singleValue*values(i))
            i += 1
          }
        }
        case (t1: SparseTensor, t2: SparseTensor) => {
          val len1 = t1.activeDomainSize
          val indices1 = t1._indices
          val values1 = t1._valuesSeq
          val len2 = t2.activeDomainSize
          val indices2 = t2._indices
          val values2 = t2._valuesSeq
          var i = 0
          while (i < len1) {
            var j = 0
            while (j < len2) {
              this += (t.singleFlatIndex(indices1(i), indices2(j)), ff*values1(i)*values2(j))
              j += 1
            }
            i += 1
          }
        }
        case (t1: DenseTensor, t2: DenseTensor) => {
          val arr1 = t1.asArray
          val arr2 = t2.asArray
          var i = 0
          while (i < arr1.length) {
            var j = 0
            while (j < arr2.length) {
              this += (t.singleFlatIndex(i, j), arr1(i)*arr2(j)*ff)
              j += 1
            }
            i += 1
          }
        }
        case _ => throw new Error("types are " + t.tensor1.getClass.getName + " and " + t.tensor2.getClass.getName) }
      }
    case t:Tensor =>
      if (!SparseIndexedTensor.hasLogged) {
        SparseIndexedTensor.hasLogged = true
        println("Warning: SparseIndexedTensor slow += with type " + t.getClass.getName)
      }
      t.foreachActiveElement((i, v) => this += (i, v * f))
  }
  /** Increment Array "a" with the contents of this Tensor, but do so at "offset" into array and multiplied by factor "f". */
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = {
    val indices = __indices
    val values = __values
    val npos = __npos
    var i = 0
    while (i < npos) {
      a(indices(i) + offset) += f * values(i)
      i += 1
    }
  }
  
  override def expNormalize(): Double = {
    var max = Double.MinValue
    var i = 0
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

  override def exponentiate() {
    var i = 0
    while (i < __npos) {
      __values(i) = math.exp(__values(i))
      i += 1
    }
  }
  override def foldActiveElements(seed: Double, f: (Int, Double, Double) => Double): Double = {
    var acc = seed; var i = 0
    while (i < __npos) { acc = f(__indices(i), __values(i), acc); i += 1 }
    acc
  }
  override def maxNormalize() {
    var maxi = 0
    var max = Double.MinValue
    var i = 0
    while (i < __npos) {
      if (__values(i) > max) {
        max = __values(i)
        maxi = __indices(i)
      }
      i += 1
    }
    zero()
    update(maxi, 1)
  }

  override def *=(other: Double) {
    _makeReadable()
    var i = 0
    val len = activeDomainSize
    while (i < len) {
      __values(i) *= other
      i += 1
    }
  }

  def sizeHint(size: Int): Unit = ensureCapacity(size)
}

object SparseIndexedTensor {
  var hasLogged = false
}

