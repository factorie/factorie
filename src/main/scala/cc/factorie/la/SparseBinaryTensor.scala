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
import cc.factorie.util.{DoubleSeq, TruncatedArrayIntSeq}

trait SparseBinaryTensor extends SparseTensor {
  // unsafe - call makeReadable first
  def _valuesSeq: DoubleSeq = new UniformTensor1(this._unsafeActiveDomainSize, 1.0)
}

trait ArraySparseBinaryTensor extends SparseBinaryTensor with cc.factorie.util.ProtectedIntArrayBuffer {
  def activeDomain = new TruncatedArrayIntSeq(_array, _length)
  def _appendUnsafe(i: Int) = _append(i) // TODO Make a new class UnsortedSparseBinaryTensorLike1, because, note, then the indices don't get sorted, and various index search methods will fail.
  def sizeHint(size:Int) = _sizeHint(size)
  override def activeDomainSize = _length
  def _makeReadable(): Unit = { }
  def _unsafeActiveDomainSize: Int = _length
  @inline final def apply(index:Int): Double = if (_indexOfSorted(index) >= 0) 1.0 else 0.0
  @inline final def contains(index:Int): Boolean = _containsSorted(index)
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val len = _length; var i = 0; while (i < len) { f(_array(i), 1.0); i += 1 }}
  override def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
    var i = 0
    def hasNext = i < _length
    def next() = { i += 1; (_array(i-1), 1.0) }
  }
  /** Efficient (but dangerous) direct access to underlying array of indices.  Note that the array.length may be larger than the number of indices. */
  def _indices: Array[Int] = _array
  override def sum: Double = _length.toDouble
  override def max: Double = if (_length > 0) 1.0 else 0.0
  override def min: Double = if (_length == 0) 0.0 else 1.0
  override def indexOf(d:Double): Int = if (d != 0.0 && d != 1.0) -1 else if (d == 1.0) { if (_length == 0) -1 else _apply(0) } else { if (_length == 0) 0 else throw new Error("Not yet implemented") }
  override def maxIndex: Int = if (_length == 0) 0 else _apply(0)
  override def containsNaN: Boolean = false
  //def =+(a:Array[Double]): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)) += 1.0; i += 1 } }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)+offset) += f; i += 1 } }
  // FIX: if you call this on a tensor that isn't statically a SparseBinaryTensor this will do the wrong thing
  // since overloading will pick the +=(double) method that adds a double uniformly to the whole DoubleSeq -luke
  // just removing these two methods still compiles but breaks tons of unit tests, probably because of the above overloading sitution
  // BUG this bit me again, need to figure this out
  def +=(i:Int): Unit = _insertSortedNoDuplicates(i)
  def -=(i:Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSortedNoDuplicates(is(j)); j += 1} }
  def ++=(is:Iterable[Int]): Unit = { _ensureCapacity(_length + is.size); is.foreach(_insertSortedNoDuplicates(_)) }
  override def update(i:Int, v:Double): Unit = {
    if (i < 0 || i >= length) throw new Error("Tensor index out of range: "+i+" range between 0 and " + length)
    if (v == 1.0) this += i else if (v == 0.0) tryRemove(i) else throw new Error(getClass.getName+" cannot update with values other than 0.0 or 1.0.")
  }
  private def tryRemove(i: Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) }
  /** In SparseBinary, this is equivalent to update(i,v) */
  override def +=(i:Int, v:Double): Unit = update(i, v)
  override def zero(): Unit = _clear() // TODO I think _clear should be renamed _zero -akm
  // TODO: can we add efficient dot against other sparse binary tensors? -luke
  override def dot(v: DoubleSeq): Double = v match {
    case t: SingletonBinaryTensor => if (contains(t.singleIndex)) 1.0 else 0.0
    case t: SingletonTensor => if (contains(t.singleIndex)) t.singleValue else 0.0
    // TODO Any other special cases here?
    case ds: DenseTensor => {
      val len = activeDomainSize
      val indices = _indices
      val arr = ds.asArray
      var result = 0.0; var i = 0
      while (i < len) { result += arr(indices(i)); i += 1 }
      result
    }
    case ds: DoubleSeq => { var result = 0.0; var i = 0; while (i < _length) { result += ds(_apply(i)); i += 1 }; result }
  }
//  def asIntArray = _asArray
  def toIntArray = _toArray
  override def foldActiveElements(seed: Double, f: (Int, Double, Double) => Double): Double = {
    var acc = seed; var i = 0
    while (i < _length) { acc = f(_apply(i), 1.0, acc); i += 1 }
    acc
  }
}

