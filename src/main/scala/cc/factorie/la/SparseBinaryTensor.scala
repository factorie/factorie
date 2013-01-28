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

trait SparseBinaryTensor extends Tensor with cc.factorie.util.ProtectedIntArrayBuffer with SparseDoubleSeq {
  def isDense = false
  def activeDomain = new ArrayIntSeq(_array)
  override def activeDomainSize = _length
  @inline final def apply(index:Int): Double = if (_indexOfSorted(index) >= 0) 1.0 else 0.0
  @inline final def contains(index:Int): Boolean = _containsSorted(index)
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val len = _length; var i = 0; while (i < len) { f(_array(i), 1.0); i += 1 }}
  override def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
    var i = 0
    def hasNext = i < _length
    def next = { i += 1 ; (_array(i-1), 1.0) }
  }
  /** Efficient (but dangerous) direct access to underlying array of indices.  Note that the array.length may be larger than the number of indices. */
  def _indices: Array[Int] = _array
  def _indicesLength: Int = _length // TODO Remove because we have activeDomainSize?
  override def sum: Double = _length.toDouble
  override def max: Double = if (_length > 0) 1.0 else 0.0
  override def min: Double = if (_length == 0) 0.0 else 1.0
  override def indexOf(d:Double): Int = if (d != 0.0 && d != 1.0) -1 else if (d == 1.0) { if (_length == 0) -1 else _apply(0) } else { if (_length == 0) 0 else throw new Error("Not yet implemented") }
  override def maxIndex: Int = if (_length == 0) 0 else _apply(0)
  override def containsNaN: Boolean = false
  //def =+(a:Array[Double]): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)) += 1.0; i += 1 } }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)+offset) += f; i += 1 } }
  def +=(i:Int): Unit = _insertSortedNoDuplicates(i)
  def -=(i:Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSortedNoDuplicates(is(j)); j += 1} }
  def ++=(is:Iterable[Int]): Unit = { _ensureCapacity(_length + is.size); is.foreach(_insertSortedNoDuplicates(_)) }
  override def update(i:Int, v:Double): Unit = {
    if (i < 0 || i >= length) throw new Error("Tensor index out of range: "+i)
    if (v == 1.0) this += i else if (v == 0.0) tryRemove(i) else throw new Error(getClass.getName+" cannot update with values other than 0.0 or 1.0.")
  }
  private def tryRemove(i: Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) }
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

