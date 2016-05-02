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
import cc.factorie.util.DoubleSeq

//trait SingletonTensor extends SparseTensor with SparseDoubleSeq with ReadOnlyTensor {
//}

trait SingletonTensor extends SparseTensor with ReadOnlyTensor {
  def singleIndex: Int
  def singleValue: Double
  val activeDomainSize = 1
  def sizeHint(size: Int): Unit = { }
  def _makeReadable(): Unit = { }
  def _unsafeActiveDomainSize: Int = 1
  def _indices: Array[Int] = Array(singleIndex)
}

trait SingletonIndexedTensor extends SparseIndexedTensor with SingletonTensor {

  def _values: Array[Double] = Array(singleValue)
  def copyInto(t: SparseIndexedTensor): Unit = t(singleIndex) = singleValue

  //def activeDomain: IntSeq = new SingletonIntSeq(singleIndex) // Can't be here and in Tensor1
  override def apply(i:Int) = if (i == singleIndex) singleValue else 0.0
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = f(singleIndex, singleValue)
  override def activeElements: Iterator[(Int,Double)] = Iterator.single((singleIndex, singleValue))
  override def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = f(singleIndex, singleValue)
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = a(offset+singleIndex) += f * singleValue
  override def sum: Double = singleValue
  override def max: Double = if (singleValue > 0.0) singleValue else 0.0 
  override def min: Double = if (singleValue < 0.0) singleValue else 0.0 
  override def maxIndex: Int = if (singleValue >= 0.0) singleIndex else if (singleIndex != 0) 0 else 1
  override def containsNaN: Boolean = false
  //override def dot(v:DoubleSeq): Double = v(singleIndex) * singleValue
  //override def copy: SingletonTensor = this // immutable, but careful in the future we might make a mutable version
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor => if (singleIndex == t.singleIndex) singleValue else 0.0
    case t:SingletonTensor => if (singleIndex == t.singleIndex) singleValue * t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex) * singleValue
  }
}

