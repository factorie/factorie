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

trait SingletonBinaryTensor extends SparseBinaryTensor with SingletonTensor {
  def singleIndex: Int

  def singleValue = 1.0

  override def apply(i:Int) = if (i == singleIndex) 1.0 else 0.0
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = f(singleIndex, 1.0)
  override def activeElements: Iterator[(Int,Double)] = Iterator.single((singleIndex, 1.0))
  override def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = f(singleIndex, 1.0)
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = a(offset+singleIndex) += f
  override def sum: Double = 1.0
  override def max: Double = 1.0
  override def min: Double = 0.0
  override def containsNaN: Boolean = false
  override def maxIndex: Int = singleIndex
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor => if (singleIndex == t.singleIndex) 1.0 else 0.0
    case t:SingletonTensor => if (singleIndex == t.singleIndex) t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex)
  }
  //override def copy: SingletonBinaryTensor = this // immutable, but careful in the future we might make a mutable version
}
// TODO Make a mutable version of this to be used in BP with DotFamily.score(Tensor)
