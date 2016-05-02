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
import cc.factorie.util.{SparseDoubleSeq, IntSeq, DoubleSeq}

/** An lazy product of a Vector and a scalar.
    Note that changes in the underlying Tensor will also show up here. 
    @author Andrew McCallum */
@deprecated("Not used anywhere", "Before 10/06/15")
class TensorTimesScalar(val tensor:Tensor, val scalar:Double) extends Tensor with ReadOnlyTensor with SparseDoubleSeq {
  def activeDomainSize = tensor.activeDomainSize
  def numDimensions: Int = tensor.numDimensions
  def dimensions: Array[Int] = tensor.dimensions
  // For handling sparsity
  def activeDomain: IntSeq = tensor.activeDomain
  def activeDomains: Array[IntSeq] = tensor.activeDomains
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = tensor.activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  def isDense: Boolean = tensor.isDense
  def length = tensor.length
  override def dot(t:DoubleSeq): Double = tensor.dot(t) * scalar
  override def *(scalar:Double) = new TensorTimesScalar(tensor, scalar*this.scalar)
  //override def update(i:Int, v:Double): Unit = tensor.update(idx, value/scalar)
  //override def +=(v: Vector) { vector += v*(1.0/scalar) }
  def apply(index:Int) = tensor.apply(index) * scalar
  def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
  def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

