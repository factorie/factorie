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
import cc.factorie.util.{DoubleSeq, DenseDoubleSeq}

// TODO this should implement DenseTensor - then our cases will catch everything just matching sparse and dense -luke
trait UniformTensor extends Tensor with ReadOnlyTensor with DenseDoubleSeq {
  def activeDomainSize = length
  def uniformValue: Double
  def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
  def apply(i:Int) = uniformValue
  def isDense = true
  //def activeDomain: IntSeq = new RangeIntSeq(0, length) // Can't be both here an Tensor1
  override def isUniform = true
  override def sum: Double = length * uniformValue
  override def max: Double = uniformValue
  override def min: Double = uniformValue
  override def maxIndex: Int = 0
  override def containsNaN: Boolean = false
  override def dot(v:DoubleSeq): Double = v.sum * uniformValue
  //override def copy = this // safe because it is immutable
}

