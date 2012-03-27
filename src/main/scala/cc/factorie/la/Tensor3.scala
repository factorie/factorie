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

trait Tensor3 extends Tensor {
  def dim1: Int
  def dim2: Int
  def dim3: Int
  def activeDomain1: IntSeq
  def activeDomain2: IntSeq
  def activeDomain3: IntSeq
  def numDimensions: Int = 3
  def activeDomains = Array(activeDomain1, activeDomain2)
  def dimensions = Array(dim1, dim2, dim3)
  def apply(i:Int, j:Int, k:Int): Double = apply(i*dim2*dim3 + j*dim2 + k)
  def update(i:Int, j:Int, k:Int, v:Double): Unit = update(i*dim2*dim3 + j*dim2 + k, v)
  @inline final def length = dim1 * dim2 * dim3
  @inline final def singleIndex(i:Int, j:Int, k:Int): Int = i*dim2*dim3 + j*dim2 + k 
  @inline final def multiIndex(i:Int): (Int, Int, Int) = (i/dim2/dim3, (i/dim3)%dim2, i%dim3)
}

trait DenseTensorLike3 extends Tensor3 {
  private var _values = new Array[Double](dim1*dim2*dim3)
  def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain3 = new RangeIntSeq(0, dim3)
  def activeDomain = new RangeIntSeq(0, dim1*dim2*dim3)
  def apply(i:Int): Double = _values(i)
  override def apply(i:Int, j:Int, k:Int): Double = _values(i*dim2*dim3 + j*dim2 + k)
  override def update(i:Int, v:Double): Unit = _values(i) = v
  override def update(i:Int, j:Int, k:Int, v:Double): Unit = _values(i*dim2*dim3 + j*dim2 + k) = v
  override def +=(i:Int, v:Double): Unit = _values(i) += v
  override def zero(): Unit = java.util.Arrays.fill(_values, 0.0)
}
class DenseTensor3(val dim1:Int, val dim2:Int, val dim3:Int) extends DenseTensorLike3
