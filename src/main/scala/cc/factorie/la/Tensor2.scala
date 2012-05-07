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

trait Tensor2 extends Tensor {
  def dim1: Int
  def dim2: Int
  def activeDomain1: IntSeq
  def activeDomain2: IntSeq
  def numDimensions: Int = 2
  def activeDomains = Array(activeDomain1, activeDomain2)
  def dimensions = Array(dim1, dim2)
  def apply(i:Int, j:Int): Double = apply(i*dim2 + j)
  def apply(i:Int): Double //= apply(i % dim1, i / dim2)
  def update(i:Int, j:Int, v:Double): Unit = update(i*dim2 + j, v)
  def +=(i:Int, j:Int, v:Double): Unit = +=(singleIndex(i, j), v)
  @inline final def length = dim1 * dim2
  @inline final def singleIndex(i:Int, j:Int): Int = i*dim2 + j
  @inline final def multiIndex(i:Int): (Int, Int) = (i/dim2, i%dim2)
}


trait DenseTensorLike2 extends Tensor2 with DenseTensorLike {
  private var __values = new Array[Double](dim1*dim2)
  protected def _values = __values
  def isDense = true
  def activeDomain1 = new RangeIntSeq(0, dim1)
  def activeDomain2 = new RangeIntSeq(0, dim2)
  def activeDomain = new RangeIntSeq(0, dim1*dim2)
  def apply(i:Int): Double = __values(i)
  override def apply(i:Int, j:Int): Double = __values(i*dim2+j)
  override def +=(i:Int, v:Double): Unit = _values(i) += v
  override def +=(ds:DoubleSeq): Unit = { require(ds.length == length); var i = 0; while (i < length) { _values(i) += ds(i); i += 1 } }
  override def update(i:Int, v:Double): Unit = _values(i) = v
  override def update(i:Int, j:Int, v:Double): Unit = _values(i*dim2+j) = v
}

class DenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 {
  def this(t:Tensor2) = { this(t.dim1, t.dim2); this := t }
}


// TODO Make a GrowableDenseTensor2



trait LayeredTensorLike2 extends Tensor2 {
  def newTensor1(dim:Int): Tensor1
  private var _inners = Array.fill(dim1)(newTensor1(dim2))
  override def apply(i:Int, j:Int): Double = _inners(i).apply(j)
  def apply(i:Int): Double = apply(i/dim1, i%dim2)
  override def update(i:Int, j:Int, v:Double): Unit = _inners(i).update(j, v)
}
// TODO Move these next three traits to the file Tensor1.scala
trait InnerDenseTensor1 {
  def newTensor1(dim:Int) = new DenseTensor1(dim) 
}
trait InnerSparseTensor1 {
  def newTensor1(dim:Int) = new SparseTensor1(dim) 
}
trait InnerSparseBinaryTensor1 {
  def newTensor1(dim:Int) = new SparseBinaryTensor1(dim) 
}
abstract class LayeredTensor2(val dim1:Int, val dim2:Int) extends LayeredTensorLike2
// e.g. new LayeredTensor2(20,30) with InnerDenseTensor1


