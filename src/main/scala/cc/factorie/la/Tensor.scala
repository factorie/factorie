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

// Just some preliminary notes on the upcoming "unflattening" of parameter and discrete statistics representations...

// TODO Beware of "equals" in IndexedSeq, though
trait Tensor extends IndexedSeq[Double] {
  def dimensions: Array[Int]
  private var _default = 0.0
  def default: Double = _default
  //def toDenseArray: Array[Double]
}

object Tensor {
  def outer(t1:Tensor, t2:Tensor): Tensor = null
  def outer(t1:Tensor, t2:Tensor, t3:Tensor): Tensor = null
  def outer(t1:Tensor, t2:Tensor, t3:Tensor, t4:Tensor): Tensor = null
}

trait Tensor1 extends Tensor {
  def dim1: Int
  def dimensions = Array(dim1)
  final def length: Int = dim1
  def apply(i:Int): Double
  def update(i:Int, v:Double): Unit
}

trait Tensor2 extends Tensor with IndexedSeq[Double] {
  def dim1: Int
  def dim2: Int
  def dimensions = Array(dim1, dim2)
  def apply(i:Int, j:Int): Double
  def apply(i:Int): Double = apply(i % dim1, i / dim2)
  def length = dim1 * dim2
  def update(i:Int, j:Int, v:Double): Unit
  def update(i:Int, v:Double): Unit = update(i % dim1, i / dim2, v)
}

trait Tensor3 extends Tensor {
  def dim1: Int
  def dim2: Int
  def dim3: Int
  def dimensions = Array(dim1, dim2, dim3)
  def apply(i:Int, j:Int, k:Int): Double
  def update(i:Int, j:Int, k:Int, v:Double): Unit
}

trait Tensor4 extends Tensor {
  def dim1: Int
  def dim2: Int
  def dim3: Int
  def dim4: Int
  def dimensions = Array(dim1, dim2, dim3, dim4)
  def apply(i:Int, j:Int, k:Int, l:Int): Double
  def update(i:Int, j:Int, k:Int, l:Int, v:Double): Unit
}


trait DenseTensorLike1 extends Tensor1 {
  private var _values = new Array[Double](dim1)
  def apply(i:Int) = _values(i)
  def update(i:Int, v:Double): Unit = _values(i) = v
  def outer(t1:Tensor1): Tensor2 = {
    val t2 = new DenseTensor2(dim1, t1.dim1)
    for (i <- 0 until dim1; j <- 0 until t1.dim1) t2(i,j) = this(i) * t1(j)
    t2
  }
}
class DenseTensor1(val dim1:Int) extends DenseTensorLike1
class SparseTensor1(dim1:Int) extends DenseTensor1(dim1) // TODO Fix this later

trait DenseTensorLike2 extends Tensor2 {
  private var _values = new Array[Double](dim1*dim2)
  def apply(i:Int, j:Int): Double = _values(i*dim2+j)
  def update(i:Int, j:Int, v:Double): Unit = _values(i*dim2+j) = v
}
class DenseTensor2(val dim1:Int, val dim2:Int) extends DenseTensorLike2
class DenseTensor2b(val dim1:Int, val dim2:Int) extends Tensor2 {
  private var values = Array.fill(dim1)(new DenseTensor1(dim2))
  def apply(i:Int, j:Int): Double = values(i).apply(j)
  def update(i:Int, j:Int, v:Double): Unit = values(i).update(j, v)
}


