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

package cc.factorie.la.tensorsketch

/*
 * @author martin
 * @date 3/6/2012
 */

import collection.mutable.IndexedSeq
import collection.mutable.HashMap

// Sparse and Dense backing collections must be of the same type, IndexedSeq[T].
// this specialized might not make a difference, is the backing map specialized?
class SparseArray[@specialized(Double) T](_length: Int, _default: () => T) extends IndexedSeq[T] {
  private val hash = new HashMap[Int,T] { override def default(k: Int): T = _default() }
  override val length = _length
  override def apply(i: Int): T = hash(i)
  override def update(i: Int, d: T) { hash.update(i, d) }
}

trait Tensor {
  def dimensions: Array[Int]
  private val _default = 0.0
  def default: Double = _default
}

class Tensor1(val dim1: Int, val sparsity: Symbol) extends IndexedSeq[Double] with Tensor {
  def dimensions = Array(dim1)
  final def length: Int = dim1
  private val values: IndexedSeq[Double] = sparsity match {
    case 'Dense => new Array[Double](dim1)
    case 'Sparse => new SparseArray[Double](dim1, () => 0.0)
    case _ => sys.error("Unsupported sparsity type.")
  }
  def apply(i:Int) = values(i)
  def update(i:Int, v:Double) { values(i) = v }
  def dot(t: Tensor1): Double = this.zip(t).map(x => x._1 * x._2).sum
  def toDenseArray: Array[Double] = values.toArray
  def outer(t1:Tensor1): Tensor2 = {
    val t2 = new Tensor2(dim1, t1.dim1, Seq(this.sparsity, t1.sparsity))
    for (i <- 0 until dim1) { // actually use something like activeDomain here
      val t2i: Tensor1 = t2(i)
      for (j <- 0 until t1.dim1) {
        t2i(j) = this(i) * t1(j)
      }
    }
    t2
  }
}

class Tensor2(val dim1: Int, val dim2: Int, val sparsity: Seq[Symbol]) extends IndexedSeq[Tensor1] with Tensor {
  def dimensions = Array(dim1, dim2)
  val length: Int = dim2
  def apply(i:Int): Tensor1 = values(i)
  assert(sparsity.length == 2)
  private val values: IndexedSeq[Tensor1] = sparsity(0) match {
    case 'Dense => Array.fill(dim2)(new Tensor1(dim1, sparsity(1)))
    case 'Sparse => new SparseArray[Tensor1](dim2, () => new Tensor1(dim1, sparsity(1)))
    case _ => sys.error("Unsupported sparsity type.")
  }
  def update(i: Int, t: Tensor1): Unit = values(i) = t
  def dot(t: Tensor2): Double = this.zip(t).map(x => x._1 dot x._2).sum
  def toDenseArray: Array[Array[Double]] = this.map(_.toDenseArray).toArray
}

class Tensor3(val dim1: Int, val dim2: Int, val dim3: Int, val sparsity: Seq[Symbol]) extends IndexedSeq[Tensor2] with Tensor {
  def dimensions = Array(dim1, dim2, dim3)
  val length: Int = dim3
  def apply(i:Int): Tensor2 = values(i)
  assert(sparsity.length == 3)
  private val values: IndexedSeq[Tensor2] = sparsity(0) match {
    case 'Dense => Array.fill(dim2)(new Tensor2(dim1, dim2, sparsity.tail))
    case 'Sparse => new SparseArray[Tensor2](dim2, () => new Tensor2(dim1, dim2, sparsity.tail))
    case _ => sys.error("Unsupported sparsity type.")
  }
  def update(i: Int, t: Tensor2): Unit = values(i) = t
  def dot(t: Tensor3): Double = this.zip(t).map(x => x._1 dot x._2).sum
  def toDenseArray: Array[Array[Array[Double]]] = this.map(_.toDenseArray).toArray
}



object Tensor {
  def outer(t1:Tensor, t2:Tensor): Tensor = null
  def outer(t1:Tensor, t2:Tensor, t3:Tensor): Tensor = null
  def outer(t1:Tensor, t2:Tensor, t3:Tensor, t4:Tensor): Tensor = null
  // TODO: move dot
}

object DenseTensor1 { def apply(dim1: Int): Tensor1 = new Tensor1(dim1, 'Dense) }
object SparseTensor1 { def apply(dim1: Int): Tensor1 = new Tensor1(dim1, 'Sparse) }


object DenseTensor2 { def apply(dim1: Int, dim2: Int): Tensor2           = new Tensor2(dim1, dim2, Seq('Dense, 'Dense)) }
object SparseOuter1DenseInner { def apply(dim1: Int, dim2: Int): Tensor2 = new Tensor2(dim1, dim2, Seq('Sparse, 'Dense)) }
object DenseOuter1SparseInner { def apply(dim1: Int, dim2: Int): Tensor2 = new Tensor2(dim1, dim2, Seq('Dense, 'Sparse)) }
object SparseSparseTensor2 { def apply(dim1: Int, dim2: Int): Tensor2    = new Tensor2(dim1, dim2, Seq('Sparse, 'Sparse)) }


object DenseTensor3 { def apply(dim1: Int, dim2: Int): Tensor2           = new Tensor2(dim1, dim2, Seq('Dense, 'Dense, 'Dense)) }
object SparseOuter2DenseInner { def apply(dim1: Int, dim2: Int): Tensor2 = new Tensor2(dim1, dim2, Seq('Sparse, 'Sparse, 'Dense)) }

object TensorTest extends App {
  var t = DenseTensor2(10,10)
  t(0)(0) = 1
  println(t)
}
