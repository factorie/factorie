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

import cc.factorie.model.{Weights, WeightsMap}
import cc.factorie.util.{Accumulator, DoubleAccumulator, LocalDoubleAccumulator}

// NOTE: we don't implement Accumulator[WeightsMap] because contravariance
trait WeightsMapAccumulator {
  def accumulate(key: Weights, t: Tensor): Unit
  def accumulate(key: Weights, t: Tensor, factor: Double): Unit
  def accumulate(map: WeightsMap): Unit = {
    for ((k, v) <- map.toSeq)
      accumulate(k, v)
  }
  def accumulate(map: WeightsMap, factor: Double): Unit = {
    for ((k, v) <- map.toSeq)
      accumulate(k, v, factor)
  }
}

class LocalWeightsMapAccumulator(val tensorSet: WeightsMap) extends WeightsMapAccumulator {
  def accumulate(key: Weights, t: Tensor): Unit = tensorSet(key) += t
  def accumulate(key: Weights, index: Int, value: Double): Unit = tensorSet(key)(index) += value
  def accumulate(key: Weights, t: Tensor, factor: Double): Unit = tensorSet(key) += (t, factor)
  def combine(a: WeightsMapAccumulator): Unit = a match {
    case a: LocalWeightsMapAccumulator => tensorSet += a.tensorSet
  }
}

class SmartGradientAccumulator extends WeightsMapAccumulator {
  val map = new WeightsMap(w => throw new Error("trying to read an inexistent gradient"))
  val stateMap = collection.mutable.HashMap[Weights,Int]()
  val EMPTY = 0
  val SINGLE_TENSOR = 1
  val ACCUMULATOR = 3
  def clear() {
    map.clear()
    stateMap.clear()
  }
  def getMap: WeightsMap = map
  def accumulate(key: Weights, t: Tensor, d: Double) {
    stateMap.getOrElse(key, EMPTY) match {
      case ACCUMULATOR => map(key) += (t,d)
      case SINGLE_TENSOR =>
        val newTensor = map(key) match {
          case t: Outer1Tensor2 if t.tensor1.isDense && t.tensor2.isDense => new DenseTensor2(t.dim1, t.dim2)
          case t: Tensor1 => new SparseIndexedTensor1(t.dim1)
          case t: Tensor2 => new SparseIndexedTensor2(t.dim1, t.dim2)
          case t: Tensor3 => new SparseIndexedTensor3(t.dim1, t.dim2, t.dim3)
          case t: Tensor4 => new SparseIndexedTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
          case _ => throw new Error(s"Any concrete tensor should be either a Tensor1, Tensor2, Tensor3, or Tensor4. Offending class: ${map(key).getClass.getName}")
        }
        newTensor += map(key)
        newTensor += (t,d)
        map(key) = newTensor
        stateMap(key) = ACCUMULATOR
      case EMPTY =>
        t match {
          case t: SparseTensor if !t.isInstanceOf[SparseIndexedTensor] =>
            stateMap(key) = SINGLE_TENSOR
            // This again suggests we really want more tensors supporting *=
            val newT = Tensor.newSparse(t)
            newT += (t,d)
            map(key) = newT
          case t: Singleton2BinaryLayeredTensor3 =>
            stateMap(key) = ACCUMULATOR
            val newT = Tensor.newSparse(t)
            newT += (t,d)
            map(key) = newT
          case t: DenseTensor =>
            stateMap(key) = ACCUMULATOR
            val t2 = t.copy
            t2 *= d
            map(key) = t2
          case t: SparseTensor =>
            stateMap(key) = ACCUMULATOR
            val t2 = t.copy
            t2 *= d
            map(key) = t2
          case t: Outer2Tensor =>
            stateMap(key) = SINGLE_TENSOR
            t *= d
            map(key) = t
          case t: ReadOnlyTensor =>
            stateMap(key) = ACCUMULATOR
            val t2 = Tensor.newSparse(t)
            t2 += (t,d)
            map(key) = t2
          case t: Tensor =>
            stateMap(key) = ACCUMULATOR
            val t2 = Tensor.newDense(t)
            t2 += (t,d)
            map(key) = t2
        }
    }
  }

  def accumulate(key: Weights, t: Tensor) { accumulate(key, t, 1.0) }
}

class SynchronizedWeightsMapAccumulator(val tensorSet: WeightsMap) extends WeightsMapAccumulator {
  val l = new LocalWeightsMapAccumulator(tensorSet)
  override def accumulate(key: Weights, t: Tensor): Unit = l.synchronized { l.accumulate(key, t) }
  override def accumulate(key: Weights, t: Tensor, factor: Double): Unit = l.synchronized { l.accumulate(key, t, factor) }
}

class SynchronizedDoubleAccumulator extends DoubleAccumulator {
  val l = new LocalDoubleAccumulator()
  def accumulate(t: Double): Unit = { l synchronized { l.accumulate(t) } }
  def combine(ta: Accumulator[Double]): Unit = { l.synchronized { l.combine(ta)}}
}

