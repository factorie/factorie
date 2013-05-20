package cc.factorie.la

import cc.factorie.util.{DoubleAccumulator, LocalDoubleAccumulator, Accumulator}
import cc.factorie.{WeightsMap, TensorSet, Weights}

// TODO why doesn't this implement Accumulator[WeightsMap]? -luke
// answer: it's hard
trait TensorSetAccumulator {
  def accumulate(key: Weights, t: Tensor): Unit
  def accumulate(key: Weights, index: Int, value: Double): Unit
  def accumulate(key: Weights, t: Tensor, factor: Double): Unit
}

class LocalTensorSetAccumulator(val tensorSet: TensorSet) extends TensorSetAccumulator {
  def accumulate(key: Weights, t: Tensor): Unit = tensorSet(key) += t
  def accumulate(key: Weights, index: Int, value: Double): Unit = tensorSet(key)(index) += value
  def accumulate(key: Weights, t: Tensor, factor: Double): Unit = tensorSet(key) += (t, factor)
  def combine(a: TensorSetAccumulator): Unit = a match {
    case a: LocalTensorSetAccumulator => tensorSet += a.tensorSet
  }
}

class SynchronizedTensorSetAccumulator(val tensorSet: TensorSet) extends TensorSetAccumulator {
  val l = new LocalTensorSetAccumulator(tensorSet)
  override def accumulate(key: Weights, t: Tensor): Unit = l.synchronized { l.accumulate(key, t) }
  override def accumulate(key: Weights, index: Int, value: Double): Unit = l.synchronized { l.accumulate(key, index, value) }
  override def accumulate(key: Weights, t: Tensor, factor: Double): Unit = l.synchronized { l.accumulate(key, t, factor) }
}

class SynchronizedDoubleAccumulator extends DoubleAccumulator {
  val l = new LocalDoubleAccumulator()
  def accumulate(t: Double): Unit = { l synchronized { l.accumulate(t) } }
  def combine(ta: Accumulator[Double]): Unit = { l.synchronized { l.combine(ta)}}
}

