package cc.factorie.la
import cc.factorie.util.Accumulator

trait TensorAccumulator extends Accumulator[Tensor] {
  def accumulate(index: Int, value: Double): Unit
}

class LocalTensorAccumulator(val tensor: Tensor) extends TensorAccumulator {
  def accumulate(t: Tensor) = tensor += t
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def combine(a:Accumulator[Tensor]): Unit = a match {
    case a:LocalTensorAccumulator => tensor += a.tensor
  }
}

object NoopTensorAccumulator extends TensorAccumulator {
  def accumulate(t: Tensor): Unit = {}
  def accumulate(index: Int, value: Double): Unit = {}
  def combine(a:Accumulator[Tensor]): Unit = {}
}

