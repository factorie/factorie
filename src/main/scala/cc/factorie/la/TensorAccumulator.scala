package cc.factorie.la
import cc.factorie.util.Accumulator
import cc.factorie.DotFamily

trait TensorAccumulator extends Accumulator[WeightsTensor] {
  def accumulate(index: Int, value: Double): Unit
  def accumulate(family: DotFamily, t: Tensor): Unit
  def accumulate(family: DotFamily, index: Int, value: Double): Unit
}

class LocalTensorAccumulator(val tensor: WeightsTensor) extends TensorAccumulator {
  def accumulate(t: WeightsTensor) = tensor += t
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def accumulate(family: DotFamily, t: Tensor): Unit = tensor(family) += t
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = tensor(family)(index) += value
  def combine(a:Accumulator[WeightsTensor]): Unit = a match {
    case a:LocalTensorAccumulator => tensor += a.tensor
  }
}

object NoopTensorAccumulator extends TensorAccumulator {
  def accumulate(t: WeightsTensor): Unit = {}
  def accumulate(index: Int, value: Double): Unit = {}
  def accumulate(family: DotFamily, t: Tensor): Unit = {}
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = {}
  def combine(a:Accumulator[WeightsTensor]): Unit = {}
}

