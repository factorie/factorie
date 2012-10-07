package cc.factorie.la

import cc.factorie.util.Accumulator
import cc.factorie.DotFamily

trait TensorAccumulator extends Accumulator[WeightsTensor] {
  def accumulate(index: Int, value: Double): Unit
  def accumulate(family: DotFamily, t: Tensor): Unit
  def accumulate(family: DotFamily, index: Int, value: Double): Unit
  def += (family: DotFamily, t: Tensor, c: Double): Unit
  def addOuter(family: DotFamily, t1: Tensor1, t2: Tensor1): Unit
}

class LocalTensorAccumulator(val tensor: WeightsTensor) extends TensorAccumulator {
  def accumulate(t: WeightsTensor) = tensor += t
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def accumulate(family: DotFamily, t: Tensor): Unit = tensor(family) += t
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = tensor(family)(index) += value
  def += (family: DotFamily, t: Tensor, c: Double) = tensor(family) +=(t, c)
  def addOuter(family: DotFamily, t1: Tensor1, t2: Tensor1): Unit = {
    if (t1.isUniform && t1(0) == 0.0 || t2.isUniform && t2(0) == 0.0) return
    val myTensor = tensor(family)
    val t2Size = t2.size
    val t1Iter = t1.activeElements
    while (t1Iter.hasNext) {
      val (idx1, v1) = t1Iter.next()
      val offset = t2Size * idx1
      val t2Iter = t2.activeElements
      while (t2Iter.hasNext) {
        val (idx2, v2) = t2Iter.next()
        myTensor(offset + idx2) += (v1 * v2)
      }
    }
  }
  def combine(a: Accumulator[WeightsTensor]): Unit = a match {
    case a: LocalTensorAccumulator => tensor += a.tensor
  }
}

object NoopTensorAccumulator extends TensorAccumulator {
  def accumulate(t: WeightsTensor): Unit = {}
  def accumulate(index: Int, value: Double): Unit = {}
  def accumulate(family: DotFamily, t: Tensor): Unit = {}
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = {}
  def combine(a: Accumulator[WeightsTensor]): Unit = {}
  def += (family: DotFamily, t: Tensor, c: Double): Unit = {}
  def addOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) = {}
}

