package cc.factorie.la

import cc.factorie.util.{TruncatedArrayIntSeq, Accumulator}
import cc.factorie.DotFamily

trait TensorAccumulator[T <: Tensor] extends Accumulator[T] {
  def accumulate(index: Int, value: Double): Unit
  def accumulate(t:T, factor:Double): Unit
}

trait WeightsTensorAccumulator extends TensorAccumulator[WeightsTensor] {
  def accumulate(family: DotFamily, t: Tensor): Unit
  def accumulate(family: DotFamily, index: Int, value: Double): Unit
  def accumulate(family: DotFamily, t: Tensor, factor: Double): Unit
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1): Unit
}

class LocalWeightsTensorAccumulator(val tensor: WeightsTensor) extends WeightsTensorAccumulator {
  def accumulate(t: WeightsTensor) = tensor += t
  def accumulate(t: WeightsTensor, factor:Double) = tensor.+=(t, factor)
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def accumulate(family: DotFamily, t: Tensor): Unit = tensor(family) += t
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = tensor(family)(index) += value
  def accumulate(family: DotFamily, t: Tensor, factor: Double) = tensor(family).+=(t, factor)
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1): Unit = {
    (tensor(family), t1, t2) match {
      case (_, t1: UniformTensor1, _) if t1(0) == 0.0 => return
      case (_, _, t2: UniformTensor1) if t2(0) == 0.0 => return
      case (myT: DenseTensor2, t1: DenseTensor1, t2: DenseTensor1) =>
        val t2Size = t2.size
        val t1Size = t1.size
        val myTValues = myT.asArray
        val t1Values = t1.asArray
        val t2Values = t2.asArray
        var idx1 = 0
        while (idx1 < t1Size) {
          val v1 = t1Values(idx1)
          val offset = t2Size * idx1
          var idx2 = 0
          while (idx2 < t2Size) {
            val v2 = t2Values(idx2)
            myTValues(offset + idx2) += (v1 * v2)
            idx2 += 1
          }
          idx1 += 1
        }
      case (myT: DenseTensor2, t1: DenseTensor1, t2: SparseIndexedTensor1) =>
        val t2Size = t2.size
        val t1Size = t1.size
        val myTValues = myT.asArray
        val t1Values = t1.asArray
        val t2Indices = t2._indices
        val t2Values = t2._values
        var idx1 = 0
        while (idx1 < t1Size) {
          val v1 = t1Values(idx1)
          val offset = t2Size * idx1
          var t2i = 0
          while (t2i < t2Indices.length) {
            val idx2 = t2Indices(t2i)
            val v2 = t2Values(t2i)
            myTValues(offset + idx2) += (v1 * v2)
            t2i += 1
          }
          idx1 += 1
        }
      case (myT: DenseTensor2, t1: DenseTensor1, t2: SparseBinaryTensorLike1) =>
        val t2Size = t2.size
        val t1Size = t1.size
        val myTValues = myT.asArray
        val t2IndexSeq = t2.activeDomain.asInstanceOf[TruncatedArrayIntSeq]
        val t2Indices = t2IndexSeq.array
        val t1Values = t1.asArray
        var idx1 = 0
        while (idx1 < t1Size) {
          val v1 = t1Values(idx1)
          val offset = t2Size * idx1
          var t2i = 0
          while (t2i < t2IndexSeq.size) {
            val idx2 = t2Indices(t2i)
            myTValues(offset + idx2) += v1
            t2i += 1
          }
          idx1 += 1
        }
      case (myT, _, _) =>
        val t2Size = t2.size
        val t1Iter = t1.activeElements
        while (t1Iter.hasNext) {
          val (idx1, v1) = t1Iter.next()
          val offset = t2Size * idx1
          val t2Iter = t2.activeElements
          while (t2Iter.hasNext) {
            val (idx2, v2) = t2Iter.next()
            myT(offset + idx2) += (v1 * v2)
          }
        }
    }
  }
  def combine(a: Accumulator[WeightsTensor]): Unit = a match {
    case a: LocalWeightsTensorAccumulator => tensor += a.tensor
  }
}

object NoopWeightsTensorAccumulator extends WeightsTensorAccumulator {
  def accumulate(t: WeightsTensor): Unit = {}
  def accumulate(t:WeightsTensor, factor:Double): Unit = {}
  def accumulate(index: Int, value: Double): Unit = {}
  def accumulate(family: DotFamily, t: Tensor): Unit = {}
  def accumulate(family: DotFamily, index: Int, value: Double): Unit = {}
  def combine(a: Accumulator[WeightsTensor]): Unit = {}
  def accumulate(family: DotFamily, t: Tensor, c: Double): Unit = {}
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) = {}
}

