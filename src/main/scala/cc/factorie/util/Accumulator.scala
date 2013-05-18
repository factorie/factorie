package cc.factorie.util

trait Accumulator[A] {
  //def zero(): Unit
  def accumulate(t: A) : Unit
  // TODO Rename this method accumulate!
  def combine(ta: Accumulator[A]): Unit
}

trait DoubleAccumulator extends Accumulator[Double]

class LocalDoubleAccumulator(var value:Double = 0.0) extends DoubleAccumulator {
  def accumulate(t: Double) : Unit = value += t
  def combine(a: Accumulator[Double]): Unit = a match {
    case a: LocalDoubleAccumulator => value += a.value
  }
}

trait DoubleSeqAccumulator extends Accumulator[DoubleSeq] {
  def accumulate(index: Int, value: Double): Unit
}

class LocalDoubleSeqAccumulator(val tensor: MutableDoubleSeq) extends DoubleSeqAccumulator {
  def accumulate(t: DoubleSeq) = tensor += t
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def combine(a: Accumulator[DoubleSeq]): Unit = a match {
    case a: LocalDoubleSeqAccumulator => tensor += a.tensor
  }
}

