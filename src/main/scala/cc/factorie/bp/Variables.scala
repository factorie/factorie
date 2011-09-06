package cc.factorie.bp

import collection.mutable.{Buffer, ArrayBuffer}
import cc.factorie.{DiscreteVariable, DiscreteVar, Variable}

trait BPVariable extends DiscreteVariable {
  def bpDomain: Seq[ValueType] = domain.values

  def domainSize: Int = domain.size

  def message(scores: Seq[Double]): GenericMessage = new DiscreteMessage[ValueType](scores, bpDomain)

  def mergedMessage(scores: scala.collection.Map[Any, Double], defaultScore: Double) = {
    val seqScores = bpDomain.map(x => scores.getOrElse(x, defaultScore))
    message(seqScores)
  }

  def uniformMessage: GenericMessage = UniformMessage

  def deterministicMessage(value: Any): GenericMessage =
    value match {
      case v: ValueType => new DiscreteDeterministicMessage[ValueType](v, bpDomain)
    }
}
