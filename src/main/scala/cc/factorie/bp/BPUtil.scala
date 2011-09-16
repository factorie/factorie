package cc.factorie.bp

import cc.factorie.{Variable, DiscreteVariable}

/**
 * @author sameer
 * @date 9/7/11
 */

object BPUtil {

  def message[V <: DiscreteVariable](v: V, scores: Seq[Double]): GenericMessage = new DiscreteMessage[v.ValueType](scores, v.domain.values)

  def uniformMessage: GenericMessage = UniformMessage

  def deterministicMessage[V <: Variable](v: V, value: Any): GenericMessage =
    value match {
      case valoo: v.ValueType => new DeterministicMessage[v.ValueType](valoo)
    }
}