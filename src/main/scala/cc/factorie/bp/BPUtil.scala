package cc.factorie.bp

import cc.factorie.{Variable, DiscreteVariable}

/**
 * @author sameer
 * @date 9/7/11
 */

object BPUtil {

  def message[V <: DiscreteVariable](v: V, scores: Seq[Double]): GenericMessage = new DiscreteMessage[v.ValueType](scores, v.domain.values)

  def uniformMessage[V <: Variable](v: V): GenericMessage = UniformMessage

  def deterministicMessage[V <: DiscreteVariable](v: V, value: Any): GenericMessage =
    value match {
      case valoo: v.ValueType => new DiscreteDeterministicMessage[v.ValueType](valoo, v.domain.values)
    }
}