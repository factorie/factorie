package cc.factorie.bp

import collection.mutable.HashMap
import cc.factorie.{DiscreteDomain, Variable, DiscreteVariable}
import cc.factorie.la.DenseVector

/**
 * @author sameer
 * @date 9/7/11
 */

object BPUtil {

  def message[V <: DiscreteVariable](v: V, scores: Array[Double]): GenericMessage = new DiscreteMessage(v, DenseVector(scores))

  def uniformMessage: GenericMessage = UniformMessage

  def deterministicMessage[V <: Variable](v: V, value: Any): GenericMessage =
    value match {
      case valoo: v.ValueType => new DeterministicMessage[v.ValueType](valoo)
    }
}