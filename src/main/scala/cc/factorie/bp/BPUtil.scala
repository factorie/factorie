package cc.factorie.bp

import collection.mutable.HashMap
import cc.factorie.{DiscreteDomain, Variable, DiscreteVariable}

/**
 * @author sameer
 * @date 9/7/11
 */

object BPUtil {

  val dmap = HashMap[DiscreteDomain,HashMap[DiscreteVariable#Value, Int]]()

  def message[V <: DiscreteVariable](v: V, scores: Seq[Double]): GenericMessage = {
    if (!dmap.contains(v.domain)) {
      val dm = HashMap[DiscreteVariable#Value,  Int]()
      v.domain.values.zipWithIndex.foreach(v => dm(v._1) = v._2)
      dmap(v.domain) = dm
    }
    new DiscreteMessage[v.ValueType](scores, v.domain.values, dmap(v.domain))
  }

  def uniformMessage: GenericMessage = UniformMessage

  def deterministicMessage[V <: Variable](v: V, value: Any): GenericMessage =
    value match {
      case valoo: v.ValueType => new DeterministicMessage[v.ValueType](valoo)
    }
}