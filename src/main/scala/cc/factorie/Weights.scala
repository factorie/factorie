package cc.factorie

import cc.factorie.la.{Tensor, Tensors}

/**
 * User: apassos
 * Date: 5/1/13
 * Time: 2:52 PM
 */
trait Weights {
  def weights: Tensors
}

class WeightsCubbie(val model:Weights) extends Cubbie {
  val families = new PrimitiveListSlot[Tensor]("families") {}
  families := model.weights.values.toSeq
}
