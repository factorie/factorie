package cc.factorie

import cc.factorie.la.{Tensor, Tensors}

/** And object containing parameter weights of type Tensors (which is a Map[Any,Tensor]).
    The most common use-case is "MyModel extends Model with Weights". */
trait Weights {
  def weights: Tensors
}

class WeightsCubbie(val model:Weights) extends Cubbie {
  val tensors = new TensorListSlot("tensors")
  tensors := model.weights.values.toSeq // This relies on Tensors storing its contents in a LinkedHashMap which preserves order
}
