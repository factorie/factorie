package cc.factorie

import cc.factorie.la._
import scala.collection.mutable

/** And object containing parameter weightsSet of type TensorSet (which is a Map[Any,Tensor]).
    The most common use-case is "MyModel extends Model with WeightsDef". */
class WeightsCubbie(val model: WeightsDef) extends Cubbie {
  val tensors = new TensorListSlot("tensors")
  tensors := model.weightsSet.tensors.toSeq // This relies on TensorSet storing its contents in a LinkedHashMap which preserves order
}

trait WeightsSet extends TensorSet {
  def blankDenseCopy: TensorSet
  def blankSparseCopy: TensorSet
  def add(ctor: => Tensor): TensorSetKey
  def add(ctor: => Tensor1): TensorSetKey1
  def add(ctor: => Tensor2): TensorSetKey2
  def add(ctor: => Tensor3): TensorSetKey3
  def add(ctor: => Tensor4): TensorSetKey4
}

class WeightsSetImpl extends WeightsSet {
  self =>
  private val _tensors = mutable.ArrayBuffer[Tensor]()
  private val _keys = mutable.ArrayBuffer[TensorSetKey]()
  private var _forcedWeights = false

  def tensors = {
    if (! _forcedWeights) {
      for ((t, i) <- _tensors.zipWithIndex)
        if (t == null) { _tensors(i) = _keys(i).newBlankTensor; _tensors(i) } else t
      _forcedWeights = true
    }
    _tensors
  }
  def keys = _keys
  def apply(key:TensorSetKey): Tensor =
    if (_tensors(key.weightsIndex) ne null)
      _tensors(key.weightsIndex)
    else {
      _tensors(key.weightsIndex) = key.newBlankTensor
      _tensors(key.weightsIndex)
    }

  def update(key:TensorSetKey, value:Tensor) = _tensors(key.weightsIndex) = value

  private trait InnerKey extends TensorSetKey {
    def sharedWeights = self
    val weightsIndex = register(this)
  }
  private trait DefaultTensorSetKey extends InnerKey {
    override type TensorType = Tensor
  }
  def add(ctor: => Tensor): TensorSetKey = new DefaultTensorSetKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor1): TensorSetKey1 = new TensorSetKey1 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor2): TensorSetKey2 = new TensorSetKey2 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor3): TensorSetKey3 = new TensorSetKey3 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor4): TensorSetKey4 = new TensorSetKey4 with InnerKey { def newBlankTensor = ctor }

  var lastIndex = -1
  private def register(slot: TensorSetKey): Int = { lastIndex += 1; _keys.append(slot); _tensors.append(null); lastIndex }

  def copy: TensorSet = {
    val copyTensor = blankDenseCopy
    copyTensor += self
    copyTensor
  }
  def blankDenseCopy: TensorSet = new TensorHashSetImpl(key => Tensor.newDense(self(key)))
  def blankSparseCopy: TensorSet = new TensorHashSetImpl(key => Tensor.newSparse(self(key)))
}

trait WeightsDef {
  val weightsSet: WeightsSet = new WeightsSetImpl

  def Weights(t1: => Tensor1): TensorSetKey1 = weightsSet.add(t1)
  def Weights(t2: => Tensor2): TensorSetKey2 = weightsSet.add(t2)
  def Weights(t3: => Tensor3): TensorSetKey3 = weightsSet.add(t3)
  def Weights(t4: => Tensor4): TensorSetKey4 = weightsSet.add(t4)
}

trait TensorHashSet extends TensorSet

class TensorHashSetImpl(defaultTensor: TensorSetKey => Tensor) extends TensorHashSet {
  private val _map = new mutable.LinkedHashMap[TensorSetKey, Tensor]
  // Note that for sparse tensor hash sets, "keys" only gives you the keys that have been added thus far
  def keys: Seq[TensorSetKey] = _map.keys.toSeq
  def tensors: Seq[Tensor] = _map.values.toSeq
  def apply(key: TensorSetKey): Tensor = _map.getOrElseUpdate(key, defaultTensor(key))
  def update(key: TensorSetKey, value: Tensor) = _map(key) = value
  def copy: TensorSet = {
    val c = new TensorHashSetImpl(defaultTensor)
    c += this
    c
  }
}
