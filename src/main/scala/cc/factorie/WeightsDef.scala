package cc.factorie

import cc.factorie.la._
import scala.collection.mutable

/** And object containing a WeightsSet member, which is an extensible TensorSet for holding weights, with factories for dense and sparse copying.
    The most common use-case is "MyModel extends Model with WeightsDef". For efficiency the weights are stored in the TensorSetKeys themselves,*/

trait WeightsDef {
  val weightsSet: WeightsSet = new WeightsTensorSet

  def Weights(t1: => Tensor1): TensorSetKey1 = weightsSet.add(t1)
  def Weights(t2: => Tensor2): TensorSetKey2 = weightsSet.add(t2)
  def Weights(t3: => Tensor3): TensorSetKey3 = weightsSet.add(t3)
  def Weights(t4: => Tensor4): TensorSetKey4 = weightsSet.add(t4)
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

/* This implementation actually stores the weights in the TensorSetKeys themselves instead
 * of storing them in the map itself. This is just for efficiency, as the API remains the same.
 */
class WeightsTensorSet extends WeightsSet {
  self =>
  private val _keys = mutable.ArrayBuffer[TensorSetKey]()

  def keys: Seq[TensorSetKey] = _keys
  def tensors: Seq[Tensor] = keys.map(_.value)

  def update(key:TensorSetKey, value:Tensor) = {
    val actualKey = key.asInstanceOf[InnerKey]
    actualKey._actualWeights = value.asInstanceOf[actualKey.TensorType]
  }
  def apply(key: TensorSetKey): Tensor = key.value

  def copy: TensorSet = { val copyTensor = blankDenseCopy; copyTensor += self; copyTensor }
  def blankDenseCopy: TensorSet = new HashTensorSet(key => Tensor.newDense(self(key)))
  def blankSparseCopy: TensorSet = new HashTensorSet(key => Tensor.newSparse(self(key)))

  def add(ctor: => Tensor): TensorSetKey = new TensorSetKeySetType with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor1): TensorSetKey1 = new TensorSetKey1 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor2): TensorSetKey2 = new TensorSetKey2 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor3): TensorSetKey3 = new TensorSetKey3 with InnerKey { def newBlankTensor = ctor }
  def add(ctor: => Tensor4): TensorSetKey4 = new TensorSetKey4 with InnerKey { def newBlankTensor = ctor }

  private trait InnerKey extends TensorSetKey {
    def sharedWeights = self
    var _actualWeights: TensorType = null.asInstanceOf[TensorType]
    def realValue = { if (_actualWeights eq null) { _actualWeights = newBlankTensor} ; _actualWeights }
  }
}

class HashTensorSet(defaultTensor: TensorSetKey => Tensor) extends TensorSet {
  private val _map = new mutable.LinkedHashMap[TensorSetKey, Tensor]
  // Note that for sparse tensor hash sets, "keys" only gives you the keys that have been added thus far
  def keys: Seq[TensorSetKey] = _map.keys.toSeq
  def tensors: Seq[Tensor] = _map.values.toSeq
  def apply(key: TensorSetKey): Tensor = _map.getOrElseUpdate(key, defaultTensor(key))
  def update(key: TensorSetKey, value: Tensor) = _map(key) = value
  def copy: TensorSet = {
    val c = new HashTensorSet(defaultTensor)
    c += this
    c
  }
}

class WeightsCubbie(val model: WeightsDef) extends Cubbie {
  val tensors = new TensorListSlot("tensors")
  tensors := model.weightsSet.tensors.toSeq // This relies on TensorSet storing its contents in a LinkedHashMap which preserves order
}