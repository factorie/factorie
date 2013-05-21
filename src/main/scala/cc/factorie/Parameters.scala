package cc.factorie

import cc.factorie.la._
import scala.collection.mutable

/** And object containing a WeightsSet member, which is an extensible WeightsMap for holding weights, with factories for dense and sparse copying.
    The most common use-case is "MyModel extends Model with Parameters". For efficiency the weights are stored in the TensorSetKeys themselves,*/

trait Parameters {
  val parameters: WeightsSet = new WeightsSet

  def Weights(t1: => Tensor1): Weights1 = parameters.newWeights(t1)
  def Weights(t2: => Tensor2): Weights2 = parameters.newWeights(t2)
  def Weights(t3: => Tensor3): Weights3 = parameters.newWeights(t3)
  def Weights(t4: => Tensor4): Weights4 = parameters.newWeights(t4)
}

/* This implementation actually stores the weights in the Weights themselves instead
 * of storing them in the map itself. This is just for efficiency, as the API remains the same.
 */
class WeightsSet extends TensorSet {
  self =>
  private val _keys = mutable.ArrayBuffer[Weights]()

  def keys: Seq[Weights] = _keys
  def tensors: Seq[Tensor] = keys.map(_.value)

  def update(key:Weights, value:Tensor) = {
    val actualKey = key.asInstanceOf[InnerKey] // Avoid this casting by allowing Weight.set directly.
    actualKey._actualWeights = value.asInstanceOf[actualKey.Value]
  }
  def apply(key: Weights): Tensor = key.value

  // TODO But these aren't really "copies", they are WeightsMaps.  Rename to blankDenseMap and blankSparseMap
  def copy: WeightsMap = { val copyTensor = blankDenseCopy; copyTensor += self; copyTensor } // TODO Why doesn't this preserve sparse/denseness?
  def blankDenseCopy: WeightsMap = new WeightsMap(key => Tensor.newDense(key.value))
  def blankSparseCopy: WeightsMap = new WeightsMap(key => Tensor.newSparse(key.value))

  // TODO Why not create Weights separately, and then add Weights to the WeightsSet?  This would enable "new TemplateModel(MyTemplate1, MyTemplate2)".
  // TODO Would it be a problem for one Weights to belong to more than one WeightsSet?  I don't think so.
  def newWeights(ctor: => Tensor): Weights = new Weights with InnerKey with VarWithValue[Tensor] { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor1): Weights1 = new Weights1 with InnerKey { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor2): Weights2 = new Weights2 with InnerKey { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor3): Weights3 = new Weights3 with InnerKey { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor4): Weights4 = new Weights4 with InnerKey { def newBlankTensor = ctor }

  override def -(other: TensorSet) = { val newT = copy; newT += (other, -1); newT }

  private trait InnerKey extends Weights {
    _keys.append(this)
    var _actualWeights: Value = null.asInstanceOf[Value]
    def value = { if (_actualWeights eq null) { _actualWeights = newBlankTensor }; _actualWeights }
  }
}

class WeightsMap(defaultTensor: Weights => Tensor) extends TensorSet {
  private val _map = new mutable.LinkedHashMap[Weights, Tensor]
  // Note that for sparse tensor hash sets, "keys" only gives you the keys that have been added thus far
  def keys: Seq[Weights] = _map.keys.toSeq
  def tensors: Seq[Tensor] = _map.values.toSeq
  def apply(key: Weights): Tensor = _map.getOrElseUpdate(key, defaultTensor(key))
  def update(key: Weights, value: Tensor) = _map(key) = value
  def copy: WeightsMap = {
    val c = new WeightsMap(defaultTensor)
    c += this
    c
  }
  override def -(other: TensorSet) = { val newT = copy; newT += (other, -1); newT }
}

trait TensorSet {
  def keys: Seq[Weights]
  def tensors: Seq[Tensor]

  def update(key: Weights, value:Tensor)
  def copy: TensorSet

  def apply(key: Weights): Tensor
//  def apply(key: Weights1): Tensor1 = apply(key: Weights).asInstanceOf[Tensor1]
//  def apply(key: Weights2): Tensor2 = apply(key: Weights).asInstanceOf[Tensor2]
//  def apply(key: Weights3): Tensor3 = apply(key: Weights).asInstanceOf[Tensor3]
//  def apply(key: Weights4): Tensor4 = apply(key: Weights).asInstanceOf[Tensor4]

  def -(other: TensorSet) = { val newT = copy; newT += (other, -1); newT }
  def zero(): Unit = tensors.foreach(_.zero())
  def +=(w: TensorSet, f: Double): Unit = { w.keys.foreach(k => this(k) += (w(k), f))}
  def +=(w: TensorSet): Unit = this += (w, 1.0)
  def dot(w: TensorSet): Double = w.keys.map(k => w(k).dot(this(k))).sum
  def oneNorm: Double = tensors.map(_.oneNorm).sum
  def twoNorm: Double = math.sqrt(twoNormSquared)
  def twoNormSquared: Double = tensors.map(_.twoNormSquared).sum
  def different(w: TensorSet, tolerance: Double): Boolean = keys.exists(k => this(k).different(w(k), tolerance))
  def containsNaN(): Boolean = tensors.exists(_.containsNaN)
  def :=(other: TensorSet): Unit = other.keys.foreach(k => this(k) := other(k))
  def *=(other: Double): Unit = keys.foreach(k => this(k) *= other)
  def toSeq: Seq[(Weights, Tensor)] = keys.zip(tensors)
  def toArray: Array[Double] = {
    val arr = new Array[Double](tensors.map(_.length).sum)
    var offset = 0
    for (t <- tensors) {
      System.arraycopy(t.asArray, 0, arr, offset, t.length)
      offset += t.length
    }
    arr
  }
}

trait Weights extends TensorVar {
  def newBlankTensor: Value
  def value: Value
  def tensor = value
  def domain = TensorDomain
}

trait Weights1 extends Weights with VarWithValue[Tensor1]
trait Weights2 extends Weights with VarWithValue[Tensor2]
trait Weights3 extends Weights with VarWithValue[Tensor3]
trait Weights4 extends Weights with VarWithValue[Tensor4]

@deprecated("Use WeightsSetCubbie.")
class WeightsCubbie(val model: Parameters) extends Cubbie {
  val tensors = new TensorListSlot("tensors")
  tensors := model.parameters.tensors.toSeq // This relies on WeightsMap storing its contents in a LinkedHashMap which preserves order
}

class WeightsSetCubbie(val model: WeightsSet) extends Cubbie {
  val tensors = new TensorListSlot("tensors")
  tensors := model.tensors.toSeq // This relies on WeightsMap storing its contents in a LinkedHashMap which preserves order
}