package cc.factorie

import cc.factorie.la._
import scala.collection.mutable

/** An object with a "parameters" method, which returns a WeightsSet holding the multiple Tensors that make up the parameters.
    This trait also provides methods called "Weights" which create new Weights objects that are automatically added to the parameters WeightsSet.
    The most common use-case is "MyModel extends Model with Parameters". */
trait Parameters {
  val parameters: WeightsSet = new WeightsSet

  def Weights(t1: => Tensor1): Weights1 = parameters.newWeights(t1)
  def Weights(t2: => Tensor2): Weights2 = parameters.newWeights(t2)
  def Weights(t3: => Tensor3): Weights3 = parameters.newWeights(t3)
  def Weights(t4: => Tensor4): Weights4 = parameters.newWeights(t4)
}

/** A TensorSet used for holding parameters.
    Here the actually Tensors are the ones stored in the Weights key themselves. */
class WeightsSet extends TensorSet {
  self =>
  private val _keys = mutable.ArrayBuffer[Weights]()

  def keys: Seq[Weights] = _keys
  def tensors: Seq[Tensor] = keys.map(_.value)

  def update(key:Weights, value:Tensor) = key.set(value)
  def apply(key: Weights): Tensor = key.value

  def copy: WeightsMap = { val copyTensor = new WeightsMap(key => key.newBlankTensor); copyTensor += self; copyTensor }
  def blankDenseMap: WeightsMap = new WeightsMap(key => Tensor.newDense(key.value))
  def blankSparseMap: WeightsMap = new WeightsMap(key => Tensor.newSparse(key.value))
  
  // Weights are created here to ensure that they are immediately associate with one and only one WeightsSet.
  def newWeights(ctor: => Tensor): Weights = new Weights with ConcreteWeights with VarWithValue[Tensor] { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor1): Weights1 = new Weights1 with ConcreteWeights { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor2): Weights2 = new Weights2 with ConcreteWeights { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor3): Weights3 = new Weights3 with ConcreteWeights { def newBlankTensor = ctor }
  def newWeights(ctor: => Tensor4): Weights4 = new Weights4 with ConcreteWeights { def newBlankTensor = ctor }

  override def -(other: TensorSet): WeightsMap = { val newT = copy; newT += (other, -1); newT }

  private trait ConcreteWeights extends Weights {
    _keys.append(this)
    private var _value: Value = null.asInstanceOf[Value]
    def value = { if (_value eq null) { _value = newBlankTensor }; _value }
    def set(t: Tensor): Unit = _value = t.asInstanceOf[Value] // TODO I'd love to be able to avoid this cast. -akm
  }
}

/** A TensorSet in which the Tensors themselves are stored in a map inside this object. */
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

/** A collection of Tensors each associated with a Weights key. */
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
  def length = tensors.map(_.length).sum
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

/** A TensorVar that is also used as a key in a TensorSet. */
trait Weights extends TensorVar {
  def newBlankTensor: Value
  def value: Value
  def set(t: Tensor): Unit
  def tensor = value
  def domain = TensorDomain
}

trait Weights1 extends Weights with VarWithValue[Tensor1]
trait Weights2 extends Weights with VarWithValue[Tensor2]
trait Weights3 extends Weights with VarWithValue[Tensor3]
trait Weights4 extends Weights with VarWithValue[Tensor4]

/** A Cubbie for serializing a WeightsSet.  Typically used for saving parameters to disk. */
class WeightsSetCubbie(val ws: WeightsSet) extends Cubbie {
  // we write directly into the WeightsSet so that if we deserialize the weights before the domains, we can give everything the right size from the file
  // This uses indices as keys and so relies on WeightsSet storing its contents in a LinkedHashMap which preserves order
  setMap(new mutable.Map[String, Any] {
    override def update(key: String, value: Any): Unit = {
      if (!value.isInstanceOf[Tensor])
        sys.error("Can't set non-tensor value into weights set cubbie.")
      key.toIntSafe.flatMap(i => ws.keys.indexSafe(i)) match {
        case Some(weights) => weights.set(value.asInstanceOf[Tensor])
        case None => sys.error("unknown key for weights set: " + key)
      }
    }
    def += (kv: (String, Any)): this.type = { update(kv._1, kv._2); this }
    def -= (key: String): this.type = sys.error("Can't remove slots from weights set cubbie!")
    def get(key: String): Option[Any] = key.toIntSafe.flatMap(i => ws.tensors.indexSafe(i))
    def iterator: Iterator[(String, Any)] = ws.tensors.zipWithIndex.map({case (t, i) => i.toString -> t}).iterator
  })
}