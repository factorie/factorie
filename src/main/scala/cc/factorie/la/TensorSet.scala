package cc.factorie.la

import scala.collection.mutable

trait TensorSet {
  def keys: Seq[TensorSetKey]
  def tensors: Seq[Tensor]
  def apply(key: TensorSetKey): Tensor
  def update(key: TensorSetKey, value:Tensor)
  def copy: TensorSet

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
  def toSeq: Seq[(TensorSetKey, Tensor)] = keys.zip(tensors)
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

trait TensorSetKey {
  type TensorType <: Tensor
  def newBlankTensor: TensorType
  protected def realValue: TensorType
  def value: TensorType = realValue
}

trait TensorSetKeySetType extends TensorSetKey {
  override type TensorType = Tensor
}
trait TensorSetKey1 extends TensorSetKey {
  override type TensorType = Tensor1
  def newBlankTensor: TensorType
  override def value: TensorType = realValue.asInstanceOf[TensorType]
}
trait TensorSetKey2 extends TensorSetKey {
  override type TensorType = Tensor2
  def newBlankTensor: TensorType
  override def value: TensorType = realValue.asInstanceOf[TensorType]
}
trait TensorSetKey3 extends TensorSetKey {
  override type TensorType = Tensor3
  def newBlankTensor: TensorType
  override def value: TensorType = realValue.asInstanceOf[TensorType]
}
trait TensorSetKey4 extends TensorSetKey {
  override type TensorType = Tensor4
  def newBlankTensor: TensorType
  override def value: TensorType = realValue.asInstanceOf[TensorType]
}
