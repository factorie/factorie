/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.model

import cc.factorie._
import cc.factorie.la._
import cc.factorie.variable.TensorVar

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
    Here the actual Tensors are the ones stored in the Weights key themselves
    This is used to store the weights (parameter) value themselves. */
class WeightsSet extends TensorSet {
  self =>
  private val _keys = mutable.ArrayBuffer[Weights]()

  def append(key: Weights): Unit = _keys.append(key)

  def keys: Seq[Weights] = _keys
  def tensors: Seq[Tensor] = keys.map(_.value)

  def update(key:Weights, value:Tensor) = key.set(value)
  def apply(key: Weights): Tensor = key.value

  def copy: WeightsMap = { val copyTensor = new WeightsMap(key => key.newBlankTensor); copyTensor += self; copyTensor }
  def blankDenseMap: WeightsMap = new WeightsMap(key => Tensor.newDense(key.value))
  def blankSparseMap: WeightsMap = new WeightsMap(key => Tensor.newSparse(key.value))
  
  // Weights are created here to ensure that they are immediately associate with one and only one WeightsSet.
  def newWeights(ctor: => Tensor): Weights = new Weights with ConcreteWeights {
    type Value = Tensor
    def newBlankTensor = ctor
  }
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
  
  // TODO Consider implementing Tensor.toSparse as an alternative to this new/+=. -akm
  // This would make it easier for users to loop through the weights and decide themselves on a per-Tensor basis which ones they want to make sparse.
  def sparsify(): Unit = for (weights <- _keys if !weights.value.isInstanceOf[util.SparseDoubleSeq] && weights.value.length > 200) {
    val st = weights.value match {
      case t: Tensor2 => new DenseLayeredTensor2(t.dim1, t.dim2, (i) => new SparseIndexedTensor1(i))
      case _ => Tensor.newSparse(weights.value)
    }
    weights.value.foreachActiveElement((i, v) => if (v != 0.0) st += (i,v))
    weights.set(st)
  }

  def densify(): Unit = for (weights <- _keys if !weights.value.isInstanceOf[util.DenseDoubleSeq]) {
    val dt = Tensor.newDense(weights.value)
    dt += weights.value
    weights.set(dt)
  }

  def filter(pred: Weights => Boolean): WeightsSet = {
    val ret = new WeightsSet
    for (k <- keys; if pred(k))
      ret.append(k)
    ret
  }
}

/** A TensorSet in which the Tensors are not stored in the Weights objects, but in a map inside this object.
    This is used to store gradients and expectations---tensors that have the same structure as the WeightsSet, but are not the parameter values themselves. */
class WeightsMap(defaultTensor: Weights => Tensor) extends TensorSet {
  private val _map = new mutable.LinkedHashMap[Weights, Tensor]
  // Note that for sparse tensor hash sets, "keys" only gives you the keys that have been added thus far
  def keys: Iterable[Weights] = _map.keys
  def tensors: Iterable[Tensor] = _map.values
  def containts(key: Weights) = _map.contains(key)
  def clear() = _map.clear()
  def apply(key: Weights): Tensor = _map.getOrElseUpdate(key, defaultTensor(key))
  def update(key: Weights, value: Tensor) = _map(key) = value
  def copy: WeightsMap = {
    val c = new WeightsMap(defaultTensor)
    c += this
    c
  }
  override def -(other: TensorSet) = { val newT = copy; newT += (other, -1); newT }
  def filter(pred: Weights => Boolean): WeightsMap = {
    val ret = new WeightsMap(_.newBlankTensor)
    for ((k, v) <- toSeq; if pred(k))
      ret(k) = v
    ret
  }
}

/** A collection of Tensors each associated with a Weights key. */
trait TensorSet  extends Serializable {
  def keys: Iterable[Weights]
  def tensors: Iterable[Tensor]

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
  def different(w: TensorSet, tolerance: Double): Boolean = tensors.zip(w.tensors).exists(pair => pair._1.different(pair._2, tolerance)) //k => this(k).different(w(k), tolerance))
  def containsNaN(): Boolean = tensors.exists(_.containsNaN)
  def :=(other: TensorSet): Unit = other.keys.foreach(k => this(k) := other(k))
  def *=(other: Double): Unit = keys.foreach(k => this(k) *= other)
  def toSeq: Iterable[(Weights, Tensor)] = keys.zip(tensors)
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
  //def domain = TensorDomain
}

trait Weights1 extends Weights { type Value = Tensor1 }
trait Weights2 extends Weights { type Value = Tensor2 }
trait Weights3 extends Weights { type Value = Tensor3 }
trait Weights4 extends Weights { type Value = Tensor4 }

trait ConstantWeights extends Weights {
  def newBlankTensor: Value = value.blankCopy.asInstanceOf[Value]
  def set(t: Tensor): Unit = sys.error("Weights are constant, can't set.")
}

class ConstantWeights1(val value: Tensor1) extends ConstantWeights with Weights1
class ConstantWeights2(val value: Tensor2) extends ConstantWeights with Weights2
class ConstantWeights3(val value: Tensor3) extends ConstantWeights with Weights3
class ConstantWeights4(val value: Tensor4) extends ConstantWeights with Weights4

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