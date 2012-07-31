/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie
import cc.factorie.la._

trait TensorDomain extends Domain[Tensor]
object TensorDomain extends TensorDomain

trait TensorVar extends Variable with VarAndValueType[TensorVar,Tensor] {
  def domain: TensorDomain
  def tensor: Value
  def length: Int = tensor.length
  def apply(i:Int): Double = tensor.apply(i)
}

// TODO Consider also, just in case needed:
// trait TypedTensorVar[+A<:Tensor] extends TensorVar with VarAndValueType[TypedTensorVar[A],A]
// trait TensorVar extends TypedTensorVar[Tensor]

/** A variable whose value is a cc.factorie.la.Tensor.
    The zero-arg constructor should only be used by subclasses
    (e.g. so that CategoricalVariable can use its domain for value lookup),
    and should never be called by users. */
abstract class TensorVariable extends TensorVar with MutableVar {
  def this(initialValue:Tensor) = { this(); set(initialValue)(null) } 
  //def init(initialValue:Value) = { _set(initialValue) }
  //def domain: TensorDomain = TensorDomain // TODO Really?  What about future DiscreteVariable and CategoricalVariable?
  private var _value: Value = null.asInstanceOf[Value]
  def value: Value = _value // TODO Should this make a copy?
  @inline final def tensor: Value = _value // This method will definitely not make a copy
  // Some methods for direct access
  @inline final override def length: Int = _value.length
  @inline final override def apply(i:Int): Double = _value.apply(i)
  // Why is this necessary?  Why not use set()(null)?  I think there was a reason... -akm
  @inline protected final def _set(newValue:Value): Unit = _value = newValue 
  // Methods that track modifications on a DiffList
  def set(newValue:Value)(implicit d:DiffList): Unit = {
    if (d ne null) d += SetTensorDiff(_value, newValue)
    _value = newValue
  }
  def update(index:Int, newValue:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    tensor.update(index, newValue)
  }
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    if (d ne null) d += IncrementTensorIndexDiff(index, incr)
    tensor.+=(index, incr)
  }
  def increment(incr:Tensor)(implicit d:DiffList): Unit = {
    require(incr.length == tensor.length)
    if (d ne null) d += IncrementTensorDiff(incr)
    tensor += incr
  }
  def zero(implicit d:DiffList): Unit = {
    if (d ne null) d += ZeroTensorDiff(tensor.toArray)
    tensor.zero()
  }
  
  case class SetTensorDiff(oldValue:Value, newValue:Value) extends Diff {
    def variable = TensorVariable.this
    def undo = _value = oldValue
    def redo = _value = newValue
  }
  case class IncrementTensorIndexDiff(index:Int, incr:Double) extends Diff {
    def variable = TensorVariable.this
    def undo = tensor.+=(index, -incr)
    def redo = tensor.+=(index, incr)
  }
  case class IncrementTensorDiff(t: Tensor) extends Diff {
    def variable = TensorVariable.this
    def undo = tensor -= t // Note this relies on Tensor t not having changed.
    def redo = tensor += t
  }
  case class ZeroTensorDiff(prev: Array[Double]) extends Diff {
    def variable = TensorVariable.this
    def undo = tensor += prev
    def redo = tensor.zero()
  }
}


object TensorVariable {
  def apply(dim1:Int) = new TensorVariable(new DenseTensor1(dim1)) { def domain = TensorDomain }
}
