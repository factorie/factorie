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
}

trait MutableTensorVar extends TensorVar with MutableVar with VarAndValueType[MutableTensorVar,MutableTensor]

/** A variable whose value is a cc.factorie.la.MutableTensor.
    The zero-arg constructor should only be used by subclasses
    (e.g. so that CategoricalVariable can use its domain for value lookup),
    and should never be called by users. */
class TensorVariable extends TensorVar {
  def this(initialValue:Tensor) = { this(); set(initialValue)(null) } 
  //def init(initialValue:Value) = { _set(initialValue) }
  def domain: TensorDomain = TensorDomain // TODO Really?  What about future DiscreteVariable and CategoricalVariable?
  private var _value: ValueType = null.asInstanceOf[ValueType]
  def value: Value = _value // TODO Should this make a copy?
  @inline final def tensor: Value = _value // This method will definitely not make a copy
  // Some methods for direct access
  def length: Int = _value.length
  def apply(i:Int): Double = _value.apply(i)
  // Why is this necessary?  Why not use set()(null)?  I think there was a reason... -akm
  @inline protected final def _set(newValue:ValueType): Unit = _value = newValue 
  // Methods that track modifications on a DiffList
  def set(newValue:ValueType)(implicit d:DiffList): Unit = {
    if (d ne null) d += SetTensorDiff(_value, newValue)
    _value = newValue
  }

  case class SetTensorDiff(oldValue:Tensor, newValue:Tensor) extends Diff {
    def variable = TensorVariable.this
    def undo = _value = oldValue
    def redo = _value = newValue
  }
}

class MutableTensorVariable extends TensorVariable with MutableTensorVar {
  def this(initialValue:MutableTensor) = { this(); _set(initialValue) } 
  // TODO Make Diff-creating versions of the two methods below
  def update(index:Int, newValue:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    tensor.update(index, newValue)
  }
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    tensor.+=(index, incr)
  }
  def increment(incr:Tensor)(implicit d:DiffList): Unit = {
    require(incr.length == tensor.length)
    if (d ne null) throw new Error("Not yet implemented")
    tensor += incr
  }
  def zero(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    tensor.zero()
  }
}

object TensorVariable {
  def apply(dim1:Int) = new TensorVariable(new DenseTensor1(dim1))
}
