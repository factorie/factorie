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
package cc.factorie.variable

import cc.factorie.util.VectorUtils

/**
 * @author John Sullivan
 */
class DenseDoubleBagVariable(dim:Int=200) extends MutableVar {
  import VectorUtils._
  type Value = Array[Double]
  private var _value: Value = Array.ofDim[Double](dim)
  @inline final def value: Value = _value // This method will definitely not make a copy
  // Methods that track modifications on a DiffList
  def set(newValue:Value)(implicit d:DiffList): Unit = {
    if (d ne null) d += SetVectorDiff(_value, newValue)
    _value = newValue
  }
  def update(index:Int, newValue:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    value.update(index, newValue)
  }
  def add(other:Array[Double])(implicit d:DiffList) {
    if(d != null) d += AddVectorDiff(other)
    value += other
  }
  def remove(other:Array[Double])(implicit d:DiffList) {
    if(d != null) d += RemoveVectorDiff(other)
    value -= other
  }

  def ++ (other:DenseDoubleBagVariable)(implicit d:DiffList):DenseDoubleBagVariable = {
    val n = new DenseDoubleBagVariable
    n.add(this.value)(d)
    n.add(other.value)(d)
    n
  }

  def -- (other:DenseDoubleBagVariable)(implicit d:DiffList):DenseDoubleBagVariable = {
    val n = new DenseDoubleBagVariable
    n.remove(this.value)(d)
    n.remove(other.value)(d)
    n
  }
  case class SetVectorDiff(oldValue:Value, newValue:Value) extends Diff {
    def variable = DenseDoubleBagVariable.this
    def undo() = _value = oldValue
    def redo() = _value = newValue
  }
  case class UpdateVectorDiff(index:Int, oldValue:Double, newValue:Double) extends Diff {
    def variable = DenseDoubleBagVariable.this
    def undo() = _value(index) = oldValue
    def redo() = _value(index) = newValue
  }
  case class AddVectorDiff(t: Array[Double]) extends Diff {
    def variable = DenseDoubleBagVariable.this
    def undo() = _value -= t // Note this relies on Tensor t not having changed.
    def redo() = _value += t
  }
  case class RemoveVectorDiff(t:Array[Double]) extends Diff {
    def variable = DenseDoubleBagVariable.this
    def undo() = _value += t // Note this relies on Tensor t not having changed.
    def redo() = _value -= t
  }
}

