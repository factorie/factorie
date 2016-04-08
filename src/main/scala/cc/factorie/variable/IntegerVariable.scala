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


/** A Domain whose values are the integers.
    @author Andrew McCallum */
trait IntegerDomain extends Domain {
  type Value = Int
}
/** A Domain whose values are the integers.
    @author Andrew McCallum */
object IntegerDomain extends IntegerDomain

/** An abstract variable with one Int value.  
    @author Andrew McCallum */
trait IntegerVar extends ScalarVar with VarWithDomain {
  type Value = Int
  def value: Int
  def domain: IntegerDomain = IntegerDomain
  def maxIntValue = Int.MaxValue
  def minIntValue = Int.MinValue
  def intValue: Int = value
  final def doubleValue: Double = intValue.toDouble
  override def toString = printName + "(" + intValue + ")"
}

/** An abstract variable with one Int value, which can be modified.
    This trait makes no commitment about how the value is stored.
    @author Andrew McCallum */
trait MutableIntegerVar extends IntegerVar with MutableVar

/** A concrete variable with a mutable Int value.
    @author Andrew McCallum */ 
class IntegerVariable(initialValue:Int = 0) extends MutableIntegerVar with MutableIntScalarVar {
  private var _value: Int = initialValue
  @inline final def value = _value
  @inline final override def intValue = _value
  def set(newValue: Int)(implicit d: DiffList = null): Unit = if (newValue != _value) {
    if (d ne null) d += new IntegerVariableDiff(_value, newValue)
    _value = newValue
  }
  //override def :=(newValue:Int): Unit = set(newValue)(null) // To avoid wrapping the Int when calling the generic method in MutableVar
  def +=(x:Int) = set(_value + x)(null) // Should we allow non-null DiffLists?
  def -=(x:Int) = set(_value - x)(null)
  def *=(x:Int) = set(_value * x)(null)
  def /=(x:Int) = set(_value / x)(null)
  case class IntegerVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntegerVariable = IntegerVariable.this
    @inline final def redo() = _value = newIndex
    @inline final def undo() = _value = oldIndex
    override def toString = "IntegerVariableDiff("+oldIndex+","+newIndex+")"
  }
}
