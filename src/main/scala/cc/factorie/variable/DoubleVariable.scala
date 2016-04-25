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

/** The type of the domain of DoubleVariables.
    @author Andrew McCallum */
trait DoubleDomain extends Domain {
  type Value = Double
  def minValue = Double.MinValue
  def maxValue = Double.MaxValue
}
/** The domain of DoubleVariables.
    @author Andrew McCallum */
object DoubleDomain extends DoubleDomain

/** A Variable with a real (double) value. 
    If you want a variable that holds a single double but also has a value that inherits from Tensor, then consider RealVar. 
    @author Andrew McCallum */
trait DoubleVar extends ScalarVar {
  type Value = Double
  def domain: DoubleDomain = DoubleDomain
  @inline final def value: Double = doubleValue
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  override def toString = printName + "(" + doubleValue.toString + ")"
}

trait MutableDoubleVar extends DoubleVar with MutableDoubleScalarVar with MutableIntScalarVar with MutableVar {
  override type Value = Double
}

/** A Variable with a mutable Double value.
    @author Andrew McCallum */
class DoubleVariable(initialValue: Double) extends MutableDoubleVar {
  def this() = this(0.0)
  private var _value: Double = initialValue
  @inline final def doubleValue = _value
  def +=(x:Double) = set(_value + x)(null) // Should we allow non-null DiffLists?
  def -=(x:Double) = set(_value - x)(null)
  def *=(x:Double) = set(_value * x)(null)
  def /=(x:Double) = set(_value / x)(null)
  def set(newValue: Double)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new DoubleDiff(_value, newValue)
    _value = newValue
  }
  final def set(newValue:Int)(implicit d:DiffList): Unit = set(newValue.toDouble)
  //override def :=(newValue:Double): Unit = set(newValue)(null) // To avoid wrapping the Double when calling the generic method in MutableVar, but this method is final in MutableVar.
  case class DoubleDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: DoubleVariable = DoubleVariable.this
    def redo() = _value = newValue
    def undo() = _value = oldValue
  }
}
