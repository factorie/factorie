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

/** A Variable with a real (double) value. */
trait RealVar extends Variable with TypedValue with NumericValue with VectorVar {
  type VariableType <: RealVar
  type ValueType = Double
  type DomainType <: RealDomain[VariableType]
  // TODO Replace this mechanism with an Annotation? -akm
  class DomainClass extends RealDomain[VariableType]()(null)
  @inline final def value: Double = doubleValue
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  def ===(other: RealVar) = doubleValue == other.doubleValue
  def !==(other: RealVar) = doubleValue != other.doubleValue
  /** A Vector representation of this Variable's value. */
  def vector: cc.factorie.la.Vector = new cc.factorie.la.SingletonVector(1, 0, doubleValue)
  override def toString = printName + "(" + doubleValue.toString + ")"
  // TODO Consider making a RealDomain class
}

/** A Variable with a mutable real (double) value. */
class RealVariable(initialValue: Double = 0.0) extends RealVar with MutableTypedValue {
  type VariableType <: RealVariable
  private var _value: Double = initialValue
  @inline final def doubleValue = _value
  def +=(x:Double) = set(_value + x)(null) // Should we allow non-null DiffLists?
  def -=(x:Double) = set(_value - x)(null)
  def *=(x:Double) = set(_value * x)(null)
  def /=(x:Double) = set(_value / x)(null)
  def set(newValue: Double)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RealDiff(_value, newValue)
    _value = newValue
  }
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}

/** A Variable with an immutable real (double) value. */
class RealObservation(val doubleValue:Double) extends RealVar with ConstantValue {
  type VariableType <: RealObservation
}
