/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** A Variable with a real (double) value. */
trait RealVar extends Variable with TypedValue with NumericValue {
  type VariableType <: RealVar
  type ValueType = Double
  @inline final def value: Double = doubleValue
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  def ===(other: RealVar) = doubleValue == other.doubleValue
  def !==(other: RealVar) = doubleValue != other.doubleValue
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


// TODO, I should create corresponding classes for Integer, Discrete, Ordinal, Categorical and Proportion
// Alternatively, consider removing this class.
/** A RealVariable with a shorter name. */
/*class Real(initialValue:Double) extends RealVariable(initialValue) {
  type VariableType <: Real
}*/

