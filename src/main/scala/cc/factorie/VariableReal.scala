/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** A Variable with a real (double) value. */
trait RealValue extends Variable {
  type VariableType <: RealValue
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  def ===(other: RealValue) = doubleValue == other.doubleValue
  def !==(other: RealValue) = doubleValue != other.doubleValue
  override def toString = printName + "(" + doubleValue.toString + ")"
}

/** A variable class for immutable real (double) values. */
class RealObservation(val doubleValue:Double) extends RealValue with ConstantValue {
  type VariableType <: RealObservation
}

/** A variable class for mutable real (double) values. */
class RealVariable(initialValue: Double = 0.0) extends RealValue {
  type VariableType <: RealVariable
  private var _value: Double = math.NaN_DOUBLE // This should be set to some "impossible" value so that the "set" call below
  set(initialValue)(null)
  @inline final def doubleValue = _value
  def +=(x:Double) = set(_value + x)(null)
  def -=(x:Double) = set(_value - x)(null)
  def *=(x:Double) = set(_value * x)(null)
  def /=(x:Double) = set(_value / x)(null)
  // TODO Consider implementing 'def update' for syntax like val x = RealVariable; x = 3 ???  No.  We have := instead.
  def set(newValue: Double)(implicit d: DiffList): Unit =
    if (newValue != _value) {
      if (d != null) d += new RealDiff(_value, newValue)
      _value = newValue
    }
  def :=(newValue:Double)(implicit d:DiffList = null) = set(newValue) // Go through 'set' so we can do coordination in subclasses.
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}

// TODO, I should create corresponding classes for Integer, Discrete, Ordinal, Categorical and Proportion
// Alternatively, consider removing this class.
/** A RealVariable with a shorter name. */
class Real(initialValue:Double) extends RealVariable(initialValue) {
  type VariableType <: Real
}
