/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
  
// IntVariable (has integer value) { def intValue: Int }
// DiscreteVariable extends IntVariable (has a finite number of integer values from 0 ... N) { def domainSize: Int }
// CategoricalVariable[A] extends DiscreteVariable (its integer values are mapped to categorical values) { def value:A }
  
/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  
    @author Andrew McCallum */
trait IntegerVars extends Variable {
  type VariableType <: IntegerVars
  def maxIntValue = Math.MAX_INT
  def minIntValue = Math.MIN_INT
  // TODO Consider a def maxValue: Double = Math.POS_INF_DOUBLE ??
  // TODO Implement a method def vector?
}

/** A Variable with one Int value.  
    @author Andrew McCallum */
trait IntegerVar extends IntegerVars with NumericValue {
  this: Variable =>
  type VariableType <: IntegerVar
  def intValue: Int
  final def doubleValue: Double = intValue.toDouble
  override def toString = printName + "(" + intValue + ")"
  def ===(other: IntegerVar) = intValue == other.intValue
  def !==(other: IntegerVar) = intValue != other.intValue
}

/** A Variable with a mutable Int value.
    @author Andrew McCallum */ 
class IntegerVariable(initialValue:Int = 0) extends IntegerVar with MutableIntValue {
  type VariableType <: IntegerVariable
  private var _value = initialValue
  @inline final def intValue = _value
  def set(newValue: Int)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new IntegerVariableDiff(_value, newValue)
    _value = newValue
  }
  case class IntegerVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntegerVariable = IntegerVariable.this
    @inline final def redo = _value = newIndex
    @inline final def undo = _value = oldIndex
    override def toString = variable match { 
      case cv:CategoricalVar[AnyRef] if (oldIndex >= 0) => "IntegerVariableDiff("+cv.domain.get(oldIndex)+"="+oldIndex+","+cv.domain.get(newIndex)+"="+newIndex+")"
      case _ => "IntegerVariableDiff("+oldIndex+","+newIndex+")"
    }
  }
}

/** A Variable with a immutable Int value.
    @author Andrew McCallum */
class IntegerObservation(val intValue:Int) extends IntegerVar with ConstantValue {
  type VariableType <: IntegerObservation
}


