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

/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  
    @author Andrew McCallum */
trait IntegerVars extends Variable {
  type VariableType <: IntegerVars
  def maxIntValue = Int.MaxValue
  def minIntValue = Int.MinValue
  // TODO Consider a def maxValue: Double = Math.POS_INF_DOUBLE ??
  // TODO Implement a method def vector?
  // TODO Consider moving activeDomain from DiscreteVars to here? -akm
}

/** A Variable with one Int value.  
    @author Andrew McCallum */
trait IntegerVar extends IntegerVars with NumericValue {
  //this: Variable =>
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
  protected def _set(newValue:Int): Unit = _value = newValue
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
      case cv:CategoricalVar[_/*AnyRef*/] if (oldIndex >= 0) => "IntegerVariableDiff("+cv.domain.get(oldIndex)+"="+oldIndex+","+cv.domain.get(newIndex)+"="+newIndex+")"
      case _ => "IntegerVariableDiff("+oldIndex+","+newIndex+")"
    }
  }
}

/** A Variable with a immutable Int value.
    @author Andrew McCallum */
class IntegerObservation(theValue:Int) extends IntegerVar with ConstantValue {
  type VariableType <: IntegerObservation
  private var _intValue: Int = theValue
  protected def _initializeValue(x:Int): Unit = _intValue = x
  def intValue = _intValue
}

// TODO Consider not distinguishing between Variable and Observation?
