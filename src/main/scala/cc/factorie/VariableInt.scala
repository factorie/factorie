/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log}
  
// IntVariable (has integer value)
// OrdinalVariable (has integer value 0...)
// DiscreteVariable (has a finite number of integer values from 0 ... N) { def domainSize: Int }
// CategoricalVariable (its integer values are mapped to categorical values) (was called "IndexedVariable")

// Considering adding these Variable naming conventions:
//  akm 31/12/09
// DiscreteVariable is a trait
// Discrete is a class with a constructor argument giving initial value
// DiscreteConstant is a class (inheriting from DiscreteObservation) with a constructor argument (although perhaps no need for both, and Observation -> Constant) 
// UncoordinatedDiscrete is also likewise a class, (although perhaps CoordinatedDiscrete instead, with uncoordinated default)
// then we also have class names:
// Integer
// Ordinal
// Categorical
// Real and RealConstant
// PositiveReal
  
/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  Either way you can get a scalala.tensor.Vector from it. 
    @author Andrew McCallum */
// A "Value" is not really a "Variable", but it turns out to be more convenient shorthand inherit directly instead of insisting with "this:Variable=>"
trait IntegerValues extends Variable {
  type VariableType <: IntegerValues
  def maxIntValue = Math.MAX_INT
  def minIntValue = Math.MIN_INT
  // TODO Consider a def maxValue: Double = Math.POS_INF_DOUBLE ??
}

/** A Variable with one Int value.  
    Unlike CategoricalVariable, however, the integers are not necessarily mapped to objects stored in an CategoricalDomain. 
    @author Andrew McCallum */
trait IntegerValue extends IntegerValues {
  this: Variable =>
  type VariableType <: IntegerValue
  def intValue: Int
  def doubleValue: Double = intValue.toDouble
  override def toString = printName + "(" + intValue + ")"
  def ===(other: IntegerValue) = intValue == other.intValue
  def !==(other: IntegerValue) = intValue != other.intValue
}

/** A Variable with a mutable Int value.
    @author Andrew McCallum */ 
class IntegerVariable extends Variable with IntegerValue {
  type VariableType <: IntegerVariable
  def this(initialValue:Int) = { this(); setByInt(initialValue)(null) }
  private var _value = -1 // This should be a impossible value so that initialization with this(0) will still result in a call to "setByInt"
  @inline final def intValue = _value
  def setByInt(newValue: Int)(implicit d: DiffList): Unit = {
    if (newValue != _value) {
      if (d != null) d += new IntegerVariableDiff(_value, newValue)
      _value = newValue
    }
  }
  // TODO Consider having all := methods return this, so that we can do:  val x = Gaussian(mean, variance) := 2.3
  def :=(newIndex:Int): this.type = { setByInt(newIndex)(null); this } // TODO Consider adding parameter (implicit d:DiffList = null)
  case class IntegerVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntegerVariable = IntegerVariable.this
    @inline final def redo = _value = newIndex
    @inline final def undo = _value = oldIndex
    override def toString = variable match { 
      case cv:CategoricalVariable[AnyRef] if (oldIndex >= 0) => "IntegerVariableDiff("+cv.domain.get(oldIndex)+"="+oldIndex+","+cv.domain.get(newIndex)+"="+newIndex+")"
      case _ => "IntegerVariableDiff("+oldIndex+","+newIndex+")"
    }
  }
}

/** A Variable with a immutable Int value.
    @author Andrew McCallum */
class IntegerObservation(val intValue:Int) extends IntegerValue with ConstantValue {
  type VariableType <: IntegerObservation
}


/** An IntegerValue with minimum of 0, but no maximum. 
    @author Andrew McCallum */
trait OrdinalValues extends IntegerValues {
  this: Variable =>
  type VariableType <: OrdinalValues
  override def minIntValue = 0
}

trait OrdinalValue extends OrdinalValues with IntegerValue {
  this: Variable =>
  type VariableType <: OrdinalValue
}

abstract class OrdinalVariable extends IntegerVariable with OrdinalValue {
  type VariableType <: OrdinalVariable
  def this(initialValue:Int) = { this(); setByInt(initialValue)(null) }
}

abstract class OrdinalObservation(theValue:Int) extends IntegerObservation(theValue) with OrdinalValue {
  type VariableType <: OrdinalObservation
}

