/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._
  
// IntVariable (has integer value)
// OrdinalVariable (has integer value 0...)
// DiscreteVariable (has a finite number of integer values from 0 ... N) { def domainSize: Int }
// CategoricalVariable (its integer values are mapped to categorical values) (was called "IndexedVariable")

// Considering additional Variable naming conventions:
//  akm 31/12/09
// DiscreteVariable is a trait
// Discrete is a class with a constructor argument giving initial value
// DiscreteConstant is a class (inheriting from DiscreteObservation) with a constructor argument (although perhaps no need for both, and Observation -> Constant) 
// UncoordinatedDiscrete is also likewise a class, (although perhaps CoordinatedDiscrete instead, with uncoordinated default)
// then we also have class names:
// Integer
// Ordinal
// Categorical (which will replace the EnumVariable)
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
  //def index: Int
  def intValue: Int
  override def toString = printName + "(" + intValue + ")"
  def ===(other: IntegerValue) = intValue == other.intValue
  def !==(other: IntegerValue) = intValue != other.intValue
}

/** A Variable with a mutable Int value.
    @author Andrew McCallum */ 
trait IntegerVariable extends Variable with IntegerValue {
  type VariableType <: IntegerVariable
  private var _index = -1
  @inline final def intValue = _index
  def setByInt(newValue: Int)(implicit d: DiffList): Unit = {
    if (newValue != _index) {
      if (d != null) d += new IntegerVariableDiff(_index, newValue)
      _index = newValue
    }
  }
  def :=(newIndex:Int) = setByInt(newIndex)(null)
  case class IntegerVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntegerVariable = IntegerVariable.this
    @inline final def redo = _index = newIndex
    @inline final def undo = _index = oldIndex
    override def toString = variable match { 
      case cv:CategoricalVariable if (oldIndex != -1) => "IntegerVariableDiff("+cv.domain.get(oldIndex)+","+cv.domain.get(newIndex)+")"
      case _ => "IntegerVariableDiff("+oldIndex+","+newIndex+")"
    }
  }
}

/** An IntegerVariable class with a constructor argument.  The canonical concrete IntegerVariable. */
class Integer(initialValue:Int) extends IntegerVariable {
  setByInt(initialValue)(null)
}

/** A Variable with a immutable Int value.
    @author Andrew McCallum */
class IntegerObservation(val intValue:Int) extends IntegerValue 

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
trait OrdinalVariable extends IntegerVariable with OrdinalValue {
  type VariableType <: OrdinalVariable
}

/** An OrdinalValue with finite range 0...N.  
    For your own subclass MyDiscreteValue, you can set N=9 with Domain[MyDiscreteValue].size = 9 
    @author Andrew McCallum */
// Semantically "Values" are not really "Variables", but we must inherit from cc.factorie.Variable in order to handle Domain properly
@DomainInSubclasses
trait DiscreteValues extends Variable with OrdinalValues {
  type VariableType <: DiscreteValues
  type DomainType <: DiscreteDomain[VariableType]
  class DomainClass extends DiscreteDomain[VariableType]
  def domainSize: Int = domain.size
  override def maxIntValue = domainSize - 1
  def vector: Vector
}
@DomainInSubclasses
trait DiscreteValue extends DiscreteValues with OrdinalValue {
  this: Variable =>
  type VariableType <: DiscreteValue
  @inline final def index = intValue // simply an alias for intValue 
  def vector = new SingletonBinaryVector(domainSize, intValue)
}
@DomainInSubclasses
trait DiscreteVariable extends OrdinalVariable with DiscreteValue with IterableSettings {
  type VariableType <: DiscreteVariable
  // TODO Consider doing a range check on "setByIndex", but it would slow us down, so do a speed/timing check.
  final override def setByInt(newValue: Int)(implicit d: DiffList): Unit = setByIndex(newValue)(d) 
  def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
    // TODO Note that we do not check that (newIndex < domain.size), but perhaps we should; this would slow us down, though!
    if (newIndex < 0) throw new Error("DiscreteVariable setByIndex can't be negative.")
    super.setByInt(newIndex)(d)
  }
  def setRandomly(random:Random, d:DiffList): Unit = setByIndex(random.nextInt(domainSize))(d)
  def setRandomly(random:Random): Unit = setRandomly(random, null)
  def setRandomly: Unit = setRandomly(cc.factorie.Global.random)
  def settings = new SettingIterator {
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; setByIndex(i)(d); d }
    def reset = i = -1
    override def variable : DiscreteVariable.this.type = DiscreteVariable.this
  }
}

trait UncoordinatedDiscreteVariable extends DiscreteVariable with NoVariableCoordination {
  // TODO But this does not absolutely guarantee that some other trait hasn't already overriden set and setByIndex to do coordination!
  // TODO I want some way to tell the compiler that this method should be overriding the CategoricalVariable.set method.
  final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


// TODO Perhaps I should create an IntervalValue, see http://en.wikipedia.org/wiki/Nominal_scale
// ??? class DiscreteIntervalValue(low:Int, high:Int, bins:Int) extends DiscreteValue {}

// TODO I don't especially like this name.  Need to think more generally about names for class versions of the *Variable traits.
/** A DiscreteVariable with initial value set by constructor.  
    Note that you must set the size of your subclasses' DiscreteDomain. */
@DomainInSubclasses
abstract class DiscVariable(initialValue:Int) extends DiscreteVariable {
  type VariableType <: DiscVariable
  setByInt(initialValue)(null)
}


