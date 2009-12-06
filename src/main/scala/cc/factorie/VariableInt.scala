/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
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
// 
  
/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  Either way you can get a scalala.tensor.Vector from it. 
    @author Andrew McCallum */
// A "Value" is not really a "Variable", but it turns out to be more convenient shorthand inherit directly instead of insisting with "this:Variable=>"
trait IntValues extends Variable {
  type VariableType <: IntValues
  def maxIntValue = Math.MAX_INT
  def minIntValue = Math.MIN_INT
  // TODO Consider a def maxValue: Double = Math.POS_INF_DOUBLE ??
}

/** A Variable with one Int value.  
    Unlike CategoricalVariable, however, the integers are not necessarily mapped to objects stored in an CategoricalDomain. 
    @author Andrew McCallum */
trait IntValue extends IntValues {
  this: Variable =>
  type VariableType <: IntValue
  //def index: Int
  def intValue: Int
  override def toString = printName + "(" + intValue + ")"
  def ===(other: IntValue) = intValue == other.intValue
  def !==(other: IntValue) = intValue != other.intValue
}

/** A Variable with a mutable Int value.
    @author Andrew McCallum */ 
// TODO Rename CountVariable or OrdinalVariable, or perhaps leave as IntVariable so that it can be a subclass of CategoricalVariable
trait IntVariable extends Variable with IntValue {
  type VariableType <: IntVariable
  private var _index = -1 // TODO make this 'private', for efficiency
  @inline final def intValue = _index
  def setByInt(newValue: Int)(implicit d: DiffList): Unit = {
    if (newValue != _index) {
      if (d != null) d += new IntVariableDiff(_index, newValue)
      _index = newValue
    }
  }
  def :=(newIndex:Int) = setByInt(newIndex)(null)
  case class IntVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntVariable = IntVariable.this
    @inline final def redo = _index = newIndex
    @inline final def undo = _index = oldIndex
  }
}

/** A Variable with a immutable Int value.
    @author Andrew McCallum */
class IntObservation(val intValue:Int) extends IntValue 

/** An IntValue with minimum of 0, but no maximum. 
    @author Andrew McCallum */
trait OrdinalValues extends IntValues {
  this: Variable =>
  type VariableType <: OrdinalValues
  override def minIntValue = 0
}
trait OrdinalValue extends OrdinalValues with IntValue {
  this: Variable =>
  type VariableType <: OrdinalValue
}
trait OrdinalVariable extends IntVariable with OrdinalValue {
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
  class MyDomain extends DiscreteDomain[VariableType] { def size = domainSize } // See Domain.scala for an explanation of this silliness
  class DomainClass extends MyDomain
  def domainSize = domain.domainSize
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
  def setRandomly(implicit random:Random, d:DiffList) : Unit = setByIndex(random.nextInt(domainSize))(d)
  def setRandomly(implicit random:Random) : Unit = setByIndex(random.nextInt(domainSize))(null)
  def setRandomly : Unit = setRandomly(cc.factorie.Global.random)
  def settings = new SettingIterator {
    var i = -1
    val max = domain.domainSize - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; setByIndex(i)(d); d }
    def reset = i = -1
    override def variable : DiscreteVariable.this.type = DiscreteVariable.this
  }
}


// TODO Perhaps I should create an IntervalValue, see http://en.wikipedia.org/wiki/Nominal_scale
// ??? class DiscreteIntervalValue(low:Int, high:Int, bins:Int) extends DiscreteValue {}

// TODO I don't especially like this name.  Need to think more generally about names for class versions of the *Variable traits.
/** A DiscreteVariable with initial value set by constructor.  
    Note that you must set the size of your subclasses' DiscreteDomain. */
@DomainInSubclasses
class DiscVariable(initialValue:Int) extends DiscreteVariable {
  type VariableType <: DiscVariable
  setByInt(initialValue)(null)
}


