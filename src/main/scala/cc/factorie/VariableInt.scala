package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

// TODO The separation between "Observation" variables and "Variable" variables is getting a little messy.
// Sometimes Variable means "mutable", as in IntVariable, 
//  but other times is doesn't, as in Variable and IndexedVariable.
// Sometimes Observation means it cannot change (even in subclasses), as in EnumObservation, 
//  but other times it doesn't because the "Variable" form subclasses it, as in IntObservation.
// Think about a clean naming convension.
// Consider the following
// Except for cc.factorie.Variable, "Variable" means mutable
// "Observation" always means immutable, and mixes in ConstantValue
// "Value" is agnostic about whether it is mutable or not.  Hence "IntValue"
// Sounds good.  I went with this scheme in the just completed flurry of renaming.

// But the "Value" name would imply that that this.value would return "Int" for Discrete and Categorical variables
// For "Categorical" is currently returns the CategoricalDomain entry
// Perhaps rename this method this.category
// Alternatively just have a method called this.intValue = this.index, and this.value would still return the Domain entry.
// I've chosen this last option

  
// IntVariable (has integer value)
// OrdinalVariable (has integer value 0...)
// DiscreteVariable (has a finite number of integer values from 0 ... N) { def domainSize: Int }
// CategoricalVariable (its integer values are mapped to categorical values) (was called "IndexedVariable")
// 
  
/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  Either way you can get a scalala.tensor.Vector from it. */
// A "Value" is not really a "Variable", but it turns out to be more convenient shorthand inherit directly instead of insisting with "this:Variable=>"
trait IntValues extends Variable {
  type VariableType <: IntValues
  def maxIntValue = Math.MAX_INT
  def minIntValue = Math.MIN_INT
  // TODO Consider a def maxValue: Double = Math.POS_INF_DOUBLE ??
  def vector: Vector
}

/** A Variable with one Int value.  
    Unlike CategoricalVariable, however, the integers are not necessarily mapped to objects stored in an CategoricalDomain. */
trait IntValue extends IntValues {
  this: Variable =>
  type VariableType <: IntValue
  def index: Int
  def intValue = index // TODO consider swapping so "def index = intValue"
  override def toString = printName + "(" + index + ")"
  def ===(other: IntValue) = index == other.index
  def !==(other: IntValue) = index != other.index
}

/** A Variable with a mutable Int value */ 
// TODO Rename CountVariable or OrdinalVariable, or perhaps leave as IntVariable so that it can be a subclass of CategoricalVariable
trait IntVariable extends Variable with IntValue {
  type VariableType <: IntVariable
  protected var _index = -1
  @inline final def index = _index
  // TODO Consider renaming "setByInt"?
  def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
    // TODO Note that we do not check that (newIndex < domain.size), but perhaps we should; this would slow us down, though!
    if (newIndex < 0) throw new Error("CategoricalVariable setByIndex can't be negative.")
    if (newIndex != _index) {
      if (d != null) d += new IntVariableDiff(_index, newIndex)
      _index = newIndex
    }
  }
  def :=(newIndex:Int) = setByIndex(newIndex)(null)
  case class IntVariableDiff(oldIndex: Int, newIndex: Int) extends Diff {
    @inline final def variable: IntVariable = IntVariable.this
    @inline final def redo = _index = newIndex
    @inline final def undo = _index = oldIndex
  }
}

/** An IntValue with minimum of 0, but no maximum. */
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
    For your own subclass MyDiscreteValue, you can set N with Domain[MyDiscreteValue].size = 9 */
// Semantically "Values" are not really "Variables", but we must inherit from cc.factorie.Variable in order to handle Domain properly
trait DiscreteValues extends Variable with OrdinalValues {
  type VariableType <: DiscreteValues
  type DomainType <: DiscreteDomain[VariableType]
  class MyDomain extends DiscreteDomain[VariableType] { def size = domainSize } // See Domain.scala for an explanation of this silliness
  class DomainClass extends MyDomain
  class DomainInSubclasses
  def domainSize = domain.domainSize
  override def maxIntValue = domainSize - 1
}
trait DiscreteValue extends DiscreteValues with OrdinalValue {
  this: Variable =>
  type VariableType <: DiscreteValue
  def vector = new SingletonBinaryVector(domainSize, index)
}
trait DiscreteVariable extends OrdinalVariable with DiscreteValue with IterableSettings {
  type VariableType <: DiscreteVariable
  // TODO Consider doing a range check on "setByIndex", but it would slow us down, so do a speed/timing check.
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
    //override def variable : CategoricalVariable = CategoricalVariable.this
  }
}

