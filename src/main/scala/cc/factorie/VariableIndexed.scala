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

  
// IntVariable (has integer value)
// OrdinalVariable (has integer value 0...)
// DiscreteVariable (has a finite number of integer values from 0 ... N) { def domainSize: Int }
// CategoricalVariable (its integer values are mapped to categorical values) (was called "IndexedVariable")
// 
  
/** A Variable with one or more Int values.  It encompasses single integer values, 
    as well as vector collections of many (weighted) int values.  Either way you can get a scalala.tensor.Vector from it. */
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
  type VariableType <: IntValue
  def index: Int
  def intValue = index // TODO consider swapping so "def index = intValue"
  override def toString = printName + "(" + index + ")"
  def ===(other: IntValue) = index == other.index
  def !==(other: IntValue) = index != other.index
}

/** A Variable with a mutable Int value */ 
// TODO Rename CountVariable or OrdinalVariable, or perhaps leave as IntVariable so that it can be a subclass of CategoricalVariable
trait IntVariable extends IntValue {
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
  type VariableType <: OrdinalValues
  override def minIntValue = 0
}
trait OrdinalValue extends OrdinalValues with IntValue {
  type VariableType <: OrdinalValue
}
trait OrdinalVariable extends OrdinalValue with IntVariable {
  type VariableType <: OrdinalVariable
}

/** An OrdinalValue with finite range 0...N */
trait DiscreteValues extends OrdinalValues {
  type VariableType <: DiscreteValues
  type DomainType <: DiscreteDomain[VariableType]
  class MyDomain extends DiscreteDomain[VariableType] { def size = domainSize } // See Domain.scala for an explanation of this silliness
  class DomainClass extends MyDomain
  class DomainInSubclasses
  def domainSize = domain.domainSize
  override def maxIntValue = domainSize - 1
}
trait DiscreteValue extends DiscreteValues with OrdinalValue {
  type VariableType <: DiscreteValue
  def vector = new SingletonBinaryVector(domainSize, index)
}
trait DiscreteVariable extends DiscreteValue with OrdinalVariable with IterableSettings {
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

// Mapping of recent changes:
// IndexedVariable => CategoricalValues
// SingleIndexed => CategoricalValue
// SingleIndexedVariable => CategoricalVariable
// TypedSingleIndexedObservation => TypedCategoricalValue
// TypedSingleIndexedVariable => TypedCategoricalVariable
// TrueIndexedValue => TrueCategoricalValue
// TypedTrueIndexedValue => TypedTrueCategoricalValue

/** For use with variables whose values are mapped to densely-packed integers from 0 and higher, using a CategoricalDomain.
    It can apply to a single index (as in EnumVariable or CategoricalValue) or a collection of indices (as in BinaryVectorVariable) */
trait CategoricalValues extends DiscreteValues with TypedValues {
	type VariableType <: CategoricalValues
	type DomainType <: CategoricalDomain[VariableType]
	class DomainClass extends CategoricalDomain[VariableType]
	class DomainInSubclasses
	override def domainSize = domain.allocSize
}

// TODO Consider making a ConstantSingleIndexedVariable, for use by MixtureComponent
// But how would it be enforced?
// Under the new renaming, it would be called CategoricalObservation, and I think it is possible now.

/** A single-indexed Variable without storage for the index.  Sub-trait CategoricalVariable declares mutable storage for the index.
 * This trait is a generalization of both the (mutable) CategoricalVariable and the (immutable) TypedSingleIndexedObservation
 * If you are looking for a concrete implementation with storage for index, consider EnumObservation or EnumVariable or CoordinatedEnumVariable. */
abstract trait CategoricalValue extends CategoricalValues with DiscreteValue {
	type VariableType <: CategoricalValue
 	class DomainInSubclasses
} 

/** For variables whose values are associated with a an Int from an index. */
abstract trait CategoricalVariable extends CategoricalValue with DiscreteVariable {
	type VariableType <: CategoricalVariable
 	class DomainInSubclasses
	//def propose(model:Model, d: DiffList) = {setByIndex(Global.random.nextInt(domain.size))(d); 0.0} // TODO remove this method
}

/** For variables that can store counts associated with each of their possible values, for use in estimating marginal probabilities from samples. */
// TODO: I'm not sure I like the names of these methods.  Consider changes. 
// TODO I think this should be removed.  All inference meta-data should be stored separately from the Variables in the inferencer itself,
//  because we can't know at Variable creation time which Variables we will want to do inference on and which not.
//  Likely it would be stored in a HashMap[Variable,Array[Double]], or somesuch.
/*@deprecated // Such distributions are now stored separately from variables, in Marginal objects
trait SampleCounts {
  this : CategoricalVariable =>
  val sampleCounts = new Array[Double](domain.allocSize)
  var sampleTotal = 0.0
  def incrementSample(incr:Double) : Unit = { sampleCounts(index) += incr; sampleTotal += incr }
  def incrementSample : Unit = incrementSample(1.0)
  def clearSamples = { sampleTotal = 0.0; for (i <- 0 until domain.allocSize) sampleCounts(i) = 0.0 }
  def samplePr(idx:Int) : Double = sampleCounts(idx)/sampleTotal
  //def samplePr(x:VariableType#ValueType) : Double = samplePr(domain.index(x)) // TODO How can I make this typing work?
}*/

/** For variables put in an index, and whose value is the variable itself. */
// TODO Should this be renamed ItemizedValue?
abstract trait ItemizedVariable[This <: ItemizedVariable[This]] extends CategoricalValue {
	this : This =>
  type VariableType = This
  type ValueType = This
  class DomainInSubclasses
  domain.index(this) // Put the variable in the CategoricalDomain
  val index = domain.index(this) // Remember our own index.  We could save memory by looking it up in the Domain each time, but speed is more important
}

/** A CategoricalValue variable (which is also a TypedValues), but whose type is specified by a type argument. */
abstract trait TypedCategoricalValue[T] extends CategoricalValue {
	// But doesn't have mixin 'ConstantValue' because we can't yet be guaranteed that this variable's index will not change; we can in EnumObservation, though
	type VariableType <: TypedCategoricalValue[T]
  type ValueType = T
  class DomainInSubclasses
  def value: T = domain.get(index) // TODO Should this method be moved to a super class?
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + index + ")"
  // NOTE that "def index" has yet to be defined
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
abstract trait TypedCategoricalVariable[T] extends TypedCategoricalValue[T] with CategoricalVariable {
	type VariableType <: TypedCategoricalVariable[T]
  class DomainInSubclasses
  final def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
	def :=(newValue:T) = set(newValue)(null)
}	

/** A Variable to hold one of an enumerated set of values of type T, and which does not change.  */
abstract class EnumObservation[T](value:T) extends TypedCategoricalValue[T] with ConstantValue {
	type VariableType <: EnumObservation[T]
  class DomainInSubclasses
  final val index = domain.index(value)
}

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.
// TODO Really?  Verify how much efficiency gain we could get.
// No.  We can't do this.  For example, belief propagation relies on having no coordination

/**A variable whose value is a single indexed value, initialized at construction time; mutable.
 This variable does not, however, hold a trueValue.  For that you should use a Label. */
abstract class CoordinatedEnumVariable[T](initialValue:T) extends TypedCategoricalVariable[T] {
	type VariableType <: CoordinatedEnumVariable[T]
  class DomainInSubclasses
	if (initialValue != null) setByIndex(domain.index(initialValue))(null)
}

/** A kind of _EnumVariable that does no variable value coordination in its 'set' method. 
    This trait abstracts over both EnumVariable and Label, and is used in belief probagation 
    and other places that cannot tolerate coordination. */
trait UncoordinatedCategoricalVariable extends CategoricalVariable with NoVariableCoordination {
  // TODO But this does not absolutely guarantee that some other trait hasn't already overriden set and setByIndex to do coordination!
  // TODO I want some way to tell the compiler that this method should be overriding the CategoricalVariable.set method.
	final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method,  
 * ensuring no coordination is necessary for optimization of belief propagation. 
 * This variable does not hold a trueValue; for that you should use a Label. */
abstract class EnumVariable[T](initialValue:T) extends CoordinatedEnumVariable[T](initialValue) with UncoordinatedCategoricalVariable {
	type VariableType <: EnumVariable[T]
  class DomainInSubclasses
  //final override def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
	//final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value. */
//trait TrueIndexedValue[T] extends TypedSingleIndexedVariable[T]
// TODO We could make version of this for OrdinalValue: TrueOrdinalValue
trait TrueCategoricalValue extends TrueSetting {
  this : CategoricalVariable =>
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  var trueIndex: Int
  def trueValue = if (trueIndex >= 0) domain.get(trueIndex) else null
  def setToTruth(implicit d:DiffList): Unit = setByIndex(trueIndex)
  def valueIsTruth: Boolean = trueIndex == index
  def isUnlabeled = trueIndex < 0
  def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex - 1 else throw new Error("Already unlabeled.")
  def relabel = if (trueIndex < 0) trueIndex = -(trueIndex+1) else throw new Error("Already labeled.")
}

// TODO consider moving TrueIndexedValue to inside with this:TrueIndexedValue => ?
abstract trait TypedTrueCategoricalValue[T] extends TrueCategoricalValue with TypedCategoricalVariable[T] {
  class DomainInSubclasses
	def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
}

abstract class TrueCategoricalTemplate[V<:CategoricalVariable with TrueCategoricalValue](implicit m:Manifest[V]) extends TemplateWithVectorStatistics1[V] {
  def score(s:Stat) = if (s.s1.index == s.s1.trueIndex) 1.0 else 0.0
}

class TrueLabelTemplate[V<:CoordinatedLabel[_]](implicit m:Manifest[V]) extends TrueCategoricalTemplate[V]()(m)


/** A variable with a single index and a true value. */
class CoordinatedLabel[T](trueval:T) extends CoordinatedEnumVariable[T](trueval) with TypedTrueCategoricalValue[T] {
	type VariableType <: CoordinatedLabel[T]
  class DomainInSubclasses
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
}

class Label[T](trueval:T) extends CoordinatedLabel(trueval) with UncoordinatedCategoricalVariable {
  type VariableType <: Label[T]
  class DomainInSubclasses
	//override final def set(newValue:T)(implicit d: DiffList) = super.set(newValue)(d)
	//override final def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable class for boolean values, defined here for convenience.  If you have several different "types" of booleans, you might want to subclass this to enable type safety checks. */
class CoordinatedBool(b: Boolean) extends CoordinatedEnumVariable(b) {
  def this() = this(false)
	type VariableType <: CoordinatedBool
	type DomainType <: BoolDomain[VariableType]
  class DomainClass extends BoolDomain
  override def intValue = if (b) Bool.t.index else Bool.f.index
	def ^(other:Bool) = value && other.value
	def v(other:Bool) = value || other.value
	def ==>(other:Bool) = !value || other.value
	override def toString = if (index == 0) printName+"(false)" else printName+"(true)"
}
class Bool(b: Boolean) extends CoordinatedBool(b) with UncoordinatedCategoricalVariable {
  def this() = this(false)
	type VariableType <: Bool
	type DomainType <: BoolDomain[VariableType]
  class DomainClass extends BoolDomain
}
class BoolDomain[V<:Bool] extends CategoricalDomain[V] {
  this += false
  this += true
  this.freeze
}
object Bool {
	val t = new Bool(true)
	val f = new Bool(false)
	def apply(b: Boolean) = if (b) t else f
}


// TODO We can do better than this now!  It doesn't have to be Categorical
class IntRangeVariable(low:Int, high:Int) extends CategoricalVariable {
  type VariableType = IntRangeVariable
  type ValueType = Int
  class DomainInSubclasses
  assert(low < high)
  // TODO But note that this will not properly initialize the CategoricalDomain until an instance is created
  if (domain.size == 0) { for (i <- low until high) domain.index(i) }
  assert (domain.size == high-low)
}

// TODO Is this really necessary?  Who added this? -akm  
// Yes, I think I see its usefulness now.  PrimitiveVariable wouldn't do it because it isn't an IntValue 
/** A variable who value is a pointer to an ItemizedVariable; useful for entity-attributes whose value is another variable. */
// TODO Consider renaming to "ItemizedVariableRef"
class ItemizedVariablePointer[V<:ItemizedVariable[V]] extends TypedCategoricalVariable[V] {
  type VariableType = ItemizedVariablePointer[V]
  class DomainInSubclasses
}
