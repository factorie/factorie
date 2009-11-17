package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

// Categorical variables are Discrete variables in which the integers 0...N have each been mapped to some other objects of type ValueType.

// Mapping of changes in mid-November 2009:
// IndexedVariable => CategoricalValues
// SingleIndexed => CategoricalValue
// SingleIndexedVariable => CategoricalVariable
// TypedSingleIndexedObservation => TypedCategoricalValue
// TypedSingleIndexedVariable => TypedCategoricalVariable
// TrueIndexedValue => TrueCategoricalValue
// TypedTrueIndexedValue => TypedTrueCategoricalValue
// PrimitiveVariable => RefVariable
// ItemizedVariable => ItemizedValue

/** For use with variables whose values are mapped to densely-packed integers from 0 and higher, using a CategoricalDomain.
    It can apply to a single int value (as in EnumVariable or CategoricalValue) or a collection of indices (as in BinaryVectorVariable) */
// Semantically "Values" are not really "Variables", but we must inherit from cc.factorie.Variable in order to handle Domain properly
trait CategoricalValues extends Variable with DiscreteValues with TypedValues {
  type VariableType <: CategoricalValues
  type DomainType <: CategoricalDomain[VariableType]
  class DomainClass extends CategoricalDomain[VariableType]
  class DomainInSubclasses
  override def domainSize = domain.allocSize
}

/** A DiscreteValue whose integers 0...N are associated with an categorical objects of type ValueType.
    If you are looking for a concrete implementation, consider EnumObservation or EnumVariable or CoordinatedEnumVariable. */
abstract trait CategoricalValue extends CategoricalValues with TypedValue with DiscreteValue {
  this: Variable =>
  type VariableType <: CategoricalValue
  class DomainInSubclasses
  //def value: ValueType = domain.get(index) // TODO I wanted to define this here, but Scala cannot resolve the right type.
  //override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + index + ")"
} 

/** A DiscreteVariable whose integers 0...N are associated with an object of type ValueType. */
abstract trait CategoricalVariable extends Variable with CategoricalValue with DiscreteVariable {
  type VariableType <: CategoricalVariable
  class DomainInSubclasses
  // final def set(newValue: ValueType)(implicit d: DiffList) = setByIndex(domain.index(newValue)) // TODO I wanted to put this here, but Scala cannot reolve the right type 
}

/** A CategoricalValue variable (which is also a TypedValue), but whose type is specified by a type argument. */
abstract trait TypedCategoricalValue[T] extends CategoricalValue {
  this: Variable =>
  // But doesn't have mixin 'ConstantValue' because we can't yet be guaranteed that this variable's index will not change; we can in EnumObservation, though
  type VariableType <: TypedCategoricalValue[T]
  type ValueType = T
  class DomainInSubclasses
  def value: ValueType = domain.get(index) // TODO I'd love to move this to the superclass, but Scala type system is complaining
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + index + ")"
  // NOTE that "def index" has yet to be defined
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
abstract trait TypedCategoricalVariable[T] extends CategoricalVariable with TypedCategoricalValue[T] {
  type VariableType <: TypedCategoricalVariable[T]
  class DomainInSubclasses
  final def set(newValue: ValueType)(implicit d: DiffList) = setByIndex(domain.index(newValue))
  def :=(newValue:ValueType) = set(newValue)(null)
} 

/** A Variable to hold one of an enumerated set of values of type T, and which does not change.  */
abstract class EnumObservation[T](value:T) extends Variable with TypedCategoricalValue[T] with ConstantValue {
  type VariableType <: EnumObservation[T]
  class DomainInSubclasses
  final val index = domain.index(value)
}

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.
// TODO Really?  Verify how much efficiency gain we could get.
// No.  We can't do this.  For example, belief propagation relies on having no coordination

/** A variable whose value is a single indexed value, initialized at construction time; mutable.
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
}




/*
// TODO We can do better than this now!  It doesn't have to be Categorical
// TODO Perhaps it isn't needed at all, now that we have DiscreteValue?
// TODO Perhaps I should create an IntervalValue, see http://en.wikipedia.org/wiki/Nominal_scale
class IntRangeVariable(low:Int, high:Int) extends TypedCategoricalVariable[Int] {
  type VariableType = IntRangeVariable
  class DomainInSubclasses
  assert(low < high)
  // TODO But note that this will not properly initialize the CategoricalDomain until an instance is created
  if (domain.size == 0) { for (i <- low until high) domain.index(i) }
  assert (domain.size == high-low)
}
// ??? class DiscreteIntervalValue(low:Int, high:Int, bins:Int) extends DiscreteValue {}
*/



// ItemizedVariable support

/** A Variable put in an index, and whose value is the variable itself.  
    For example, you can create 10 'Person extends ItemizedValue[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.value == p1. */
// TODO Since this Variable has constant value, I'm tempted to call it something other than "Variable"
// but I'm not sure what the right alternative.  "ItemizedObservation" seems awkward.
trait ItemizedVariable[This <: ItemizedVariable[This]] extends CategoricalValue {
  this : This =>
  type VariableType = This
  type ValueType = This
  class DomainInSubclasses
  domain.index(this) // Put the variable in the CategoricalDomain
  val index = domain.index(this) // Remember our own index.  We could save memory by looking it up in the Domain each time, but speed is more important
  def value = this
}

/** A variable who value is a pointer to an ItemizedValue; useful for entity-attributes whose value is another variable. */
// TODO Consider renaming to "ItemizedValueRef"
class ItemizedValueRef[V<:ItemizedVariable[V]] extends TypedCategoricalVariable[V] {
  type VariableType = ItemizedValueRef[V]
  class DomainInSubclasses
}



// Domains that count calls to 'index'
/** When mixed in to  */
trait CountingCategoricalDomain[This<:CountingCategoricalDomain[This] with CategoricalValues] {
  this: This =>
  type VariableType = This
  type DomainType = CategoricalDomainWithCounter[This]
  class DomainClass extends CategoricalDomainWithCounter[This]
}

