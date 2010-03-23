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
import cc.factorie.util.{Log}
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
    It can apply to a single int value (as in EnumVariable or CategoricalValue) or a collection of indices (as in BinaryVectorVariable) 
    @author Andrew McCallum */
// Semantically "Values" are not really "Variables", but we must inherit from cc.factorie.Variable in order to handle Domain properly
@DomainInSubclasses
trait CategoricalValues extends Variable with DiscreteValues with TypedValues {
  type VariableType <: CategoricalValues
  type DomainType <: CategoricalDomain[VariableType]
  class DomainClass extends CategoricalDomain[VariableType]()(null)
  override def domainSize = domain.allocSize
}

/** A DiscreteValue whose integers 0...N are associated with an categorical objects of type ValueType.
    If you are looking for a concrete implementation, consider EnumObservation or EnumVariable or CoordinatedEnumVariable. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait CategoricalValue extends CategoricalValues with TypedValue with DiscreteValue {
  this: Variable =>
  type VariableType <: CategoricalValue
  //def value: ValueType = domain.get(index) // TODO I wanted to define this here, but Scala cannot resolve the right type.
  //override def toString = printName + "(" + (if (value == this) "this" else value.toString) + "=" + index + ")"
} 

/** A DiscreteVariable whose integers 0...N are associated with an object of type ValueType. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait CategoricalVariable extends Variable with CategoricalValue with DiscreteVariable {
  type VariableType <: CategoricalVariable
  // final def set(newValue: ValueType)(implicit d: DiffList) = setByIndex(domain.index(newValue)) // TODO I wanted to put this here, but Scala cannot reolve the right type
}

/** A CategoricalValue variable (which is also a TypedValue), but whose type is specified by a type argument. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait TypedCategoricalValue[T] extends CategoricalValue {
  this: Variable =>
  type VariableType <: TypedCategoricalValue[T]
  type ValueType = T
  def value: ValueType = domain.get(intValue) // TODO I'd love to move this to the superclass, but Scala type system is complaining
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + "=" + intValue + ")"
  // NOTE that "def index" has yet to be defined
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait TypedCategoricalVariable[T] extends CategoricalVariable with TypedCategoricalValue[T] {
  type VariableType <: TypedCategoricalVariable[T]
  final def set(newValue: ValueType)(implicit d: DiffList) = setByIndex(domain.index(newValue))
  final def :=(newValue:ValueType) = set(newValue)(null)
  final def value_=(newValue:ValueType) = set(newValue)(null)
} 

/** For variables holding a single, constant indexed value which is of Scala type T. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait TypedCategoricalObservation[T] extends Variable with TypedCategoricalValue[T] with ConstantValue {
  type VariableType <: TypedCategoricalObservation[T]
}
// TODO Consider making this a class with a constructor argument.

/** A Variable to hold one of an enumerated set of values of type T, and which does not change.  
    @author Andrew McCallum */
@DomainInSubclasses
abstract class EnumObservation[T](value:T) extends TypedCategoricalObservation[T] {
  type VariableType <: EnumObservation[T]
  final val intValue = domain.index(value)
}

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.
// TODO Really?  Verify how much efficiency gain we could get.
// No.  We can't do this.  For example, belief propagation relies on having no coordination

/** A variable whose value is a single indexed value, initialized at construction time; mutable.
    This variable does not, however, hold a trueValue.  For that you should use a Label. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract class CoordinatedEnumVariable[T](initialValue:T) extends TypedCategoricalVariable[T] {
  def this() = this(null)
  type VariableType <: CoordinatedEnumVariable[T]
  if (initialValue != null) setByIndex(domain.index(initialValue))(null)
}

/** A kind of _EnumVariable that does no variable value coordination in its 'set' method. 
    This trait abstracts over both EnumVariable and Label, and is used in belief probagation 
    and other places that cannot tolerate coordination. 
    @author Andrew McCallum */
trait UncoordinatedCategoricalVariable extends CategoricalVariable with UncoordinatedDiscreteVariable

/** A variable whose value is a single indexed value that does no variable coordination in its 'set' method,  
    ensuring no coordination is necessary for optimization of belief propagation. 
    This variable does not hold a trueValue; for that you should use a Label.
    @author Andrew McCallum
*/
@DomainInSubclasses
abstract class EnumVariable[T](initialValue:T) extends CoordinatedEnumVariable[T](initialValue) with UncoordinatedCategoricalVariable {
  type VariableType <: EnumVariable[T]
}




// ItemizedObservation support

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.value == p1. 
    @author Andrew McCallum */
@DomainInSubclasses
trait ItemizedObservation[This <: ItemizedObservation[This]] extends TypedCategoricalObservation[This] {
  this : This =>
  type VariableType = This
  domain.index(this) // Put the variable in the CategoricalDomain
  val intValue = domain.index(this) // Remember our own index.  We could save memory by looking it up in the Domain each time, but speed is more important
  override def value = this
}

/** A variable who value is a pointer to an ItemizedObservation.  It is useful for entity-attributes whose value is another variable. 
    @author Andrew McCallum */
@DomainInSubclasses
class ItemizedObservationRef[V<:ItemizedObservation[V]] extends TypedCategoricalVariable[V] {
  type VariableType = ItemizedObservationRef[V]
}



/** When mixed in to a CategoricalVariable or CategoricalObservation, the variable's Domain will count the number of calls to 'index'.  
    Then you can reduce the size of the Domain by calling 'trimBelowCount' or 'trimBelowSize', 
    which will recreate the new mapping from categories to densely-packed non-negative integers. 
    In typical usage you would (1) read in the data, (2) trim the domain, (3) re-read the data with the new mapping, creating variables. 
    @author Andrew McCallum */
trait CountingCategoricalDomain[This<:CountingCategoricalDomain[This] with CategoricalValues] {
  this: This =>
  type VariableType = This
  type DomainType = CategoricalDomainWithCounter[VariableType]
  class DomainClass extends CategoricalDomainWithCounter[VariableType]()(null)
}

