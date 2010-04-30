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
    It can apply to a single int value (as in CategoricalValue) or a collection of indices (as in BinaryVectorVariable) 
    @author Andrew McCallum */
// Semantically "Values" are not really "Variables", but we must inherit from cc.factorie.Variable in order to handle Domain properly
@DomainInSubclasses
trait CategoricalValues[T] extends Variable with DiscreteValues with TypedValues with AbstractCategoricalValues {
  type VariableType <: CategoricalValues[T]
  type ValueType = T
  type DomainType <: CategoricalDomain[VariableType]
  class DomainClass extends CategoricalDomain[VariableType]()(null)
  override def domainSize = domain.allocSize
}

/** A CategoricalValues that does not take a type parameter, for use in the definition of CategoricalDomain. */
@DomainInSubclasses
trait AbstractCategoricalValues extends Variable with DiscreteValues with TypedValues {
  type VariableType <: AbstractCategoricalValues
  type DomainType <: CategoricalDomain[VariableType]
  class DomainClass extends CategoricalDomain[VariableType]()(null)
}

/** A DiscreteValue whose integers 0...N are associated with an categorical objects of type ValueType.
    If you are looking for a concrete implementation, consider CategoricalVariable or CategoricalObservation. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait CategoricalValue[T] extends CategoricalValues[T] with DiscreteValue with TypedValue {
  this: Variable =>
  type VariableType <: CategoricalValue[T]
  def value: ValueType = domain.get(intValue) // TODO I wanted to define this here, but Scala cannot resolve the right type.
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + "=" + intValue + ")"
} 

/** A DiscreteVariable whose integers 0...N are associated with an object of type ValueType. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract class CategoricalVariable[T] extends DiscreteVariable with CategoricalValue[T] {
  type VariableType <: CategoricalVariable[T]
  def this(initialValue:T) = { this(); set(initialValue)(null) }
  final def set(newValue: ValueType)(implicit d: DiffList) = setByIndex(domain.index(newValue))
  final def :=(newValue:ValueType) = set(newValue)(null)
  final def value_=(newValue:ValueType) = set(newValue)(null)
}

/** For variables holding a single, constant indexed value which is of Scala type T. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract class CategoricalObservation[T](theValue:T) extends DiscreteObservation(0) with CategoricalValue[T] {
  type VariableType <: CategoricalObservation[T]
  override val intValue = domain.index(theValue)
}

/** When mixed in to a CategoricalVariable or CategoricalObservation, the variable's Domain will count the number of calls to 'index'.  
    Then you can reduce the size of the Domain by calling 'trimBelowCount' or 'trimBelowSize', 
    which will recreate the new mapping from categories to densely-packed non-negative integers. 
    In typical usage you would (1) read in the data, (2) trim the domain, (3) re-read the data with the new mapping, creating variables. 
    @author Andrew McCallum */
trait CountingCategoricalDomain[This<:CountingCategoricalDomain[This] with CategoricalValues[_]] {
  this: This =>
  type VariableType = This
  type DomainType = CategoricalDomainWithCounter[VariableType]
  class DomainClass extends CategoricalDomainWithCounter[VariableType]()(null)
}








// ItemizedObservation support

// TODO Replace this with Catalog?

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.value == p1. 
    @author Andrew McCallum */
@DomainInSubclasses
trait ItemizedObservation[This <: ItemizedObservation[This]] extends CategoricalValue[This] with ConstantValue {
  this: This =>
  type VariableType = This
  domain.index(this) // Put the variable in the CategoricalDomain
  override val intValue = domain.index(this) // Remember our own index.  We could save memory by looking it up in the Domain each time, but speed is more important
  override def value = this
}

/** A variable who value is a pointer to an ItemizedObservation.  It is useful for entity-attributes whose value is another variable. 
    @author Andrew McCallum */
@DomainInSubclasses
class ItemizedObservationRef[V<:ItemizedObservation[V]] extends CategoricalVariable[V] {
  type VariableType = ItemizedObservationRef[V]
}


