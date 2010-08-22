/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** A CategoricalVars that does not take a type parameter, for use in the definition of CategoricalDomain. */
@DomainInSubclasses
trait AbstractCategoricalVars extends Variable with DiscreteVars with TypedValues {
  type VariableType <: AbstractCategoricalVars
  type CategoryType
  type DomainType <: CategoricalDomain[VariableType]
  class DomainClass extends CategoricalDomain[VariableType]()(null)
}

/** For use with variables whose values are mapped to densely-packed integers from 0 and higher, using a CategoricalDomain.
    It can apply to a single Int value (as in CategoricalVariable) or a collection of indices (as in BinaryVectorVariable).
    All instances of such a subclass share the same domain. 
    @author Andrew McCallum */
@DomainInSubclasses
trait CategoricalVars[T] extends DiscreteVars with AbstractCategoricalVars with TypedValues {
  type VariableType <: CategoricalVars[T]
  type ValueType = T
  type CategoryType = ValueType // Just a redundant name for ValueType, in case later I want to use ValueType for something else
  //type DomainType <: CategoricalDomain[VariableType]
  //class DomainClass extends CategoricalDomain[VariableType]()(null)
  //override def domainSize = domain.size // TODO Why was this 'allocSize' and not 'size'? -akm 5 July 2010
}

/** A DiscreteVar whose integers 0...N are associated with an categorical objects of type A.
    Concrete implementations include CategoricalVariable and CategoricalObservation. 
    @author Andrew McCallum */
@DomainInSubclasses
abstract trait CategoricalVar[A] extends CategoricalVars[A] with DiscreteVar with TypedValue {
  type VariableType <: CategoricalVar[A]
  def value: A = domain.get(intValue)
  //final def categoryValue: A = value // TODO Include this too?  Nice, clear name, but two redundant methods could be confusing.
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + "=" + intValue + ")"
} 

/** A DiscreteVariable whose integers 0...N are associated with an object of type A. 
    @author Andrew McCallum */
// TODO try to pass something like this to DiscreteVariable constructor: Domain.get[CategoricalVar[A]](this.getClass).index(initialValue)
@DomainInSubclasses
abstract class CategoricalVariable[A] extends DiscreteVariable(0) with CategoricalVar[A] with MutableTypedValue {
  type VariableType <: CategoricalVariable[A]
  //def this(initialValue:A) = this(Domain.get[CategoricalVariable[A]](this.getClass).index(initialValue))
  def this(initialValue:A) = { this(); set(initialValue)(null) }
  def set(newValue:A)(implicit d: DiffList): Unit = set(domain.index(newValue))
  //final def category_=(newValue:A)(implicit d:DiffList = null) = set(newValue)
}

/** For variables holding a single, constant indexed value which is of Scala type T. 
    @author Andrew McCallum */
@DomainInSubclasses
// TODO!!!! This doesn't work because "getClass" here returns scala.Predef!
abstract class CategoricalObservation[A](theValue:A) extends DiscreteObservation(0) with CategoricalVar[A] {
  type VariableType <: CategoricalObservation[A]
  initializeIntValue(domain.index(theValue))
}

/** When mixed in to a CategoricalVariable or CategoricalObservation, the variable's Domain will count the number of calls to 'index'.  
    Then you can reduce the size of the Domain by calling 'trimBelowCount' or 'trimBelowSize', 
    which will recreate the new mapping from categories to densely-packed non-negative integers. 
    In typical usage you would (1) read in the data, (2) trim the domain, (3) re-read the data with the new mapping, creating variables. 
    @author Andrew McCallum */
trait CountingCategoricalDomain[This<:CountingCategoricalDomain[This] with CategoricalVars[_]] {
  this: This =>
  type VariableType = This
  type DomainType = CategoricalDomainWithCounter[VariableType]
  class DomainClass extends CategoricalDomainWithCounter[VariableType]()(null)
}








// ItemizedObservation support

// TODO Replace this with Catalog?
// But then we couldn't use syntax like:  Domain[Person].size

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.value == p1. 
    @author Andrew McCallum */
@DomainInSubclasses
trait ItemizedObservation[This <: ItemizedObservation[This]] extends CategoricalVar[This] with ConstantValue {
  this: This =>
  type VariableType = This
  // Put the variable in the CategoricalDomain and remember it.
  // We could save memory by looking it up in the Domain each time, but speed is more important
  val intValue = domain.index(this) 
  override def value = this
}

/** A variable who value is a pointer to an ItemizedObservation.  It is useful for entity-attributes whose value is another variable. 
    @author Andrew McCallum */
/*@DomainInSubclasses
class ItemizedObservationRef[V<:ItemizedObservation[V]](v:V) extends CategoricalVariable[V](v) {
  type VariableType = ItemizedObservationRef[V]
}
*/
