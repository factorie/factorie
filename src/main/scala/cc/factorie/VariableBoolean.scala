/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** A Variable containing a single Boolean value, which might be mutable or immutable.
    @see BooleanVariable
    @see BooleanObservation
    @author Andrew McCallum */
trait BooleanValue extends DiscreteValue {
  type VariableType <: BooleanValue
  def value = (intValue == 1) // Efficiently avoid a lookup in the domain 
  def booleanValue: Boolean = (intValue == 1)
  def ^(other:BooleanValue):Boolean = value && other.value
  def v(other:BooleanValue):Boolean = value || other.value
  def ==>(other:BooleanValue):Boolean = !value || other.value
  def unary_!(): Boolean = !value
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
  type DomainType <: BooleanDomain[VariableType]
  class DomainClass extends BooleanDomain
  // Domain is not in subclasses:  all BooleanValue variables share the same domain.
}

/** A trait for mutable Boolean variables. 
    @author Andrew McCallum */
class BooleanVariable(initialValue:Boolean = false) extends DiscreteVariable(if (initialValue) 0 else 1) with BooleanValue { 
  type VariableType <: BooleanVariable
  final def set(newValue:Boolean)(implicit d: DiffList) = setByInt(domain.index(newValue))
  def set(newValue:Int)(implicit d: DiffList) = setByInt(newValue)
  final def :=(newValue:Boolean) = set(newValue)(null)
  final def value_=(newValue:Boolean) = set(newValue)(null)
}

/** A trait for variables with immutable Boolean values.
    @author Andrew McCallum */
class BooleanObservation(b:Boolean) extends DiscreteObservation(if (b) 1 else 0) with BooleanValue {
  type VariableType <: BooleanObservation
}

// The next two are versions that take convenient constructor arguments.
// TODO Are we happy with their names?  "Bool"?  Might someone want/expect to simply extend BooleanVariable(myflag) ??

// /** A variable class for boolean values, defined specially for convenience.  
//     If you have several different "types" of booleans, you might want to subclass this to enable type safety checks.
//     This class allowed variable-value coordination by overriding the 'setByIndex' method; by contrast the 'Bool' class does not. */
// class CoordinatedBoolVariable(initialValue: Boolean) extends BooleanVariable {
//   def this() = this(false)
//   type VariableType <: CoordinatedBoolVariable
//   setByIndex(if (initialValue == true) 1 else 0)(null)
// }

// /** A variable class for boolean values, defined specially for convenience.  
//     If you have several different "types" of booleans, you might want to subclass this to enable type safety checks. */
// // TODO Should I rename this BoolVariable for consistency?
// class BoolVariable(b: Boolean) extends CoordinatedBoolVariable(b) with UncoordinatedCategoricalVariable {
//   def this() = this(false)
//   type VariableType <: BoolVariable
// }

// Provide an alias with a shorter name
// TODO Consider removing this for uniformity and simplicity.  We could make up for it by introducing an implicit convertion from scala.Boolean to BooleanObservation.
class Bool(b:Boolean = false) extends BooleanVariable(b) {
  type VariableType <: Bool
  //def this() = this(false)
}
// class CoordinatedBool(b:Boolean) extends CoordinatedBoolVariable(b) {
//   def this() = this(false)
// }


class BooleanDomain[V<:BooleanValue](implicit m:Manifest[V]) extends DiscreteDomain[V] {
  setSize(2)
  freeze
  def apply(index:Int) = index == 1
  def get(index:Int) = index == 1
  def index(entry:Boolean) = if (entry) 1 else 0
  def getIndex(entry:Boolean) = if (entry) 1 else 0
}

object Bool {
  val t = new BooleanObservation(true) // TODO This should be BoolObservation!  Because we wouldn't want t.set(false)!!!
  val f = new BooleanObservation(false)
  def apply(b: Boolean) = if (b) t else f
}
