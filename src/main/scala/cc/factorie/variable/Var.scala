/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.variable


// Notes on class names for variables:
// "*Variable" are classes and mutable.
// "*Var" are trait counterparts, not yet committing to (im)mutability.
// Many but not all "*Var" traits do not yet commit to a value-storage mechanism.

// Some classes have a "bound" Value type member; others have an "assigned" (concrete, fixed) Value type member
// "*Variable" classes have assigned Value type
// "*Var" traits have bound Value type members
// "*VarWithValue[Foo]" traits Value type member assigned to Foo
// *Variables that are used as type arguments to Family and Template must have assigned Values in order for the Template to work
// because they must implement methods (such as statistics(Variable1#Value)) that concretely define Value.


/**Abstract superclass of all variables.  Don't need to know its value type to use it.
   <p>
   You should never make a Var a Scala 'case class' because then
   it will get hashCode and equals methods dependent on its
   constructor arguments; but the FACTORIE library depends on being
   able to distinguish individual Var instances based on their
   machine address (i.e. System.identityHashCode).
   <p>
   Similarly no Var should ever inherit from scala.collection.Iterable,
   even though its value may do so.  This prevents confusion between
   Model.factors(Var) and Model.factors(Iterable[Var]).
   @author Andrew McCallum */
trait Var extends Serializable {
  type Value <: Any

  /** Abstract method to return the value of this variable. */
  def value: Value

  /** Value comparisons (as distinct from variable pointer equality) */
  def ===(other: Var) = value == other.value
  def !==(other: Var) = value != other.value
  
  // TODO Is there a more standard way of getting this short name? -akm
  /** Return a string consisting of the class name without its package. */
  private def shortClassName = {
    var fields = this.getClass.getName.split('$')
    if (fields.length == 1)
      fields = this.getClass.getName.split('.')
    if (fields.last == "class")
      fields(fields.length - 2)
    else if ("1234567890".contains(fields.last))
     fields(fields.length-2)
    else
      fields.last
  }
  /** Return the string often used as a prefix of the toString representation of this variable. */
  def printName = shortClassName
  /** Return a short string representation of this variable, suitable for debugging messages. */
  override def toString = printName + "(_)"
}


/** A variable that has a Domain, with value type bound A. 
    Since this trait also inherits from ValueBound, 
    we ensure that the Value of the Var matches the Value of the Domain.
    @author Andrew McCallum */
trait VarWithDomain extends Var {
  /** Abstract method to return the domain of this variable. */
  def domain: Domain
}

// Various marker traits on Var

/** Used as a marker for variables whose value does not change once created.  
    Be  careful to only use this in class definitions that cannot become mutable in subclasses.
    @author Andrew McCallum */
trait VarWithConstantValue extends Var 

/** Used as a marker for variables whose value is a deterministic (non-stochastic) function of some other state,
    for example, a deterministic function of its DirectedModel parents.
    Note that this is an attribute of a variable, not a factor, because it refers to the fact that the
    variable's value changes automatically with changes to the parent variables' values.  How the automatic
    values are scored (whether they are given 0.0 or 1.0 extreme probabilities) is a different matter. 
    This function is used in cc.factorie.directed.DirectedModel.extendedParents and extendedChildren. 
    @author Andrew McCallum */
trait VarWithDeterministicValue extends Var

/** A Variable with a value that can be changed.
    @author Andrew McCallum */
trait MutableVar extends Var {
  def value: Value
  /** Assign a new value to this variable */
  def set(newValue:Value)(implicit d:DiffList): Unit
  final def :=(newValue:Value): Unit = set(newValue)(null)
}

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed.
    This trait may enable efficiencies for sampling, scoring and learning.
    @author Andrew McCallum */
trait NoVariableCoordination {
  this: Var =>
}


// Odd simple variable I wasn't sure where else to put. -akm

/** A Variable whose (constant) value is the Variable object itself.
    @author Andrew McCallum */
trait SelfVariable[This<:SelfVariable[This]] extends Var with VarWithConstantValue {
  this: This =>
  type Value = This
  final def value: This = this
}


