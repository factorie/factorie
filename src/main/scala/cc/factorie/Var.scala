/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

// Notes on class names for Variables:
// "*Variable" are classes and mutable.
// "*Var" are trait counterparts, not yet committing to (im)mutability.
// Many but not all "*Var" traits do not yet commit to a value-storage mechanism.

// Some classes have a "bound" Value type member; others have an "assigned" (concrete, fixed) Value type member
// "*Variable" classes have assigned Value type
// "*Var" traits have bound Value type members
// "*VarWithValue[Foo]" traits Value type member assigned to Foo
// *Variables that are used as type arguments to Family and Template must have assigned Values in order for the Template to work
// because they must implement methods (such as statistics(Variable1#Value)) that concretely define Value.


/** Use this trait to refine the Value type member in subclasses of Variable.
    It provides a covariant member type 'Value' in such a way that it can be override in subclasses.
    However, because this type is lower-bounded not assigned, 
    you cannot implement a method with arguments of this type, unless Value is later assigned
    (for example with VarWithValue[A]).
    @author Andrew McCallum */
trait ValueBound[+A] { type Value <: A }

/** Use this trait to refine and set the Value type member in subclasses of Variable.
    It provides a non-variant 'Value' type member in such a way that it cannot be overridden in subclasses.
    Because this type is assigned and fixed,
    you can implement a method with arguments of this type. 
    @author Andrew McCallum */
trait VarWithValue[A] extends ValueBound[A] with Var { override type Value = A }



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
trait Var extends ValueBound[Any] {
 
  /** Abstract method to return the domain of this variable. */
  def domain: Domain[Any]

  /** Abstract method to return the value of this variable. */
  def value: Any

  /** Value comparisons (as distinct from variable pointer equality) */
  def ===(other: Var) = value == other.value
  def !==(other: Var) = value != other.value
        
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
  def printName = shortClassName
  override def toString = printName + "(_)"
}

/** A Variable whose (constant) value is the Variable object itself. */
trait SelfVariable[This<:SelfVariable[This]] extends Var with VarWithValue[This] with VarWithConstantValue {
  this: This =>
  final def value: This = this
}


// Various marker traits on Var

/** Used as a marker for variables whose value does not change once created.  
    Be  careful to only use this in class definitions that cannot become mutable in subclasses. */
trait VarWithConstantValue extends Var 

/** Used as a marker for variables whose value is a deterministic (non-stochastic) function of some other state,
    for example, a deterministic function of its GenerativeModel parents.
    Note that this is an attribute of a variable, not a factor, because it refers to the fact that the
    variable's value changes automatically with changes to the parent variables' values.  How the automatic
    values are scored (whether they are given 0.0 or 1.0 extreme probabilities) is a different matter. 
    This function is used in cc.factorie.generative.GenerativeModel.extendedParents and extendedChildren. */
trait VarWithDeterministicValue extends Var

/** A Variable with a value that can be changed.
    @author Andrew McCallum */
trait MutableVar[A] extends VarWithValue[A] {
  def value: A
  /** Assign a new value to this variable */
  def set(newValue:Value)(implicit d:DiffList): Unit
  final def :=(newValue:Value): Unit = set(newValue)(null)
}

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed.
    This trait may enable efficiencies for sampling, scoring and learning. */
trait NoVariableCoordination {
  this: Var =>
}
