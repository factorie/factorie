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
// "*Var[Foo]" traits Value type member assigned to Foo
// Variables that are type arguments to Family and Template should have assigned Values
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
  
  /** The type of the variable contained inside this variable.
      Used for handling var-args. 
      @see ContainerVariable */
  type ContainedVariableType <: Var

  /** Return a collection of other variables that should be unrolled in Templates whenever this variable is unrolled.
      This is typically used in "contained variables" to return "container variables".
      For example, a Span may be part of a Event; when the Span changes the Event should be considered as having changed also, and Template1[Event] will be relevant.
      This mechanism is also used for implementing "var-args", as in Vars[].
      See also PyMC's "Containers"? */
  def unrollCascade: Iterable[Var] = Nil
  
  /** Create a new GenerativeFactor, make it the "parent" generating factor for this variable, 
      and add this new factor to the given model. */
  def ~[V<:Var](partialFactor: V => cc.factorie.generative.GenerativeFactor)(implicit model:cc.factorie.generative.MutableGenerativeModel): this.type = {
    model += partialFactor(this.asInstanceOf[V])
    this
  }
    
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
  def isConstant = false
  /** Returns true if the value of this parameter is a deterministic (non-stochastic) function of its GenerativeModel parents.
      Note that this is an attribute of a variable, not a factor, because it refers to the fact that the
      variable's value changes automatically with changes to the parent variables' values.  How the automatic
      values are scored (whether they are given 0.0 or 1.0 extreme probabilities) is a different matter. 
      This function is used in cc.factorie.generative.GenerativeModel.extendedParents and extendedChildren.
      */
  def isDeterministic = false  // TODO Perhaps there should be a "isDeterministic" method in cc.factorie.Factor?  Or should it go in Statistics?  Ug. -akm
}


/** Used as a marker for Variables whose value does not change once created.  
    Be  careful to only use this in class definitions that cannot become mutable in subclasses. */
// Semantically a "Value" is not really a "Variable", but we must inherit in order to override
trait VarWithConstantValue extends Var {
  override final def isConstant = true
}

/** A Variable with a value that can be changed.
    @author Andrew McCallum */
trait MutableVar[A] extends VarWithValue[A] {
  def value: A
  /** Assign a new value to this variable */
  def set(newValue:Value)(implicit d:DiffList): Unit
  final def :=(newValue:Value): Unit = set(newValue)(null)
  /** Create a new GenerativeFactor, make it the "parent" generating factor for this variable,
      add this new factor to the given model, 
      and also assign the variable a new value randomly drawn from this factor. */
  def :~[V<:MutableVar[A]](partialFactor: V => cc.factorie.generative.GenerativeFactor)(implicit model:cc.factorie.generative.MutableGenerativeModel): this.type = {
    this ~ (partialFactor)
    this.set(model.parentFactor(this).sampledValue.asInstanceOf[this.Value])(null)
    this
  }
}



/** A Variable whose (constant) value is the Variable object itself. */
trait SelfVariable[This<:SelfVariable[This]] extends Var with VarWithValue[This] with VarWithConstantValue {
  this: This =>
  final def value: This = this
}


// The two traits below may enable efficiencies for sampling, scoring and learning

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed */
trait NoVariableCoordination {
  this: Var =>
}

/** A marker for Variables that declare themselves to only to rely on their own Factors when they are changed,
    even if changing this variable involves changes to other variables as well. 
    Furthermore the section of Factors must not change depending on the variable's values. */
// TODO Consider removing this because this constraint is very hard to know locally: one variable cannot know all the factors of another.
trait NoFactorCoordination {
  this: Var =>
} 
// TODO.  No, create the inverse trait: FactorCoordinator?

