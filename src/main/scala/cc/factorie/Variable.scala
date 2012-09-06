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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.reflect.Manifest

// Notes on class names for Variables:
// Except for cc.factorie.Variable, "*Variable" are classes and mutable.
// "*Var" are trait counterparts, not yet committing to a value-storage mechanism or (im)mutability.

/** Provides a covariant member type 'ValueType' in such a way that it can be overridden in subclasses. */
trait ValueType[+VT] {
  type ValueType = VT
  type Value <: VT
}

// TODO Consider
// trait ValueBound[+A] { type Value <: A }
// trait ValueType[A] { type Value = A }

/** Use this trait to refine the ValueType in subclasses of Variable.
    Do not use the ValueType trait directly, because this will not result in
    a proper update to Variable's "Value" member type.
    This trait provides member types "VariableType" and "ValueType" as covariant member types.
    In Variable this supports the member type definition "Value = VariableType#ValueType",
    which magically turns out to be (pseudo?)-invariant. */
trait VarAndValueType[+This<:Variable,+VT] extends ValueType[VT] {
  //this: This => // Crashes under scala-2.9.0.r24003-b20110118020144
  type VariableType = This
}


/**Abstract superclass of all variables.  Don't need to know its value type to use it. 
   <p>
   You should never make a Variable a Scala 'case class' because then
   it will get hashCode and equals methods dependent on its
   constructor arguments; but the FACTORIE library depends on being
   able to distinguish individual Variable instances based on their
   machine address (i.e. System.identityHashCode).
   <p>
   Similarly no Variable should ever inherit from scala.collection.Iterable,
   even though its value may do so.  This prevents confusion between
   Model.factors(Variable) and Model.factors(Iterable[Variable]).
   @author Andrew McCallum */
trait Variable {
  /** The type of this variable, used specially in the definition
      of the member type 'Value'.
      Often you can treat this as an approximation to a self-type */
  type VariableType <: Variable

  /** The type of the value of this variable, as a covariant type. */
  type ValueType <: Any

  /** The type of the value of this variable, as a pseudo-invariant type. */
  type Value <: Any // <: ValueType // Any // = VariableType#ValueType
 
  /** Abstract method to return the domain of this variable. */
  def domain: Domain[Any]

  /** Abstract method to return the value of this variable. */
  def value: Any

  /** Value comparisons (as distinct from variable pointer equality) */
  def ===(other: VariableType) = value == other.value
  def !==(other: VariableType) = value != other.value
  
  /** The type of the variable contained inside this variable.
      Used for handling var-args. 
      @see ContainerVariable */
  type ContainedVariableType <: Variable

  /** Return a collection of other variables that should be unrolled in Templates whenever this variable is unrolled.
      This is typically used in "contained variables" to return "container variables".
      For example, a Span may be part of a Event; when the Span changes the Event should be considered as having changed also, and Template1[Event] will be relevant.
      This mechanism is also used for implementing "var-args" to Templates, as in GeneratedVarTemplate[GeneratedVar, MixtureChoice, Vars[Parameters]].
      See also PyMC's "Containers"? */
  def unrollCascade: Iterable[Variable] = Nil
  
  /** Create a new GenerativeFactor, make it the "parent" generating factor for this variable, 
      and add this new factor to the given model. */
  def ~[V<:this.VariableType](partialFactor:Function1[V,cc.factorie.generative.GenerativeFactor])(implicit model:cc.factorie.generative.MutableGenerativeModel): this.type = {
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
  // TODO Perhaps there should be a "isDeterministic" method in cc.factorie.Factor?  Or should it go in Statistics?  Ug. -akm
  def isDeterministic = false
}


/** Used as a marker for Variables whose value does not change once created.  
    Be  careful to only use this in class definitions that cannot become mutable in subclasses. */
// Semantically a "Value" is not really a "Variable", but we must inherit in order to override
trait VarWithConstantValue extends Variable {
  override final def isConstant = true
}

trait MutableVar[A] extends Variable {
  type Value = A
  def value: A
  /** Assign a new value to this variable */
  def set(newValue:Value)(implicit d:DiffList): Unit
  final def :=(newValue:Value): Unit = set(newValue)(null)
  /** Create a new GenerativeFactor, make it the "parent" generating factor for this variable,
      add this new factor to the given model, 
      and also assign the variable a new value randomly drawn from this factor. */
  def :~[V<:this.VariableType](partialFactor:Function1[V,cc.factorie.generative.GenerativeFactor])(implicit model:cc.factorie.generative.MutableGenerativeModel): this.type = {
    this ~ (partialFactor)
    this.set(model.parentFactor(this).sampledValue.asInstanceOf[this.Value])(null)
    this
  }
}


trait VarWithNumericValue extends Variable {
  def intValue: Int
  def doubleValue: Double
}
trait VarWithMutableIntValue extends VarWithNumericValue {
  def set(newValue:Int)(implicit d:DiffList): Unit
  def intValue_=(newValue:Int)(implicit d:DiffList): Unit = set(newValue)
}
trait VarWithMutableDoubleValue extends VarWithNumericValue {
  def set(newValue:Double)(implicit d:DiffList): Unit
  def doubleValue_=(newValue:Double)(implicit d:DiffList): Unit = set(newValue)
}


/** A Variable whose (constant) value is the Variable object itself. */
trait SelfVariable[This<:SelfVariable[This]] extends Variable with VarAndValueGenericDomain[SelfVariable[This],This] {
  this: This =>
  final def value: This = this
}


// The two traits below may enable efficiencies for sampling, scoring and learning

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed */
trait NoVariableCoordination {
  this: Variable =>
}

/** A marker for Variables that declare themselves to only to rely on their own Factors when they are changed,
    even if changing this variable involves changes to other variables as well. 
    Furthermore the section of Factors must not change depending on the variable's values. */
// TODO Consider removing this because this constraint is very hard to know locally: one variable cannot know all the factors of another.
trait NoFactorCoordination {
  this: Variable =>
} 
// TODO.  No, create the inverse trait: FactorCoordinator?

