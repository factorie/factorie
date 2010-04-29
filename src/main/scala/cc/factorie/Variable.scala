/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log}
//import cc.factorie.util.Implicits._

// Notes on class names for Variables:
// Except for cc.factorie.Variable, "*Variable" means mutable
// "*Observation" always means immutable, and mixes in trait ConstantValue
// "*Value" is agnostic about whether it is mutable or not.  Hence "IntValue"
//  Note that in the language of probability theory "value" refers not to the "random variable", but the values it takes on.
//  The way to think about this is that FACTORIE "*Values" are never instantiated on their own;
//  The instantiation reads something like "Variable with IntValue"


/**Abstract superclass of all variables.  Don't need to know its value type to use it. 
   The trait is abstract because you should not instantiate this trait directly, only subclasses.
   <p>
   You should never make a Variable a Scala 'case class' because then it will get hashCode and equals methods
   dependent on its constructor arguments; but the FACTORIE library depends on being able to distinguish individual
   Variable instances based on their address. 
   @author Andrew McCallum */
// TODO Consider adding "extends AnyRef"??
@DomainInSubclasses
trait Variable /* extends AnyRef */ {
  /** The type of this variable, especially used by this Variable's Domain.  Often you can treat this as an approximation to a self-type */
  type VariableType <: Variable
 
  // Domain handling
  /** The type of this.domain and Domain.apply[MyVariable]*/
  type DomainType <: Domain[VariableType]
  /** When a Domain is automatically constructed for this class (in object Domain), it will be the superclass of this inner class. */
  class DomainClass extends Domain[VariableType]()(Manifest.classType[Variable](classOf[Variable]).asInstanceOf[Manifest[VariableType]])
  /** When DomainInSubclasses appears as an inner class of a Variable class, 
      it simply ensures that the library will never create a Domain for this class, only its subclasses.
      If library users create their own new Variable classes, which will be subclassed, and wants each
      subclass to have its own Domain, then those new Variable classes must declare an inner class of this type. */
  final def domain: VariableType#DomainType = Domain.get[VariableType](this.getClass)
  
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
  //def factors(model:Model): Iterable[Factor] = model.factors(this) // TODO Remove this?  Why have two different short ways of doing this?
  def isConstant = false
  // Generic handling of GenerativeObservation knowledge of its 'source' parent // TODO Consider if we really want to do this.
  def _setSource(source:AnyRef)(implicit difflist:DiffList): Unit = {}
  //type SourceType <: AnyRef
  //def source:AnyRef = null //.asInstanceOf[SourceType]
}

/** For variables that support representating of their uncertainty with a distribution Q over their values, 
    for variational inference with an approximate distribution Q.
    @author Andrew McCallum */
trait QDistribution {
  this: Variable =>
  type QType
  def newQ: GenerativeDistribution[VariableType]
}

/** For variables that support integrating out their uncertainty with a distribution Q over their values, 
    for variational inference with an approximate distribution Q.
    @author Andrew McCallum */
trait MarginalDistribution {
  this: Variable =>
  type MarginalType
  def newMarginal: VariableType
}

/*import cc.factorie.er
class VariableGetter[This<:Variable] extends er.Getter[This] {
  def isConstant = getOneWay(v => Bool(v.isConstant))
}*/

/** Used as a marker for Variables whose value does not change once created.  
    Be  careful to only use this in class definitions that cannot become mutable in subclasses. */
// TODO Consider renaming to ConstantValues
// Semantically a "Value" is not really a "Variable", but we must inherit in order to override
trait ConstantValue extends Variable {
  override final def isConstant = true
}

/** For variables whose value has a type, indicated in type ValueType */
trait TypedValues {
  this: Variable =>
  type ValueType
}

/** For A Variable whose value has type ValueType.  
    Typically this is not used to for Values with simple numeric types such as Int and Double.
    Rather, those values are obtained through methods such as intValue and doubleValue. 
    Other variables, such as EnumVariable, have both an Int value and a ValueType value. */
trait TypedValue extends TypedValues {
  this: Variable =>
  type VariableType <: TypedValue
  //def value: ValueType // TODO I wanted to put this here, but then an implementation in CategoricalValue won't type check.
}
// TODO think about whether to get rid of intValue, doubleValue, proportionValue, etc.
//  No, I don't think so.  DiscreteValue.intValue is convenient, and I would like EnumVariable.value to still have type T.
// I considered whether RefValue and TypedValue should be merged, but then I see that we need to 
// distinguish a _Value that stores its value as x:T and one that doesn't (like EnumVariable[T])



// The two traits below may enable efficiencies for sampling, scoring and learning
// But they are currently unused?
// TODO: consider removing them?

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





// TODO Remove this?  It could be useful, though.
/*trait Setting {
  def set(d:DiffList) : Unit
}*/

/** An iterator over changes to the possible world.  
    Could be implemented as changes to one variable, as in IterableSettings, or more complex changes.   
    @author Andrew McCallum */
trait SettingIterator extends Iterator[DiffList] {
  /** Makes the changes to achieve the next configuration in the iteration.  
      Argument d:DiffList is the "context"---the set of Diffs that have already been made; 
      you can check this to avoid re-changing something that has already been changed.  
      This DiffList should not be modified. 
      The method should (optionally) create a new DiffList by calling the method "newDiffList",
      put any changes caused by this method into that DiffList, and return that DiffList. */
  def next(d:DiffList): DiffList 
  /** Makes the changes to achieve the next configuration in the iteration, without any context DiffList of previous changes. */
  def next: DiffList = next(null)
  /** Rewind this iterator back to its initial state, so that the follow call to "next" will produce the first setting. */
  def reset: Unit
  def hasNext: Boolean
  /** If true, calls to "next" will create a new DiffList to describe the changes they made, otherwise "next" will not track the changes, and will return null. */
  var makeNewDiffList = true
  def noDiffList: this.type = { makeNewDiffList = false; this }
  //def noDiffList: SettingIterator = { makeNewDiffList = false; this }
  /** In your implementation of "next" use this method to optionally create a new DiffList, obeying "makeNewDiffList". */
  def newDiffList = if (makeNewDiffList) new DiffList else null
}

/** A Variable that has a SettingIterator, created by calling "settings". 
    @author Andrew McCallum */
trait IterableSettings {
  this: Variable =>
  //protected def _iterableSettingsVariable : This = this // TODO Is there a better way to do this?
  trait SettingIterator extends cc.factorie.SettingIterator {
    //def variable : IterableSettings2.this.type = IterableSettings2.this
    def variable: Variable = IterableSettings.this
  }
  def settings: SettingIterator
}

/** A variable for which the true, correct value is known.  Often used for target variables inferred at training time. 
    @author Andrew McCallum */
trait TrueSetting {
  this: Variable =>
  def setToTruth(implicit d:DiffList): Unit
  def valueIsTruth: Boolean
}

