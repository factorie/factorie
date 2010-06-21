/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie


/** A Variable whose value has type indicated by the type parameter, which must be a scala.AnyRef.
    Scala primitive types such as Int and Double should be stored in specialized variables, 
    such as IntValue and RealValue, respectively. */
// I didn't like previous name "Primitive" because it encourages thinking about Scala primitive types such as Int, 
//  which belong in other Variables, such as IntValue 
// I considered alternatives "Simple"?  "Stored"?  "Basic"?  "Object"?  "Ref"?  
// I think the best choice is "Ref" because it really is a "Ref" to a Scala object, and this reminds the user
//  that they might have to be careful about changes to the contents of the Scala object.
trait RefValue[A<:AnyRef] extends TypedValue {
  this: Variable =>
  type ValueType = A
  def value:A
  def ===(other: RefValue[A]) = value == other.value
  def !==(other: RefValue[A]) = value != other.value
}

abstract class RefObservation[A<:AnyRef](theValue:A) extends Variable with RefValue[A] {
  type VariableType <: RefObservation[A];
  final val value: A = theValue
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + ")"
}

/**A variable with a single mutable (unindexed) value which is of Scala type A. */
// TODO A candidate for Scala 2.8 @specialized
class RefVariable[A<:AnyRef](initialValue:A = null) extends Variable with RefValue[A] {
  type VariableType <: RefVariable[A]
  // TODO  Consider: def this(initval:A)(implicit d:DiffList = null)
  //def this(initval:A) = { this(); set(initval)(null) } // initialize like this because subclasses may do coordination in overridden set()()
  private var _value: A = _
  set(initialValue)(null) // Initialize by calling set because set may do additional work in subclasses
  @inline final def value: A = _value
  def set(newValue:A)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RefDiff(_value, newValue)
    _value = newValue
  }
  def :=(newValue:A): this.type = { set(newValue)(null); this }
  def value_=(newValue:A) = set(newValue)(null)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + ")"
  case class RefDiff(oldValue:A, newValue:A) extends Diff {
    //        Console.println ("new RefDiff old="+oldValue+" new="+newValue)
    def variable: RefVariable[A] = RefVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}

/** For variables that have a true value described by a Scala AnyRef type T. */
trait RefTrueValue[A>:Null<:AnyRef] extends TrueSetting {
  this: RefVariable[A] =>
  def trueValue: A
  def isUnlabeled = trueValue == null
  def setToTruth(implicit d:DiffList): Unit = set(trueValue)
  def valueIsTruth: Boolean = trueValue == value
}

abstract class RefLabel[A>:Null<:AnyRef](var trueValue:A) extends RefVariable[A] with RefTrueValue[A]

