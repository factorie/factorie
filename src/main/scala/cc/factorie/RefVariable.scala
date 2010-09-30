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

/** A Variable whose value has type indicated by the type parameter, which must be a scala.AnyRef.
    Scala primitive types such as Int and Double should be stored in specialized variables, 
    such as IntegerVar and RealVar, respectively. */
trait RefVar[A<:AnyRef] extends TypedValue {
  this: Variable =>
  type ValueType = A
  def value:A
  def abstractValue: AnyRef = value
  def ===(other: RefVar[A]) = value == other.value
  def !==(other: RefVar[A]) = value != other.value
}

abstract class RefObservation[A<:AnyRef](theValue:A) extends Variable with RefVar[A] {
  type VariableType <: RefObservation[A];
  final val value: A = theValue
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + ")"
}

/**A variable with a single mutable (unindexed) value which is of Scala type A. */
// TODO A candidate for Scala 2.8 @specialized
class RefVariable[A<:AnyRef](initialValue:A = null) extends Variable with RefVar[A] {
  type VariableType <: RefVariable[A]
  // TODO  Consider: def this(initval:A)(implicit d:DiffList = null)
  //def this(initval:A) = { this(); set(initval)(null) } // initialize like this because subclasses may do coordination in overridden set()()
  private var _value: A = initialValue // was _
  //set(initialValue)(null) // Used to initialize by calling set because set may do additional work in subclasses, e.g. ParameterRef.
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
trait RefTrueVar[A>:Null<:AnyRef] extends TrueSetting {
  this: RefVariable[A] =>
  def trueValue: A
  def isUnlabeled = trueValue == null
  def setToTruth(implicit d:DiffList): Unit = set(trueValue)
  def valueIsTruth: Boolean = trueValue == value
}

abstract class RefLabel[A>:Null<:AnyRef](var trueValue:A) extends RefVariable[A] with RefTrueVar[A]

