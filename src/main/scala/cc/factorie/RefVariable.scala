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

/** A Variable whose value is a pointer to a Scala object (which may also be a Variable) */
trait RefVar[A<:AnyRef] extends Variable with VarAndValueGenericDomain[RefVar[A],A] 

/** A variable whose value is a pointer to a Scala object (which may also be a Variable).
    See also ArrowVariable and EdgeVariable. */
class RefVariable[A<:AnyRef](initialValue:A = null) extends RefVar[A] with MutableVar[A] {
  private var _value: A = initialValue
  @inline final def value: A = _value
  def set(newValue:A)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RefVariableDiff(_value, newValue)
    _value = newValue
  }
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + ")"
  case class RefVariableDiff(oldValue:A, newValue:A) extends Diff {
    //        Console.println ("new RefVariableDiff old="+oldValue+" new="+newValue)
    def variable: RefVariable[A] = RefVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}

/** For variables that have a true value described by a Scala AnyRef type T. */
trait LabeledRefVar[A>:Null<:AnyRef] extends LabeledVar {
  this: RefVariable[A] =>
  def targetValue: A
  def isUnlabeled = targetValue == null
  def valueIsTarget: Boolean = targetValue == value
}

abstract class LabeledRefVariable[A>:Null<:AnyRef](var targetValue:A) extends RefVariable[A](targetValue) with LabeledRefVar[A]

