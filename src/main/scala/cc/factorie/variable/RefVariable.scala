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


/** An abstract variable whose value is a pointer to a Scala object (which may also be a Variable).
    See also ArrowVar and EdgeVar.
    @author Andrew McCallum */
trait RefVar[+A<:AnyRef] extends Var {
  type Value <: A
}

/** An abstract mutable variable whose value is a pointer to a Scala object (which may also be a Variable).
    @author Andrew McCallum */
trait MutableRefVar[A<:AnyRef] extends RefVar[A] with MutableVar {
  type Value = A
}

/** A variable whose value is a pointer to a Scala object (which may also be a Variable).
    See also ArrowVariable and EdgeVariable.
    @author Andrew McCAllum */
class RefVariable[A<:AnyRef](initialValue:A = null) extends MutableRefVar[A] {
  private var _value: A = initialValue
  @inline final def value: A = _value
  def set(newValue:A)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RefVariableDiff(_value, newValue)
    _value = newValue
  }
  override def toString = printName + "(" + (if (value == this) "this" else value.toString) + ")"
  case class RefVariableDiff(oldValue:A, newValue:A) extends Diff {
    def variable: RefVariable[A] = RefVariable.this
    def redo() = _value = newValue
    def undo() = _value = oldValue
  }
}

/** An abstract MutableRefVar variable that also has a target value.
    @author Andrew McCallum */
trait LabeledRefVar[A>:Null<:AnyRef] extends MutableRefVar[A] with LabeledVar {
  this: MutableRefVar[A] =>
  def targetValue: A
  def isUnlabeled = targetValue == null
}

/** A RefVariable that also has a target value, suitable for supervised learning.
    It is marked 'abstract' not because it has missing definitions, but to insist that users
    create subclasses before using it.
    @author Andrew McCallum */
abstract class LabeledRefVariable[A>:Null<:AnyRef](var targetValue:A) extends RefVariable[A](targetValue) with LabeledRefVar[A]
