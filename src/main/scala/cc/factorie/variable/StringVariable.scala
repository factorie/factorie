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


/** The domain of StringVar.
    This domain doesn't have special functionality; it exists simply as a marker.
    @author Andrew McCallum */
class StringDomain extends Domain { type Value = String }
object StringDomain extends StringDomain

/** An abstract variable with String value.
    @author Andrew McCallum */
trait StringVar extends VarWithDomain {
  type Value = String
  def domain = StringDomain
  def maxToStringLength = 10
  override def toString = printName + "(" + (if (value.toString.length < maxToStringLength) value else value.toString.take(maxToStringLength)+"...") + ")"
}

/** An abstract variable with mutable String value.
    @author Andrew McCallum */
trait MutableStringVar extends StringVar with MutableVar

/** A variable with mutable String value.
    @author Andrew McCallum */
class StringVariable(initialValue:String) extends MutableStringVar {
  private var _value: String = initialValue
  def value: String = _value
  def set(newValue:String)(implicit d:DiffList): Unit = {
    if (d ne null) d += new StringVariableDiff(_value, newValue)
    _value = newValue
  }
  case class StringVariableDiff(oldString:String, newString:String) extends Diff {
    @inline final def variable: StringVariable = StringVariable.this
    @inline final def redo() = _value = newString
    @inline final def undo() = _value = oldString
    override def toString = "StringVariableDiff("+oldString+","+newString+")"
  }
}
