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

/** Typically Variable instances hold their value internally.
    Alternatively, variable values can be stored in an Assignment: a
    mapping from variables to their values.
    
    Note that this trait doesn't inherit directly from scala.collection.Map
    because we need a special type signature for 'apply' and 'get'.
    @author Andrew McCallum */
trait TypedAssignment[A<:Variable] {
  //def variables: Seq[A]
  def variables: Seq[A]
  def apply[B<:A](v:B): B#Value
  def get[B<:A](v:B): Option[B#Value]
  def contains(v:A): Boolean
  def getOrElse[B<:A](v:B, default: => B#Value): B#Value = if (contains(v)) apply(v) else default
  //def globalize: DiffList
}

trait Assignment extends TypedAssignment[Variable]


/** An Assignment in which variable-value mappings can be changed.
    @author Andrew McCallum */
trait MutableAssignment extends Assignment {
  def update[V<:Variable](variable:V, value:V#Value): Unit
}

/** A MutableAssignment backed by a HashMap.
    @author Andrew McCallum */
class MapAssignment extends MutableAssignment {
  private val map = new scala.collection.mutable.HashMap[Variable,Any]
  val _variables = map.keys.toSeq
  def variables = _variables
  def apply[V<:Variable](v:V): V#Value = map(v).asInstanceOf[V#Value]
  def get[V<:Variable](v:V): Option[V#Value] = map.get(v).map(_.asInstanceOf[V#Value])
  def update[V<:Variable](variable:V, value:V#Value): Unit = map(variable) = value
  def contains(v:Variable) = map.contains(v)
}

/** An efficient Assignment of one variable. */
class Assignment1[A<:Variable](val variable:A, var value:A#Value) extends TypedAssignment[A] {
  def variables = List(variable)
  def apply[B<:A](v:B): B#Value = if (v eq variable) value.asInstanceOf[B#Value] else null.asInstanceOf[B#Value]
  def get[B<:A](v:B): Option[B#Value] = if (v eq variable) Some(value.asInstanceOf[B#Value]) else None
  def contains(v:A): Boolean = if (v eq variable) true else false
}

/** An efficient Assignment of two variables. */
class Assignment2[A<:Variable,B<:Variable](val var1:A, var value1:A#Value, val var2:B, value2:B#Value) extends Assignment {
  def variables = Array(var1, var2)
  def apply[C<:Variable](v:C): C#Value = if (v eq var1) value1.asInstanceOf[C#Value] else if (v eq var2) value2.asInstanceOf[C#Value] else null.asInstanceOf[C#Value]
  def get[C<:Variable](v:C): Option[C#Value] = if (v eq var1) Some(value1.asInstanceOf[C#Value]) else if (v eq var2) Some(value2.asInstanceOf[C#Value]) else None
  def contains(v:Variable): Boolean = if ((v eq var1) || (v eq var2)) true else false
}


/** An Assignment whose values are those stored inside the variables themselves. 
    @author Andrew McCallum */
object GlobalAssignment extends Assignment {
  def variables = throw new Error("Cannot list all variables of the global Assignment.")
  def apply[V<:Variable](v:V): V#Value = v.value
  def get[V<:Variable](v:V): Option[V#Value] = Some(v.value)
  def contains(v:Variable) = true
}

/** An Assignment backed by a sequence of assignment.  
    The returned value will be from the first Assignment in the sequence to contain the variable. 
    @author Andrew McCallum */
class AssignmentStack(val assignment:Assignment, val next:AssignmentStack = null) extends Assignment {
  def variables = assignment.variables ++ next.variables
  protected def apply[V<:Variable](v:V, s:AssignmentStack): V#Value =
    if (s.next eq null) s.assignment.apply(v)
    else s.assignment.getOrElse(v, apply(v, s.next))
  def apply[V<:Variable](v:V): V#Value = apply(v, this)
  protected def get[V<:Variable](v:V, s:AssignmentStack): Option[V#Value] = {
    val o = assignment.get(v)
    if (o != None) o
    else if (s.next ne null) s.next.get(v)
    else None
  }
  def get[V<:Variable](v:V): Option[V#Value] = get(v, this)
  def contains(v:Variable) = if (assignment.contains(v)) true else next.contains(v)
  /** Returns a new Assignment stack, the result of prepending Assignment a. */
  def +:(a:Assignment): AssignmentStack = new AssignmentStack(a, this)
}


