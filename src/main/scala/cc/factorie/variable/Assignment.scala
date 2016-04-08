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

import cc.factorie.model._

/** Typically Variable instances hold their value internally.
    Alternatively, variable values can be stored in an Assignment: a
    mapping from variables to their values.
    
    Note that this trait doesn't inherit directly from scala.collection.Map
    because we need a special type signature for 'apply' and 'get'.
    @author Andrew McCallum */
trait Assignment {
  /** All variables with values in this Assignment */
  def variables: Iterable[Var]
  /** Return the value assigned to variable v, or throw an Error if the variable is not in this Assignment. */
  def apply(v:Var): v.Value
  /** Return the an Option for the value assigned to variable v.  If v is not contained in this Assignment return None. */
  def get(v:Var): Option[v.Value]
  /** Return true if this Assignment has a value for variable v. */
  def contains(v:Var): Boolean
  /** Set variables to the values specified in this assignment */
  def setVariables(implicit d:DiffList): Unit = {
    for (v <- variables) v match {
      case vv:MutableVar => vv.set(this(vv))
      case _ => throw new Error
    }
  }
}

/** An Assignment in which variable-value mappings can be changed.
    @author Andrew McCallum */
trait MutableAssignment extends Assignment {
  def update[V<:Var](variable:V, value:V#Value): Unit
}

/** For LabeledVar return the targetValue, otherwise return the current global assignment.
    @author Andrew McCallum */
object TargetAssignment extends Assignment {
  def variables = throw new Error("Cannot list all variables of the TargetAssignment.")
  def apply(v:Var): v.Value = v match {
    case vv:LabeledVar => vv.target.value.asInstanceOf[v.Value]
    case vv:Var => v.value
  }
  def get(v:Var): Option[v.Value] = Some(apply(v))
  def contains(v:Var) = true
  override def setVariables(implicit d:DiffList): Unit = throw new Error("Cannot set a TargetAssignment.  Instead use variables.setToTarget(DiffList).")
}

/** A MutableAssignment backed by a HashMap.
    @author Andrew McCallum */
class HashMapAssignment(val ignoreNonPresent: Boolean=true) extends MutableAssignment {
  private val map = new scala.collection.mutable.HashMap[Var, Any]
  def this(variables:Var*) = { this(ignoreNonPresent=true); variables.foreach(v => update(v, v.value.asInstanceOf[v.Value])) }
  def this(variables:Iterable[Var]) = { this(ignoreNonPresent=true); variables.foreach(v => update(v, v.value.asInstanceOf[v.Value])) }
  def variables = map.keys
  def apply(v:Var): v.Value = { get(v) match { case Some(va) => va; case None => if (!ignoreNonPresent) throw new Error("Variable not in assignment: " + v) else v.value } }
  def get(v:Var): Option[v.Value] = if (ignoreNonPresent) map.get(v).map(_.asInstanceOf[v.Value]) else Some(map.getOrElse(v, v.value).asInstanceOf[v.Value])
  def update[V<:Var](variable:V, value:V#Value): Unit = map(variable) = value
  def contains(v:Var) = map.contains(v)
}

/** An efficient abstract Assignment of one variable.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
trait AbstractAssignment1[A<:Var] extends Assignment {
  def _1: A
  def value1: A#Value
  def variables = Seq(_1) // TODO Consider making this a Set.
  def apply(v:Var): v.Value = if (v eq _1) value1.asInstanceOf[v.Value] else v.value
  def get(v:Var): Option[v.Value] = if (v eq _1) Some(value1.asInstanceOf[v.Value]) else None
  def contains(v:Var): Boolean = if (v eq _1) true else false
  override def setVariables(implicit d:DiffList): Unit = _1 match { case v:MutableVar => v.set(apply(v)) }
}

/** An efficient Assignment of one variable.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
class Assignment1[A<:Var](val _1:A, var value1:A#Value) extends AbstractAssignment1[A]

/** An efficient Assignment of one DiscreteVar.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
class DiscreteAssignment1[A<:DiscreteVar](override val _1:A, initialIntValue1:Int) extends AbstractAssignment1[A] with MutableAssignment {
  def this(variable:A, initialValue:A#Value) = this(variable, initialValue.intValue)
  private var _intValue1 = initialIntValue1
  def intValue1: Int = _intValue1
  def intValue1_=(i:Int): Unit = _intValue1 = i
  def value1: A#Value = _1.domain(_intValue1).asInstanceOf[A#Value]
  def value1_=(v:A#Value): Unit = _intValue1 = v.intValue
  //def update[V<:Var, U<:V#Value](variable:V, value:U): Unit = if (variable eq _1) _intValue1 = value.asInstanceOf[DiscreteValue].intValue else throw new Error("Cannot update DiscreteAssignment1 value for variable not present.")
  def update[V<:Var](variable:V, value:V#Value): Unit = if (variable eq _1) _intValue1 = value.asInstanceOf[DiscreteValue].intValue else throw new Error("Cannot update DiscreteAssignment1 value for variable not present.")
}

/** An efficient abstract Assignment of two variables.
    Values for variables not in this assigment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
trait AbstractAssignment2[A<:Var,B<:Var] extends Assignment {
  def _1: A
  def _2: B
  def value1: A#Value
  def value2: B#Value
  def variables = Seq(_1, _2)
  def apply(v:Var): v.Value = if (v eq _1) value1.asInstanceOf[v.Value] else if (v eq _2) value2.asInstanceOf[v.Value] else v.value // throw new Error("Variable not present: "+v)
  def get(v:Var): Option[v.Value] = if (v eq _1) Some(value1.asInstanceOf[v.Value]) else if (v eq _2) Some(value2.asInstanceOf[v.Value]) else None
  def contains(v:Var): Boolean = if ((v eq _1) || (v eq _2)) true else false
  override def setVariables(implicit d:DiffList): Unit = {
    _1 match { case v:MutableVar => v.set(value1.asInstanceOf[v.Value]) }
    _2 match { case v:MutableVar => v.set(value2.asInstanceOf[v.Value]) }
  }
}
/** An efficient Assignment of two variables.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
class Assignment2[A<:Var,B<:Var](val _1:A, var value1:A#Value, val _2:B, var value2:B#Value) extends AbstractAssignment2[A,B]

/** An efficient abstract Assignment of three variables.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
trait AbstractAssignment3[A<:Var,B<:Var,C<:Var] extends Assignment {
  def _1: A
  def _2: B
  def _3: C
  def value1: A#Value
  def value2: B#Value
  def value3: C#Value
  def variables = Seq(_1, _2, _3)
  def apply(v:Var): v.Value = if (v eq _1) value1.asInstanceOf[v.Value] else if (v eq _2) value2.asInstanceOf[v.Value] else if (v eq _3) value3.asInstanceOf[v.Value] else v.value.asInstanceOf[v.Value] // throw new Error("Variable not present: "+v)
  def get(v:Var): Option[v.Value] = if (v eq _1) Some(value1.asInstanceOf[v.Value]) else if (v eq _2) Some(value2.asInstanceOf[v.Value]) else if (v eq _3) Some(value3.asInstanceOf[v.Value]) else None
  def contains(v:Var): Boolean = if ((v eq _1) || (v eq _2) || (v eq _3)) true else false
  override def setVariables(implicit d:DiffList): Unit = {
    _1 match { case v:MutableVar => v.set(value1.asInstanceOf[v.Value]) }
    _2 match { case v:MutableVar => v.set(value2.asInstanceOf[v.Value]) }
    _3 match { case v:MutableVar => v.set(value3.asInstanceOf[v.Value]) }
  }
}
/** An efficient Assignment of three variables.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
class Assignment3[A<:Var,B<:Var,C<:Var](val _1:A, var value1:A#Value, val _2:B, var value2:B#Value, val _3:C, var value3:C#Value) extends AbstractAssignment3[A,B,C]

/** An efficient abstract Assignment of three variables.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
trait AbstractAssignment4[A<:Var,B<:Var,C<:Var,D<:Var] extends Assignment {
  def _1: A
  def _2: B
  def _3: C
  def _4: D
  def value1: A#Value
  def value2: B#Value
  def value3: C#Value
  def value4: D#Value
  def variables = Seq(_1, _2, _3, _4)
  def apply(v:Var): v.Value = if (v eq _1) value1.asInstanceOf[v.Value] else if (v eq _2) value2.asInstanceOf[v.Value] else if (v eq _3) value3.asInstanceOf[v.Value] else if (v eq _4) value4.asInstanceOf[v.Value] else v.value.asInstanceOf[v.Value] // throw new Error("Variable not present: "+v)
  def get(v:Var): Option[v.Value] = if (v eq _1) Some(value1.asInstanceOf[v.Value]) else if (v eq _2) Some(value2.asInstanceOf[v.Value]) else if (v eq _3) Some(value3.asInstanceOf[v.Value]) else if (v eq _4) Some(value4.asInstanceOf[v.Value]) else None
  def contains(v:Var): Boolean = if ((v eq _1) || (v eq _2) || (v eq _3) || (v eq _4)) true else false
  override def setVariables(implicit d:DiffList): Unit = {
    _1 match { case v:MutableVar => v.set(value1.asInstanceOf[v.Value]) }
    _2 match { case v:MutableVar => v.set(value2.asInstanceOf[v.Value]) }
    _3 match { case v:MutableVar => v.set(value3.asInstanceOf[v.Value]) }
    _4 match { case v:MutableVar => v.set(value4.asInstanceOf[v.Value]) }
  }
}
/** An efficient Assignment of three variables.
    Values for variables not in this assignment are taken from those variables themselves (the "global" assignment).
    @author Andrew McCallum */
class Assignment4[A<:Var,B<:Var,C<:Var,D<:Var](val _1:A, var value1:A#Value, val _2:B, var value2:B#Value, val _3:C, var value3:C#Value, val _4:D, var value4:D#Value) extends AbstractAssignment4[A,B,C,D]



// TODO Consider making this inherit from MutableAssignment -akm
/** An Assignment whose values are those stored inside the variables themselves. 
    @author Andrew McCallum */
object GlobalAssignment extends Assignment {
  def variables = throw new Error("Cannot list all variables of the global Assignment.")
  def apply(v:Var): v.Value = v.value
  def get(v:Var): Option[v.Value] = Some(v.value)
  def contains(v:Var) = true
  override def setVariables(implicit d:DiffList): Unit = {}
}

/** An Assignment backed by a sequence of assignments.  
    The returned value will be from the first Assignment in the sequence to contain the variable. 
    @author Andrew McCallum */
class AssignmentStack(val assignment:Assignment, val next:AssignmentStack = null) extends Assignment {
  def variables = assignment.variables ++ next.variables
  protected def apply(v:Var, s:AssignmentStack): v.Value =
    if (s.next eq null) s.assignment(v)
    else s.assignment.get(v).getOrElse(apply(v, s.next))
  def apply(v:Var): v.Value = apply(v, this)
  /** Return the an Option for the value assigned to variable v.  If v is not contained in this Assignment return None. */
  def get(v: Var, s: AssignmentStack): Option[v.Value] = {
    val o = assignment.get(v)
    if (o != None) o
    else if (s.next ne null) s.next.get(v).asInstanceOf
    else None
  }
  def get(v:Var): Option[v.Value] = get(v, this)
  def contains(v:Var) = if (assignment.contains(v)) true else next.contains(v)
  /** Returns a new Assignment stack, the result of prepending Assignment a. */
  def +:(a:Assignment): AssignmentStack = new AssignmentStack(a, this)
}

/** Allows an iterator over the assignments to the neighbors of a factor (optionally specifying the variables that should vary)
    @author Sameer Singh */
@deprecated("May be removed in future due to inefficiency.", "Before 2014-11-17")
object AssignmentIterator {
  def assignments1[N1 <: Var](f1: Factor1[N1], varying: Set[Var]): Iterator[Assignment] = assignments1(f1._1, varying)

  def assignments1[N1 <: Var](v1:N1, varying: Set[Var]): Iterator[Assignment] = {
    if (varying(v1)) 
      //v1.domain.iterator.map(value => new Assignment1(v1, value.asInstanceOf[v1.Value]))
      v1 match { case v1:DiscreteVar => v1.domain.iterator.map(value => new Assignment1(v1, value.asInstanceOf)) }
    else Iterator.empty
  }

  def assignments2[N1 <: Var, N2 <: Var](f2: Factor2[N1, N2], varying: Set[Var]): Iterator[Assignment] = assignments2(f2._1, f2._2, varying)

  def assignments2[N1 <: Var, N2 <: Var](v1:N1, v2:N2, varying: Set[Var]): Iterator[Assignment] = {
    val values1 = if (varying.contains(v1)) v1.asInstanceOf[DiscreteVar].domain else Seq(v1.value.asInstanceOf[DiscreteValue])
    val values2 = if (varying.contains(v2)) v2.asInstanceOf[DiscreteVar].domain else Seq(v2.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2) yield new Assignment2(v1, val1.asInstanceOf[v1.Value], v2, val2.asInstanceOf[v2.Value])).iterator
  }

  def assignments3[N1 <: Var, N2 <: Var, N3 <: Var](f3: Factor3[N1, N2, N3], varying: Set[Var]): Iterator[Assignment] = assignments3(f3._1, f3._2, f3._3, varying)

  def assignments3[N1 <: Var, N2 <: Var, N3 <: Var](v1:N1, v2:N2, v3:N3, varying: Set[Var]): Iterator[Assignment] = {
    val values1 = if (varying.contains(v1)) v1.asInstanceOf[DiscreteVar].domain else Seq(v1.value.asInstanceOf[DiscreteValue])
    val values2 = if (varying.contains(v2)) v2.asInstanceOf[DiscreteVar].domain else Seq(v2.value.asInstanceOf[DiscreteValue])
    val values3 = if (varying.contains(v3)) v3.asInstanceOf[DiscreteVar].domain else Seq(v3.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2; val3 <- values3) yield new Assignment3(v1, val1.asInstanceOf[v1.Value], v2, val2.asInstanceOf[v2.Value], v3, val3.asInstanceOf[v3.Value])).iterator
  }

  def assignments4[N1 <: Var, N2 <: Var, N3 <: Var, N4 <: Var](f4: Factor4[N1, N2, N3, N4], varying: Set[Var]): Iterator[Assignment] = assignments4(f4._1, f4._2, f4._3, f4._4, varying)

  def assignments4[N1 <: Var, N2 <: Var, N3 <: Var, N4 <: Var](v1:N1, v2:N2, v3:N3, v4:N4, varying: Set[Var]): Iterator[Assignment] = {
    val values1 = if (varying.contains(v1)) v1.asInstanceOf[DiscreteVar].domain else Seq(v1.value.asInstanceOf[DiscreteValue])
    val values2 = if (varying.contains(v2)) v2.asInstanceOf[DiscreteVar].domain else Seq(v2.value.asInstanceOf[DiscreteValue])
    val values3 = if (varying.contains(v3)) v3.asInstanceOf[DiscreteVar].domain else Seq(v3.value.asInstanceOf[DiscreteValue])
    val values4 = if (varying.contains(v4)) v4.asInstanceOf[DiscreteVar].domain else Seq(v4.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2; val3 <- values3; val4 <- values4) yield new Assignment4(v1, val1.asInstanceOf[v1.Value], v2, val2.asInstanceOf[v2.Value], v3, val3.asInstanceOf[v3.Value], v4, val4.asInstanceOf[v4.Value])).iterator
  }

  def assignments(vars: Seq[Var]): Iterator[Assignment] = {
    if(vars.length == 1) assignments1(vars.head, vars.toSet)
    else if(vars.length == 2) assignments2(vars(0), vars(1), vars.toSet)
    else if(vars.length == 3) assignments3(vars(0), vars(1), vars(2), vars.toSet)
    else if(vars.length == 4) assignments4(vars(0), vars(1), vars(2), vars(3), vars.toSet)
    else throw new Error ("To many variables to iterate over (>4): " + vars.length)
  }

  def assignments(f: Factor, varying: Set[Var]): Iterator[Assignment] = {
    f match {
      // Factor 1
      case f1: Factor1[_] => assignments1(f1, varying)
      // Factor 2
      case f2: Factor2[_, _] => assignments2(f2, varying)
      // Factor 3
      case f3: Factor3[_, _, _] => assignments3(f3, varying)
      // Factor 4
      case f4: Factor4[_, _, _, _] => assignments4(f4, varying)
    }
  }

}
