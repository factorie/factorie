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

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** A variable that is a container for other variables (whose type is this.ContainedVariableType),
    A Template that neighbors a ContainerVariable subclass, will also unroll a Factor
    for changes to any Variables of type ContainedVariableType.
    This mechanism is used for implementing var-args in Template arguments; 
    for example see Template2.factors.
    @author Andrew McCallum */
trait ContainerVariable[A<:Var] extends Var {
  type Value <: scala.collection.Seq[A#Value]
  type ContainedVariableType = A
  def containedVariableTag(implicit m:ClassTag[A]) = m
}

// NOTE: Vars#hashCode must be based on the contents of the collection, or else Factor uniq'ing won't work.
// So this is the exception to the "rule" that Variable must have equals and hashCode based on unique memory address.
/** A more concrete ContainerVariable, that is also a scala.collection.Seq.
    Used for implementing var-args in the neighbors of a Factor.
    @author Andrew McCallum */
trait Vars[A<:Var] extends scala.collection.Seq[A] with ContainerVariable[A] /*with VarAndValueGenericDomain[Vars[A],scala.collection.Seq[A#Value]]*/ {
  type Value = scala.collection.Seq[A#Value]
  def value: scala.collection.Seq[A#Value] = this.map(_.value.asInstanceOf[A#Value])
  override def toString = mkString("Vars(", ",",")")
}

/** A Vars with array-based storage of the value.
    @author Andrew McCallum */
class ArrayVars[V<:Var](val toArray:Array[V]) extends Vars[V] {
  //def this(vs:Seq[V]) = this(vs.toArray)
  def length = toArray.length
  def apply(index:Int) = toArray(index)
  def iterator = toArray.iterator
}

/** A Vars with Seq-based storage of the value.
    @author Andrew McCallum */
class SeqVars[V<:Var](override val toSeq:Seq[V]) extends Vars[V] {
  def length = toSeq.length
  def apply(index:Int) = toSeq(index)
  def iterator = toSeq.iterator

}

/** A Vars with ArrayBuffer-based storage of the value.
    @author Andrew McCallum */
class ArrayBufferVars[V<:Var] extends ArrayBuffer[V] with Vars[V]

/** Convenient methods for creating Vars objects.
    @author Andrew McCallum */
object Vars {
  def from[V<:Var](vs:V*): Vars[V] = new SeqVars(vs)
  def fromSeq[V<:Var](vs:Seq[V]) = new SeqVars(vs)
  def apply[V<:Var](vs:Seq[V]): Vars[V] = new SeqVars(vs) // TODO Should this be Seq or V*?
}
// TODO Consider making an implicit conversion for these.

