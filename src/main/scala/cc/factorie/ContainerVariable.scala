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

import scala.collection.mutable.ArrayBuffer

/** A variable that is a container for other variables (whose type is this.ContainedVariableType), 
    A Template that neighbors a ContainerVariable subclass, will also unroll a Factor
    for changes to any Variables of type ContainedVariableType.
    This mechanism is used for implementing var-args in Template arguments; 
    for example see Template2.factors. */
trait ContainerVariable[A<:Variable] extends Variable {
  type ContainedVariableType = A
  def containedVariableManifest(implicit m:Manifest[A]) = m
}

// NOTE: Vars#hashCode must be based on the contents of the collection, or else Factor uniq'ing won't work.
// So this is the exception to the "rule" that Variable must have equals and hashCode based on unique memory address.
trait Vars[A<:Variable] extends scala.collection.Seq[A] with ContainerVariable[A] with VarAndValueGenericDomain[Vars[A],scala.collection.Seq[A#Value]] {
  type Value = scala.collection.Seq[A#Value]
  def value: scala.collection.Seq[A#Value] = this.map(_.value.asInstanceOf[A#Value])
  override def toString = mkString("Vars(", ",",")")
}

class ArrayVars[V<:Variable](val toArray:Array[V]) extends Vars[V] {
  //def this(vs:Seq[V]) = this(vs.toArray)
  def length = toArray.length
  def apply(index:Int) = toArray(index)
  def iterator = toArray.iterator
}

class SeqVars[V<:Variable](override val toSeq:Seq[V]) extends Vars[V] {
  def length = toSeq.length
  def apply(index:Int) = toSeq(index)
  def iterator = toSeq.iterator

}

class ArrayBufferVars[V<:Variable] extends ArrayBuffer[V] with Vars[V]

object Vars {
  def from[V<:Variable](vs:V*): Vars[V] = new SeqVars(vs)
  def fromSeq[V<:Variable](vs:Seq[V]) = new SeqVars(vs)
  def apply[V<:Variable](vs:Seq[V]): Vars[V] = new SeqVars(vs) // TODO Should this be Seq or V*?
}
// TODO Consider making an implicit conversion for these.

