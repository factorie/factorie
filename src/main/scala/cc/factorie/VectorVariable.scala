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
import cc.factorie.la.Vector
import cc.factorie.la.SparseVector
import scala.util.Sorting

trait VectorDomain extends Domain[Vector]
object VectorDomain extends VectorDomain

/** A variable whose value can be described by a vector.
    For example, each "dimension" (e.g. the integers in activeDomain) may be a discrete value.
    The value at that index is a "weight" representing (partial)
    repetitions of the discrete value.  So one way to think of these
    instances is as some number of discrete variables, each with
    a weight.  
    @author Andrew McCallum */
trait VectorVar extends Variable {
  type VariableType <: VectorVar
  type DomainType <: VectorDomain
  type Value <: Vector
  /** A cc.factorie.la.Vector representation of the value of this variable. */
  // TODO Consider removing this because we can just use 'value' method?
  def vector: Vector = value
  //def dimensionSize: Int // ????
  def update(index:Int, newValue:Double): Unit = vector.update(index, newValue)
  def increment(index:Int, incr:Double): Unit = vector.update(index, vector(index) + incr)
  /** A more efficient alternative to this.vector.activeDomain */
  //def activeDomain: Iterable[Int]  // TODO Consider removing this? -akm
}

/** A vector of Double values with sparse vector representation. */
// TODO Make separate sparse and dense versions of this
abstract class VectorVariable(initialValue: Vector) extends VectorVar {
  thisVariable =>
  type VariableType <: VectorVariable
  type DomainType = VectorDomain
  type Value = Vector
  val value: Value = initialValue
  def domain = VectorDomain
  //def activeDomain = vector.activeDomain
}
