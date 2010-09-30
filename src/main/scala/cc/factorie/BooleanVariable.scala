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

/** A Variable containing a single Boolean value, which might be mutable or immutable.
    @see BooleanVariable
    @see BooleanObservation
    @author Andrew McCallum */
trait BooleanVar extends CategoricalVar[Boolean] {
  type VariableType <: BooleanVar
  override def value = (intValue == 1) // Efficiently avoid a lookup in the domain 
  def booleanValue = (intValue == 1) // Alias for the above method
  def ^(other:BooleanVar):Boolean = value && other.value
  def v(other:BooleanVar):Boolean = value || other.value
  def ==>(other:BooleanVar):Boolean = !value || other.value
  def unary_!(): Boolean = !value
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
  type DomainType <: BooleanDomain[VariableType]
  class DomainClass extends BooleanDomain
  // Domain is not in subclasses:  all BooleanValue variables share the same domain.
}

/** A trait for mutable Boolean variables. 
    @author Andrew McCallum */
class BooleanVariable(initialValue:Boolean = false) extends CategoricalVariable(initialValue) with BooleanVar { 
  type VariableType <: BooleanVariable
  // Avoid CategoricalVariable's HashMap lookup
  override final def set(newValue:Boolean)(implicit d: DiffList): Unit = set(if (newValue) 1 else 0)
  // If you want to coordinate with changes to this variable value, override set(Int)
}

/** A trait for variables with immutable Boolean values.
    @author Andrew McCallum */
class BooleanObservation(theValue:Boolean) extends CategoricalObservation(theValue) with BooleanVar {
  type VariableType <: BooleanObservation
}

/** You can efficiently get a pre-allocated BooleanObservation instance with this object
    by BooleanObservation(true) and BooleanObservation(false.) */
object BooleanObservation {
  val f = new BooleanObservation(false)
  val t = new BooleanObservation(true)
  def apply(x:Boolean): BooleanObservation = if (x) t else f
}

/** The Domain for BooleanVars, of size two, containing false == 0 and true == 1. */
class BooleanDomain[V<:BooleanVar](implicit m:Manifest[V]) extends CategoricalDomain[V] {
  super.index(false)
  super.index(true)
  freeze
  // The above makes sure that the hashtable in the CategoricalDomain is consistent, 
  // but the methods below will do most of the real work
  override def size = 2
  override def allocSize = 2
  override def apply(index:Int) = index == 1
  override def get(index:Int) = index == 1
  override def index(entry:Boolean) = if (entry) 1 else 0
  override def getIndex(entry:Boolean) = if (entry) 1 else 0
}
