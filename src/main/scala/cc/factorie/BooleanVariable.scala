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
trait BooleanVar extends CategoricalVar[Boolean] with DomainType[BooleanDomain] with ValueType[BooleanValue] {
  type VariableType <: BooleanVar
  override def entryValue = (intValue == 1) // Efficiently avoid a lookup in the domain 
  override def categoryValue = (intValue == 1) // Efficiently avoid a lookup in the domain 
  def booleanValue = (intValue == 1) // Alias for the above method
  def ^(other:BooleanVar):Boolean = booleanValue && other.booleanValue
  def v(other:BooleanVar):Boolean = booleanValue || other.booleanValue
  def ==>(other:BooleanVar):Boolean = !booleanValue || other.booleanValue
  def unary_!(): Boolean = !booleanValue
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
  def domain = BooleanDomain
}

/** A class for mutable Boolean variables. 
    @author Andrew McCallum */
class BooleanVariable(initialValue:Boolean = false) extends CategoricalVariable(initialValue) with BooleanVar { 
  type VariableType <: BooleanVariable
  // Avoid CategoricalVariable's HashMap lookup
  override final def set(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  // If you want to coordinate with changes to this variable value, override set(ValueType)
  final def :=(newBoolean:Boolean) = set(newBoolean)(null)
}

/** A class for variables with immutable Boolean values.
    @author Andrew McCallum */
class BooleanObservation(theValue:Boolean) extends CategoricalObservation(theValue) with BooleanVar {
  type VariableType <: BooleanObservation
}

/** You can efficiently get a pre-allocated BooleanObservation instance with this object
    by BooleanObservation(true) and BooleanObservation(false.) */
// TODO Try to get rid of this, now that 'statistics' takes values instead.
object BooleanObservation {
  val f = new BooleanObservation(false)
  val t = new BooleanObservation(true)
  def apply(x:Boolean): BooleanObservation = if (x) t else f
}

trait BooleanValue extends CategoricalValue[Boolean]

/** The Domain for BooleanVars, of size two, containing false == 0 and true == 1. */
class BooleanDomain extends CategoricalDomain[Boolean] with ValueType[BooleanValue] {
  val falseValue = super.getValue(false)
  val trueValue = super.getValue(true)
  freeze

  class BooleanValue(i:Int, e:Boolean) extends CategoricalValue(i, e) with cc.factorie.BooleanValue
  override protected def newCategoricalValue(i:Int, e:Boolean) = new BooleanValue(i, e)

  // The above makes sure that the hashtable in the CategoricalDomain is consistent, 
  // but the methods below will do most of the real work
  override def size = 2
  override def allocSize = 2
  //override def apply(index:Int) = index == 1
  override def getEntry(index:Int) = index == 1
  override def index(entry:Boolean) = if (entry) 1 else 0
  override def getIndex(entry:Boolean) = if (entry) 1 else 0
  override def getValue(entry:Boolean) = if (entry) trueValue else falseValue
}

object BooleanDomain extends BooleanDomain
