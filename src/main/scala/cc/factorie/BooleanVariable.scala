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

trait BooleanValue extends CategoricalValue[Boolean] with DomainType[BooleanDomain] {
  def booleanValue = if (this eq BooleanDomain.trueValue) true else false
}

/** The Domain for BooleanVar, of size two, containing false == 0 and true == 1. */
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

/** A Variable containing a single Boolean value, which might be mutable or immutable.
    @see BooleanVariable
    @see BooleanObservation
    @author Andrew McCallum */
trait BooleanVar extends CategoricalVar[Boolean] {
  type VariableType <: BooleanVar
  type DomainType = BooleanDomain
  type Value <: BooleanValue
  def domain = BooleanDomain
  override def entryValue = (value eq BooleanDomain.trueValue) // Efficiently avoid a lookup in the domain 
  override def categoryValue = (intValue == 1) // Efficiently avoid a lookup in the domain 
  final def booleanValue = entryValue // Alias for the above method
  // Avoid CategoricalVariable's HashMap lookup
  override final def set(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  // If you want to coordinate with changes to this variable value, override set(ValueType)
  final def :=(newBoolean:Boolean) = set(newBoolean)(null)
  // TODO Consider final def :=(newBoolean:Boolean)(implicit d:DiffList = null) = set(newBoolean)
  def ^(other:BooleanVar):Boolean = booleanValue && other.booleanValue
  def v(other:BooleanVar):Boolean = booleanValue || other.booleanValue
  def ==>(other:BooleanVar):Boolean = !booleanValue || other.booleanValue
  def unary_!(): Boolean = !booleanValue
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
}

/** A class for mutable Boolean variables. 
    @author Andrew McCallum */
class BooleanVariable extends BooleanVar {
  type VariableType <: BooleanVariable
  type Value = BooleanValue
  _set(domain.falseValue)
  def this(initialValue:Boolean = false) = { this(); _set(domain.getValue(initialValue)) }
}



// TODO Remove this class!
//@deprecated("Will be removed in the future.")
/*
case class BooleanBlock(v1:BooleanVariable, v2:BooleanVariable) extends Variable with IterableSettings with AbstractDomain[(Boolean,Boolean)] {
  //type Value = (Boolean,Boolean)
  //def domain = BooleanDomain
  def value = (v1.booleanValue, v2.booleanValue)
  def settings: SettingIterator = new SettingIterator {
    var i = -1
    val max = 4
    def hasNext = i < max
    def next(difflist:DiffList): DiffList = {
      val d = newDiffList
      i += 1
      if (i % 2 == 1) v2.set(true)(d) else v2.set(false)(d)
      if (i > 1) v1.set(true)(d) else v1.set(false)(d)
      d
    }
    def reset = i = -1
  }
}
*/
