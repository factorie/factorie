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

package cc.factorie.variable

import cc.factorie.variable

/** The value of a BooleanDomain.  A subclass of CategoricalValue.
    @author Andrew McCallum */
trait BooleanValue extends CategoricalValue[Boolean] { def domain: BooleanDomain = BooleanDomain }


/** The Domain for BooleanVar, of size two, containing a falseValue
    (with intValue = 0) and a trueValue (with intValue = 1). 
    @author Andrew McCallum */
class BooleanDomain extends CategoricalDomain[Boolean] with Domain {
  val falseValue = super.value(false) // will get index == 0
  val trueValue = super.value(true)   // will get index == 1
  freeze()
  override protected def newCategoricalValue(i:Int, e:Boolean) = new CategoricalValue(i, e)
  // The above makes sure that the hashtable in the CategoricalDomain is consistent, 
  // but the methods below will do most of the real work
  override def size = 2
  override def category(index:Int) = index == 1
  override def index(bool:Boolean) = if (bool) 1 else 0
  override def value(bool:Boolean) = if (bool) trueValue else falseValue
  override def apply(index:Int) = if (index == 1) trueValue else falseValue
  def apply(b:Boolean) = if (b) trueValue else falseValue
}
object BooleanDomain extends BooleanDomain

object BooleanValue {
  def apply(b:Boolean) = if (b) BooleanDomain.trueValue else BooleanDomain.falseValue
}

/** A Variable containing a single Boolean value, which might be mutable or immutable.
    @see BooleanVariable
    @author Andrew McCallum */
trait BooleanVar extends CategoricalVar[Boolean] with VarWithDomain {
  type Value = BooleanValue
  def value: BooleanValue
  //def domain: CategoricalDomain[Boolean] = BooleanDomain
  def domain: BooleanDomain = BooleanDomain
  override def categoryValue = intValue == 1 // Efficiently avoid a lookup in the domain
  @inline final def booleanValue = categoryValue // Alias for the above method
  def ^(other:BooleanVar):Boolean = booleanValue && other.booleanValue
  def v(other:BooleanVar):Boolean = booleanValue || other.booleanValue
  def ==>(other:BooleanVar):Boolean = !booleanValue || other.booleanValue
  def unary_!(): Boolean = !booleanValue
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
}

/** A class for mutable Boolean variables. 
    @author Andrew McCallum */ 
// TODO Note that Value here will be CategoricalValue[Boolean], not BooleanValue; only matters if we care about the type of .domain // TODO I think this is no longer true.
class BooleanVariable extends MutableCategoricalVar[Boolean] with BooleanVar {
  //type Value = BooleanValue
  // Default will be false, because initial setting of MutableDiscreteVar.__value is 0
  def this(initialValue:Boolean) = { this(); _set(if (initialValue) 1 else 0) }
  // Avoid CategoricalVariable's HashMap lookup
  final def set(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  override final def setCategory(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  // If you want to coordinate with changes to this variable value, override set(ValueType)
  final def :=(newBoolean:Boolean) = set(newBoolean)(null)
}
