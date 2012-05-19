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

/** The value of a BooleanDomain.  A subclass of CategoricalValue. */
trait BooleanValue extends CategoricalValue[Boolean] {
  def domain: BooleanDomain = BooleanDomain
  //def booleanValue = if (this eq BooleanDomain.trueValue) true else false
  def booleanValue = if (intValue == 1) true else false
}

/** The Domain for BooleanVar, of size two, containing a falseValue
    (with intValue = 0) and a trueValue (with intValue = 1). */
class BooleanDomain extends CategoricalDomain[Boolean] with ValueType[BooleanValue] {
  thisDomain =>
  val falseValue = super.value(false)
  val trueValue = super.value(true)
  freeze
  class BooleanValue(i:Int, e:Boolean) extends CategoricalValue(i, e) with cc.factorie.BooleanValue {
    override def domain = thisDomain
  }
  override protected def newCategoricalValue(i:Int, e:Boolean) = new BooleanValue(i, e)
  // The above makes sure that the hashtable in the CategoricalDomain is consistent, 
  // but the methods below will do most of the real work
  override def size = 2
  override def allocSize = 2
  //override def apply(index:Int) = index == 1
  override def category(index:Int) = index == 1
  override def index(bool:Boolean) = if (bool) 1 else 0
  //override def index(bool:Boolean) = if (bool) 1 else 0
  override def value(bool:Boolean) = if (bool) trueValue else falseValue
  override def apply(index:Int) = if (index == 1) trueValue else falseValue
}
object BooleanDomain extends BooleanDomain

/** A Variable containing a single Boolean value, which might be mutable or immutable.
    @see BooleanVariable
    @author Andrew McCallum */
trait BooleanVar extends CategoricalVar[Boolean] with VarAndValueType[BooleanVar,BooleanValue] {
  def domain = BooleanDomain
  override def categoryValue = (value eq BooleanDomain.trueValue) // Efficiently avoid a lookup in the domain 
  final def booleanValue = categoryValue // Alias for the above method
  def ^(other:BooleanVar):Boolean = booleanValue && other.booleanValue
  def v(other:BooleanVar):Boolean = booleanValue || other.booleanValue
  def ==>(other:BooleanVar):Boolean = !booleanValue || other.booleanValue
  def unary_!(): Boolean = !booleanValue
  override def toString = if (intValue == 0) printName+"(false)" else printName+"(true)"
}

/** A class for mutable Boolean variables. 
    @author Andrew McCallum */
class BooleanVariable(initialValue:Boolean) extends CategoricalVariable(initialValue) with BooleanVar {
  def this() = this(false)
  // Avoid CategoricalVariable's HashMap lookup
  final def set(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  override final def setCategory(newBoolean:Boolean)(implicit d: DiffList): Unit = set(if (newBoolean) 1 else 0)
  // If you want to coordinate with changes to this variable value, override set(ValueType)
  final def :=(newBoolean:Boolean) = set(newBoolean)(null)
}
