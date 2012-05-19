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
import cc.factorie.la._


/** A DiscreteVar whose integers 0...N are associated with an categorical objects of type A.
    Concrete implementations include CategoricalVariable and CategoricalObservation. 
    @author Andrew McCallum */
trait CategoricalVar[A] extends DiscreteVar with CategoricalTensorVar[A] with VarAndValueType[CategoricalVar[A],CategoricalValue[A]] {
  def domain: CategoricalDomain[A]
  def categoryValue: A = if (value ne null) value.category else null.asInstanceOf[A]
  override def toString = printName + "(" + (if (categoryValue == null) "null" else if (categoryValue == this) "this" else categoryValue.toString) + "=" + intValue + ")" // TODO Consider dropping the "=23" at the end.
}

trait MutableCategoricalVar[A] extends CategoricalVar[A] with MutableDiscreteVar {
  def setCategory(newCategory:A)(implicit d: DiffList): Unit = set(domain.index(newCategory))(d)
}

/** A DiscreteVariable whose integers 0...N are associated with an object of type A. 
    @author Andrew McCallum */
abstract class CategoricalVariable[A] extends DiscreteVariable with MutableCategoricalVar[A] {
  def this(initialCategory:A) = { this(); _set(domain.index(initialCategory)) }
  //def this(initalValue:ValueType) = { this(); _set(initialValue) }
}






// ItemizedObservation support

// TODO Replace this with Catalog?
// But then we couldn't use syntax like:  PersonDomain.size
// But this doesn't matter any more.

// TODO make this ItemizedVar

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.categoryValue == p1. 
    @author Andrew McCallum */
trait ItemizedObservation[This <: ItemizedObservation[This]] extends CategoricalVar[This] with VarWithConstantValue {
  this: This =>
  def domain: CategoricalDomain[This]
  // Put the variable in the CategoricalDomain and remember it.
  override val value = domain.value(this)
  def tensor = value
  override def categoryValue = this
}

