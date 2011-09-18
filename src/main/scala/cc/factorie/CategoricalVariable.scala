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
import cc.factorie.la.SparseBinaryVector
import cc.factorie.la.SparseIndexedVector

/** A Variable holding one or more categorical values.
    The subclass CategoricalVariable holds a single CategoricalValue.
    The subclass FeatureVectorVariable holds multiple CategoricalValues, each with a weight.
    @author Andrew McCallum */
trait CategoricalVectorVar[T] extends DiscreteVectorVar with VarAndValueType[CategoricalVectorVar[T],CategoricalVectorValue[T]] {
  type CategoryType = T
  def domain: CategoricalVectorDomain[T]
   /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
       If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
   def skipNonCategories = false
   protected def doWithIndexSafely(elt:T, f:Int=>Unit): Unit = {
     val i = domain.dimensionDomain.index(elt)
     if (i == CategoricalDomain.NULL_INDEX) {
       if (!skipNonCategories)
         throw new Error("CategoricalVectorVar += value " + value + " not found in domain " + domain)
     } else {
       f(i)
     }
   }
   def update(elt:T, newValue:Double): Unit = doWithIndexSafely(elt, i => vector.update(i, newValue))
   def increment(elt:T, incr:Double): Unit = doWithIndexSafely(elt, i => vector.increment(i, incr))
   def +=(elt:T): Unit = increment(elt, 1.0)
   def ++=(elts:Iterable[T]): Unit = elts.foreach(this.+=(_))
   def activeCategories: Seq[T] = vector.activeDomain.toSeq.map(i => domain.dimensionDomain.getCategory(i))
   override def toString = vector.activeDomain.map(i => domain.dimensionDomain.getCategory(i).toString+"="+i).mkString(printName+"(", ",", ")")
}

abstract class CategoricalVectorVariable[T] extends VectorVariable with CategoricalVectorVar[T] {
  thisVariable =>
  _set(new cc.factorie.la.GrowableSparseVector(domain.dimensionDomain) with CategoricalVectorValue[T] {
    def domain = thisVariable.domain
  })
}

/** An more traditionally-named alias for CategoricalVectorVariable */
abstract class FeatureVectorVariable[T] extends CategoricalVectorVariable[T]

trait BinaryCategoricalVectorVar[T] extends CategoricalVectorVar[T] {
  def +=(value:T): Unit 
  def ++=(values:Iterable[T]): Unit
}

trait SparseBinaryCategoricalVectorVar[T] extends SparseBinaryDiscreteVectorVar with BinaryCategoricalVectorVar[T] with VarAndValueType[SparseBinaryCategoricalVectorVar[T],SparseBinaryVector with CategoricalVectorValue[T]] {
  // TODO Rename next method to activeCategories or categoryValues
  //def values: Seq[T] = { val d = this.domain; val v = this.vector; val result = new ArrayBuffer[T](v.activeDomainSize); v.forActiveDomain(i => result += d.dimensionDomain.getCategory(i)); result }
}

abstract class BinaryFeatureVectorVariable[T] extends VectorVariable with SparseBinaryCategoricalVectorVar[T] {
  thisVariable =>
  def this(initVals:Iterable[T]) = { this(); this.++=(initVals) }
  _set(new cc.factorie.la.SparseBinaryVector(-1) with CategoricalVectorValue[T] {
    def domain = thisVariable.domain
    override def length = domain.dimensionSize
  })
}

abstract class SparseIndexedCategoricalVectorVariable[T] extends VectorVariable with SparseIndexedDiscreteVectorVar with CategoricalVectorVar[T] with VarAndValueType[SparseIndexedCategoricalVectorVariable[T],SparseIndexedVector with CategoricalVectorValue[T]] {
  thisVariable =>
  //def this(initVals:Iterable[T]) = { this(); initVals.foreach(category => increment(category, 1.0) }
  _set(new cc.factorie.la.SparseIndexedVector(-1) with CategoricalVectorValue[T] {
    def domain = thisVariable.domain
    override def length = domain.dimensionSize
  })
}

// TODO Finish this.
abstract class FeatureVectorVariable2[T] extends SparseIndexedCategoricalVectorVariable[T] {
  def this(initVals:Iterable[T]) = { this(); initVals.foreach(t => this.increment(domain.dimensionDomain.index(t), 1.0)) }
}


/** A DiscreteVar whose integers 0...N are associated with an categorical objects of type A.
    Concrete implementations include CategoricalVariable and CategoricalObservation. 
    @author Andrew McCallum */
trait CategoricalVar[A] extends CategoricalVectorVar[A] with DiscreteVar with VarAndValueType[CategoricalVar[A],CategoricalValue[A]] {
  def domain: CategoricalDomain[A]
  def categoryValue: A = if (value ne null) value.category else null.asInstanceOf[A]
  override def toString = printName + "(" + (if (categoryValue == null) "null" else if (categoryValue == this) "this" else categoryValue.toString) + "=" + intValue + ")" // TODO Consider dropping the "=23" at the end.
}

trait MutableCategoricalVar[A] extends CategoricalVar[A] with MutableVar {
  def setCategory(newCategory:A)(implicit d: DiffList): Unit = set(domain.getValue(newCategory))
}

/** A DiscreteVariable whose integers 0...N are associated with an object of type A. 
    @author Andrew McCallum */
abstract class CategoricalVariable[A] extends DiscreteVariable with MutableCategoricalVar[A] {
  def this(initialCategory:A) = { this(); _set(domain.getValue(initialCategory)) }
  //def this(initalValue:ValueType) = { this(); _set(initialValue) }
}

/*abstract class CategoricalArrayVariable[A](initialCategories:Seq[A])
         extends DiscreteArrayVariable(Nil.asInstanceOf[Seq[Int]]) with VarAndValueType[CategoricalArrayVariable[A],Seq[CategoricalValue[A]]]
{
  initialCategories.foreach(c => values += domain.getValue(c))
  def appendCategory(category:A): Unit = appendValue(domain.getValue(category))
}*/


/** When mixed in to a CategoricalVariable, the variable's Domain will count the number of calls to 'index'.  
    Then you can reduce the size of the Domain by calling 'trimBelowCount' or 'trimBelowSize', 
    which will recreate the new mapping from categories to densely-packed non-negative integers. 
    In typical usage you would (1) read in the data, (2) trim the domain, (3) re-read the data with the new mapping, creating variables. 
 */





// ItemizedObservation support

// TODO Replace this with Catalog?
// But then we couldn't use syntax like:  Domain[Person].size
// But this doesn't matter any more.

// Can we make this ItemizedVariable

/** An Observation put into an index, and whose value is the Observation variable itself.  
    For example, you can create 10 'Person extends ItemizedObservation[Person]' objects, 
    and upon creation each will be mapped to a unique integer 0..9.
    p1 = new Person; p1.index == 0; p1.categoryValue == p1. 
    @author Andrew McCallum */
trait ItemizedObservation[This <: ItemizedObservation[This]] extends CategoricalVar[This] with VarWithConstantValue {
  this: This =>
  def domain: CategoricalDomain[This]
  // Put the variable in the CategoricalDomain and remember it.
  override val value = domain.getValue(this)
  override def categoryValue = this // TODO Ug.  Not a great method name here.
}

/** A variable who value is a pointer to an ItemizedObservation.  It is useful for entity-attributes whose value is another variable. 
    @author Andrew McCallum */
/*
class ItemizedObservationRef[V<:ItemizedObservation[V]](v:V) extends CategoricalVariable[V](v) {
  type VariableType = ItemizedObservationRef[V]
}
*/
