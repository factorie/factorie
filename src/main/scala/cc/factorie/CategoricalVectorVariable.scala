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
   protected def doWithIndexSafely(elt:T, v: Double, update: Boolean): Unit = {
     val i = domain.dimensionDomain.index(elt)
     if (i == CategoricalDomain.NULL_INDEX) {
       if (!skipNonCategories)
         throw new Error("CategoricalVectorVar += value " + value + " not found in domain " + domain)
     } else {
       if (update)
         vector.update(i, v)
       else
         vector.increment(i, v)
     }
   }
   def update(elt:T, newValue:Double): Unit = doWithIndexSafely(elt, newValue, true)
   def increment(elt:T, incr:Double): Unit = doWithIndexSafely(elt, incr, false)
   def +=(elt:T): Unit = increment(elt, 1.0)
   def ++=(elts:Iterable[T]): Unit = elts.foreach(this.+=(_))
   def activeCategories: Seq[T] = vector.activeDomain.toSeq.map(i => domain.dimensionDomain.getCategory(i))
   //override def toString = vector.activeDomain.map(i => domain.dimensionDomain.getCategory(i).toString+"="+i).mkString(printName+"(", ",", ")")
   override def toString = vector.activeDomain.map(i => domain.dimensionDomain.getCategory(i).toString).mkString(printName+"(", ",", ")")
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
  def zero(): Unit
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

