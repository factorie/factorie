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

/** A variable whose value can be described by a vector.
    For example, each "dimension" (e.g. the integers in activeDomain) may be a discrete value.
    The value at that index is a "weight" representing (partial)
    repetitions of the discrete value.  So one way to think of these
    instances is as some number of discrete variables, each with
    a weight.  
    @author Andrew McCallum */
trait VectorVar extends Variable {
  type VariableType <: VectorVar
  type DomainType <: VectorDomain[VariableType]
  class DomainClass extends VectorDomain[VariableType]()(null)

  /** A cc.factorie.la.Vector representation of the value of this variable. */
  def vector: Vector
  //def dimensionSize: Int // ????
}


// Real (floating point) Vector Variables

trait RealVectorVar extends VectorVar 

/** A vector of Double values */
abstract class RealVectorVariable(theLength:Int) extends RealVectorVar {
  type VariableType <: RealVectorVariable
  def this(sizeProxy:Iterable[_]) = this(sizeProxy.size)
  val vector: Vector = new SparseVector(theLength)
  def activeDomain = vector.activeDomain
  def update(index:Int, newValue:Double): Unit = vector.update(index, newValue)
  def increment(index:Int, incr:Double): Unit = vector.update(index, vector(index) + incr)
}

/** A vector of Double value that can also be indexed the entries in a CategoricalDomain */
@DomainInSubclasses
abstract class CategoricalRealVectorVariable[T] extends RealVectorVariable(-1) with CategoricalVars[T] {
  type VariableType <: CategoricalRealVectorVariable[T]
  override val vector: Vector = new cc.factorie.la.GrowableSparseVector(domain) // domain.size is a proxy for vector.length
  def update(elt:T, newValue:Double): Unit = vector.update(domain.index(elt), newValue)
  def increment(elt:T, incr:Double): Unit = { val i = domain.index(elt); vector.update(i, vector(i) + incr) }
}

/** An more traditionally-named alias for CategoricalRealVectorVariable */
@DomainInSubclasses
abstract class FeatureVectorVariable[T] extends CategoricalRealVectorVariable[T]


// Binary Vector Variables

@DomainInSubclasses
trait BinaryVectorVar extends DiscreteVars with VectorVar {
  def contains(value:Int): Boolean = vector.apply(value) != 0.0
  // def domainSize: Int
  // def intValues: Iterable[Int]
}

/** A SparseBinaryVector as a cc.factorie.Variable.
    Note that Variables must always define "equals" as pointer equality,
    but cc.factorie.la.Vector define "equals" by content equality. 
    Thus a Variable can never inherit from a Vector; here it contains a Vector instead. */
@DomainInSubclasses
abstract class SparseBinaryVectorVariable extends BinaryVectorVar {
  val vector = new cc.factorie.la.SparseBinaryVector(-1) { override def length = domain.allocSize }
  def length = domain.allocSize
  //def apply(i:Int) = vector.apply(i)
  def activeDomain = vector.activeDomain
  def zero: Unit = vector.zero
  def +=(i:Int): Unit = { if (frozen) throw new Error("Cannot append to frozen SparseBinaryVectorVariable."); vector.+=(i) }
  //def ++=(is:Iterable[Int]): Unit = is.foreach(i => vector.+=(i)) // Conflicts with ++=(Iterable[T])
  var frozen = false
  def freeze = frozen = true
  override def isConstant = frozen
}

@DomainInSubclasses
trait CategoricalBinaryVectorVariable[T] extends BinaryVectorVar with CategoricalVars[T] {
  def +=(value:T): Unit 
  def ++=(values:Iterable[T]): Unit
}

@DomainInSubclasses
abstract class SparseCategoricalBinaryVectorVariable[T] extends SparseBinaryVectorVariable with CategoricalBinaryVectorVariable[T] {
  type VariableType <: SparseCategoricalBinaryVectorVariable[T]
  def this(initVals:Iterable[T]) = { this(); this.++=(initVals) }
  def values: Seq[T] = { val d = this.domain; val v = this.vector; val result = new ArrayBuffer[T](v.activeDomainSize); v.forActiveDomain(i => result += d.get(i)); result }
  /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
      If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
  def skipNonCategories = false
  def +=(value:T): Unit = {
    val idx = domain.index(value);
    if (idx == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories)
        throw new Error("BinaryVectorVariable += value " + value + " not found in domain " + domain)
    } else {
      this.+=(idx)
    }
  }
  def ++=(values:Iterable[T]): Unit = values.foreach(this.+=(_))
  override def toString = vector.activeDomain.map(i => domain.get(i).toString+"="+i).mkString(printName+"(", ",", ")")
}

/** A shorter, more intuitive alias for SparseCategoricalBinaryVectorVariable */
@DomainInSubclasses
abstract class BinaryFeatureVectorVariable[T] extends SparseCategoricalBinaryVectorVariable[T] {
  type VariableType <: BinaryFeatureVectorVariable[T]
  def this(initVals:Iterable[T]) = { this(); this.++=(initVals) }
}

