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

/** Each "dimension" (i.e. the intValues) is a discrete value.  The
    value at that index is a "weight" representing (partial)
    repetitions of the discrete value.  So one way to think of these
    instances is as a variable number of discrete variables, each with
    a weight.  Note, however, that only binary weights could be
    inherit from cc.factorie.DiscreteVars, because with real-valued
    weights it would be unclear what to return for the 'intValues'
    method.  
    @author Andrew McCallum */
trait VectorVar extends Variable {
  /** A cc.factorie.la.Vector representation of the value of this variable. */
  def vector: Vector // NOTE: Also defined in DiscreteVars
}


// Real (floating point) Vector Variables

trait RealVectorVar extends VectorVar
/** A vector of Real values */
abstract class RealVectorVariable(theLength:Int) extends RealVectorVar /*with SeqEqualsEq[Double]*/ {
  type VariableType <: RealVectorVariable
  def this(theLength:Int, initVals:Iterable[(Int,Double)]) = { this(theLength); this.++=(initVals) }
  val vector = new SparseVector(theLength)
  //override def length = domain.allocSize
  def incr(index:Int, incr:Double): Unit
  def +=(elt:(Int,Double)) : Unit = vector.update(elt._1, vector(elt._1) + elt._2)
  def ++=(elts:Iterable[(Int,Double)]): Unit = elts.foreach(this.+=(_))
  throw new Error("Not yet implemented")
}

@DomainInSubclasses
abstract class CategoricalRealVectorVariable[T] extends RealVectorVariable(-1) with CategoricalVars[T] {
  type VariableType <: CategoricalRealVectorVariable[T]
  //def this(initVals:Iterable[(T,Double)]) = { this(); this.++=(initVals) }
  //def +=(elt:(T,Double)): Unit
  //def ++=(elt:Iterable[(T,Double)]): Unit
  throw new Error("Not yet implemented")
}

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
class SparseBinaryVectorVariable extends BinaryVectorVar /*with SeqEqualsEq[Double]*/ {
  val vector = new cc.factorie.la.SparseBinaryVector(-1) { override def length = domain.allocSize }
  def length = domain.allocSize
  //def apply(i:Int) = vector.apply(i)
  def intValues = vector.activeDomain
  def zero: Unit = vector.zero
  def +=(i:Int): Unit = vector.+=(i)
  //def ++=(is:Iterable[Int]): Unit = is.foreach(i => vector.+=(i)) // Conflicts with ++=(Iterable[T])
}

@DomainInSubclasses
trait CategoricalBinaryVectorVariable[T] extends BinaryVectorVar with CategoricalVars[T] {
  def +=(value:T): Unit 
  def ++=(values:Iterable[T]): Unit
}

@DomainInSubclasses
class SparseCategoricalBinaryVectorVariable[T] extends SparseBinaryVectorVariable with CategoricalBinaryVectorVariable[T] {
  type VariableType <: SparseCategoricalBinaryVectorVariable[T]
  def this(initVals:Iterable[T]) = { this(); this.++=(initVals) }
  def values: Seq[T] = { val d = this.domain; val result = new ArrayBuffer[T](domainSize); this.intValues.foreach(result += d.get(_)); result }
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
class BinaryFeatureVectorVariable[T] extends SparseCategoricalBinaryVectorVariable[T] {
  type VariableType <: BinaryFeatureVectorVariable[T]
  def this(initVals:Iterable[T]) = { this(); this.++=(initVals) }
}

