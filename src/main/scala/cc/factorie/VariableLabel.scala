/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

// Categorical variables that have true values are referred to as 'Labels'

// TODO Consider using the word "Target" for variables that we are trying to predict at training and/or test time.
// The word "Hidden" should perhaps also be used somewhere.

/** A variable of finite enumerated values that has a true "labeled" value, separate from its current value. 
    @author Andrew McCallum */
// TODO We could make version of this for OrdinalValue: TrueOrdinalValue
trait TrueCategoricalValue extends TrueSetting {
  this : CategoricalVariable =>
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  var trueIndex: Int
  def trueValue: VariableType#VariableType#ValueType = if (trueIndex >= 0) domain.get(trueIndex) else null.asInstanceOf[VariableType#VariableType#ValueType]
  def setToTruth(implicit d:DiffList): Unit = setByIndex(trueIndex)
  def valueIsTruth: Boolean = trueIndex == index
  def isUnlabeled = trueIndex < 0
  def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex - 1 else throw new Error("Already unlabeled.")
  def relabel = if (trueIndex < 0) trueIndex = -(trueIndex+1) else throw new Error("Already labeled.")
}

// TODO consider moving TrueIndexedValue to inside with this:TrueIndexedValue => ?
@DomainInSubclasses
abstract trait TypedTrueCategoricalValue[T] extends TrueCategoricalValue with TypedCategoricalVariable[T] {
  def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
}

abstract class TrueCategoricalTemplate[V<:CategoricalVariable with TrueCategoricalValue](implicit m:Manifest[V]) extends TemplateWithVectorStatistics1[V] {
  def score(s:Stat) = if (s.s1.index == s.s1.trueIndex) 1.0 else 0.0
}

class TrueLabelTemplate[V<:CoordinatedLabelVariable[_]](implicit m:Manifest[V]) extends TrueCategoricalTemplate[V]()(m)


/** A variable with a single index and a true value.
    Subclasses can override setByIndex to coordinate the value of other variables with this one.
    @author Andrew McCallum
    @see LabelVariable
*/
@DomainInSubclasses
class CoordinatedLabelVariable[T](trueval:T) extends CoordinatedEnumVariable[T](trueval) with TypedTrueCategoricalValue[T] {
  type VariableType <: CoordinatedLabelVariable[T]
  var trueIndex = domain.index(trueval)
  setByIndex(domain.index(trueval))(null)
}

/** A CategoricalVariable with a single value and a true value.
    Subclasses cannot override setByIndex to coordinate the value of other variables with this one;
    hence belief propagation can be used with these variables.
    @author Andrew McCallum
    @see CoordinatedLabelVariable
 */
@DomainInSubclasses
class LabelVariable[T](trueval:T) extends CoordinatedLabelVariable(trueval) with UncoordinatedCategoricalVariable {
  type VariableType <: LabelVariable[T]
  //override final def set(newValue:T)(implicit d: DiffList) = super.set(newValue)(d)
  //override final def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}

/** A Label with a StringDomain.  StringDomains can be conveniently initialized, as in
    class NerLabel extends StringDomain { val PER, ORG, LOC, MISC, O = Value; freeze } // Then Domain[NerLabel].PER == "PER" 
    @author Andrew McCallum */
// TODO But should this be Coordinated or Uncoordinated, or should we make both.
@DomainInSubclasses
class StringLabelVariable(trueval:String) extends LabelVariable[String](trueval) {
  type VariableType <: StringLabelVariable
  type DomainType <: StringDomain[VariableType]
  class DomainClass extends StringDomain[VariableType]
  
}