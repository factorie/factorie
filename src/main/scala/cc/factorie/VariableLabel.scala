/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
//import scalala.Scalala._
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

// Categorical variables that have true values are referred to as 'Labels'

// TODO Consider using the word "Target" for variables that we are trying to predict at training and/or test time.
// The word "Hidden" should perhaps also be used somewhere?

/** A variable of finite enumerated values that has a true "labeled" value, separate from its current value. 
    @author Andrew McCallum */
// TODO We could make version of this for IntVar: TrueIntVar
trait TrueCategoricalVar[A] extends TrueSetting {
  this: CategoricalVariable[A] =>
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  def trueIntValue: Int
  def trueIntValue_=(newValue:Int): Unit
  def trueValue: VariableType#VariableType#CategoryType = if (trueIntValue >= 0) domain.get(trueIntValue) else null.asInstanceOf[VariableType#VariableType#CategoryType]
  def trueValue_=(x:A) = if (x == null) trueIntValue = -1 else trueIntValue = domain.index(x)
  def setToTruth(implicit d:DiffList): Unit = set(trueIntValue)
  def valueIsTruth: Boolean = trueIntValue == intValue
  def isUnlabeled = trueIntValue < 0
  def unlabel = if (trueIntValue >= 0) trueIntValue = -trueIntValue - 1 else throw new Error("Already unlabeled.")
  def relabel = if (trueIntValue < 0) trueIntValue = -(trueIntValue+1) else throw new Error("Already labeled.")
}

abstract class TrueDiscreteTemplate[V<:DiscreteVariable with TrueSetting](implicit m:Manifest[V]) extends TemplateWithVectorStatistics1[V] {
  def score(s:Stat) = if (s.s1.valueIsTruth) 1.0 else 0.0
}
class TrueLabelTemplate[V<:CoordinatedLabelVariable[_]:Manifest]/*(implicit m:Manifest[V])*/ extends TrueDiscreteTemplate[V] /*()(m)*/




/** A variable with a single index and a true value.
    Subclasses are allowed to override 'set' to coordinate the value of other variables with this one.
    @author Andrew McCallum
    @see LabelVariable
*/
@DomainInSubclasses
abstract class CoordinatedLabelVariable[A](trueval:A) extends CategoricalVariable[A](trueval) with TrueCategoricalVar[A] {
  type VariableType <: CoordinatedLabelVariable[A]
  var trueIntValue = domain.index(trueval)
}

/** A CategoricalVariable with a single value and a true value.
    Subclasses cannot override 'set' to coordinate the value of other variables with this one;
    hence belief propagation can be used with these variables.
    @author Andrew McCallum
    @see CoordinatedLabelVariable
 */
@DomainInSubclasses
class LabelVariable[T](trueval:T) extends CoordinatedLabelVariable(trueval) with NoVariableCoordination {
  type VariableType <: LabelVariable[T]
  // TODO Does this ext line really provide the protection we want from creating variable-value coordination?  No.  But it does catch some errors.
  override final def set(index: Int)(implicit d: DiffList) = super.set(index)(d)
}

/** A Label with a StringDomain.  StringDomains can be conveniently initialized, as in
    class NerLabel extends StringDomain { val PER, ORG, LOC, MISC, O = Value; freeze } // Then Domain[NerLabel].PER == "PER" 
    @author Andrew McCallum */
// TODO But should this be Coordinated or Uncoordinated, or should we make both.
@DomainInSubclasses
class StringLabelVariable(trueval:String) extends LabelVariable[String](trueval) {
  type VariableType <: StringLabelVariable
  type DomainType <: StringDomain[VariableType]
  class DomainClass extends StringDomain[VariableType]()(null)
}
