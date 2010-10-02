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

// Categorical variables that have true values are referred to as 'Labels'

/** A variable of finite enumerated values that has a true "labeled" value, separate from its current value. 
    @author Andrew McCallum */
// TODO We could make version of this for IntegerVar: TrueIntegerVar
trait DiscreteVariableWithTrueSetting extends DiscreteVariable with TrueSetting {
  this: DiscreteVariable =>
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  def trueIntValue: Int
  def trueIntValue_=(newValue:Int): Unit
  def setToTruth(implicit d:DiffList): Unit = set(trueIntValue)
  def valueIsTruth: Boolean = trueIntValue == intValue
  def isUnlabeled = trueIntValue < 0
  def unlabel = if (trueIntValue >= 0) trueIntValue = -trueIntValue - 1 else throw new Error("Already unlabeled.")
  def relabel = if (trueIntValue < 0) trueIntValue = -(trueIntValue+1) else throw new Error("Already labeled.")
}

trait CategoricalVariableWithTrueSetting[A] extends DiscreteVariableWithTrueSetting {
  this: CategoricalVariable[A] =>
  def trueValue_=(x:A) = if (x == null) trueIntValue = -1 else trueIntValue = domain.index(x)
  def trueValue: VariableType#VariableType#CategoryType = if (trueIntValue >= 0) domain.get(trueIntValue) else null.asInstanceOf[VariableType#VariableType#CategoryType]
}


/** A variable with a single index and a true value.
    Subclasses are allowed to override 'set' to coordinate the value of other variables with this one.
    @author Andrew McCallum
    @see LabelVariable
*/
@DomainInSubclasses
abstract class CoordinatedLabelVariable[A](trueval:A) extends CategoricalVariable[A](trueval) with CategoricalVariableWithTrueSetting[A] {
  type VariableType <: CoordinatedLabelVariable[A]
  var trueIntValue = domain.index(trueval)
  //lazy val _domain = super.domain // Considering this to speed BeliefPropagation, but making Variable.domain not "final" also costs.
  //override def domain: VariableType#DomainType = _domain
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


// Templates

abstract class DiscreteVariableWithTrueSettingTemplate[V<:DiscreteVariable with TrueSetting](implicit m:Manifest[V]) extends TemplateWithVectorStatistics1[V] {
  def score(s:Stat) = if (s._1.valueIsTruth) 1.0 else 0.0
}

class Label01LossTemplate[V<:CoordinatedLabelVariable[_]:Manifest] extends DiscreteVariableWithTrueSettingTemplate[V]
