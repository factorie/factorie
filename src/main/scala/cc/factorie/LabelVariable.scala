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

// Variables in "aimer/target" pairs, used for labeled data for training.
// The "target" is a container for the true, correct value.
// The "aimer" is the variable that is supposed to have the true value; it is aiming to have the target value, 
//  but may have some other value temporarily. 

// The "aimer" is a variable that should have the target value.  
// The "target" is the container for that target value, which also has a pointer back to its aimer. 

/** Sets the TargetType, which is the type of the container of another variable's target value,
    doing so in a way that the type is both concrete and can be overridden in subclasses. */
trait TargetType[+T<:Variable with AimerType[Variable]] {
  type TargetType = T
}
/** Sets the AimerType, which is the type of the variable that aims to have this target value,
    doing so in a way that the type is both concrete and can be overridden in subclasses. */
trait AimerType[+A<:Variable] {
  type AimerType = A
}

/** A trait for all variables that are containers of target values.  
    Having this trait allows ZeroOneLossTemplate to use this type as a neighbor,
    and, for example, avoid trying to unroll for all DiscreteVectorVar. */
trait TargetVar extends Variable with AimerType[Variable] {
  /** Returns the variable that "aims" to have to have its value match this variable's as its target */
  def aimer: AimerType
}

/** A Variable that has a desired correct "target" value, but not a method to directly obtain that value. */
// Was VarWithTargetValue
trait LabeledVar extends Variable {
  //def targetValue: Value
  def valueIsTarget: Boolean //= value == targetValue
  def setToTarget(implicit d:DiffList = null): Unit
}

/** A Variable that has a desired correct "target" value, accessible through its "targetValue" method. */
//trait LabeledVar[+A] extends Variable with ValueBound[A] /*with TargetType[Var[A]]*/ {
//  def targetValue: A
//  def valueIsTarget: Boolean
//}

/** A Variable that has a desired correct target value,
    and also a "target" method returning the Variable that holds this target value. 
    This "target" variable is of type TargetVar, and should have a "aimer" method that 
    returns a pointer back to this Variable. */
trait LabeledVarWithTarget extends LabeledVar with TargetType[TargetVar] {
  self =>
  //type TargetType = TargetType with ValueBound[this.Value]
  /** Stores the intended true "target" value for this variable. */
  def target: TargetType { type Value = self.Value }
  def valueIsTarget: Boolean = value == target.value // TODO Consider moving to LabeledVar[].
  //def targetValue = target.value.asInstanceOf[Value]
}

trait LabeledMutableVar[A] extends MutableVar[A] with LabeledVar {
  //def setToTarget(implicit d:DiffList = null): Unit = set(targetValue)
  //def setToTarget(implicit d:DiffList = null): Unit = set(target.value)
}
trait LabeledMutableVarWithTarget[A] extends LabeledVarWithTarget with LabeledMutableVar[A] 

//trait VarWithTarget extends VarWithTargetValue with TargetType[TargetVar] {
//  self =>
//  //type TargetType = TargetType with ValueType[this.Value]
//  /** Stores the intended true "target" value for this variable. */
//  def target: TargetType { type Value = self.Value }
//  def valueIsTarget: Boolean = value == target.value // TODO Consider moving to LabeledVar[].
//}



// Discrete variables with targets.

/** A container of a target value for discrete variables.  */
trait DiscreteTargetVar[V<:DiscreteValue] extends MutableDiscreteVar[V] with TargetVar with AimerType[LabeledMutableDiscreteVarWithTarget[V]] //with AimerType[DiscreteVariable]

/** These variables have a target value, but it may not necessarily be stored in a separate TargetVar variable. */
trait LabeledMutableDiscreteVar[A<:DiscreteValue] extends MutableDiscreteVar[A] with LabeledMutableVar[A] {
  def targetIntValue: Int
  def targetValue: A = domain(targetIntValue).asInstanceOf[A]
  override def valueIsTarget: Boolean = targetIntValue == intValue
  override def setToTarget(implicit d:DiffList = null): Unit = set(targetIntValue)
}

/** A discrete variable that has a true, target "labeled" value, 
    separate from its current value. 
    @author Andrew McCallum */
// TODO We could also make version of this for IntegerVar: IntegerTargetValue
// TODO Rename this DiscreteVariableWithTarget because it must include DiscreteVariable
trait LabeledMutableDiscreteVarWithTarget[A<:DiscreteValue] extends LabeledVarWithTarget with LabeledMutableDiscreteVar[A] with TargetType[DiscreteTargetVar[A]] {
  //type TargetType <: DiscreteTargetVar[A]
  //type TargetType = DiscreteValue
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  @inline final def targetIntValue: Int = if (target eq null) -1 else target.asInstanceOf[DiscreteVar].intValue // TODO Work on removing this cast
  def targetIntValue_=(newValue:Int): Unit = target.asInstanceOf[MutableDiscreteVar[DiscreteValue]].set(newValue)(null) // TODO Work on removing this cast
  //def setToTarget(implicit d:DiffList): Unit = set(target.intValue)
  //override def targetValue: Value = if (target eq null) null.asInstanceOf[Value] else target.value.asInstanceOf[Value] // TODO Consider trying to reinstate this
  def isUnlabeled = target eq null
  def unlabel = if (targetIntValue >= 0) targetIntValue = (-targetIntValue - 1) else throw new Error("Already unlabeled.")
  def relabel = if (targetIntValue < 0) targetIntValue = -(targetIntValue+1) else throw new Error("Already labeled.")
}

abstract class LabeledDiscreteVariable(targetValue:Int) extends DiscreteVariable(targetValue) with LabeledMutableDiscreteVarWithTarget[DiscreteValue] {
  self =>
  //type TargetType = DiscreteTargetVar[DiscreteValue]
  val target = new DiscreteTarget(targetValue).asInstanceOf[TargetType]
  class DiscreteTarget(targetVal:Int) extends DiscreteVariable(targetVal) with DiscreteTargetVar[DiscreteValue] /*with AimerType[LabeledDiscreteVariable]*/ {
    def domain = self.domain
    def aimer = self
  }
}

// Categorical variables with targets

trait CategoricalTargetVar[V<:CategoricalValue[C],C] extends MutableCategoricalVar[V,C] with DiscreteTargetVar[V] with AimerType[LabeledMutableCategoricalVarWithTarget[V,C]] /*with AimerType[CategoricalVariable[C]]*/

trait LabeledMutableCategoricalVar[V<:CategoricalValue[C],C] extends MutableCategoricalVar[V,C] with LabeledMutableDiscreteVar[V] {
  def targetCategory: C = targetValue.category
} 

trait LabeledMutableCategoricalVarWithTarget[V<:CategoricalValue[C],C] extends LabeledMutableCategoricalVar[V,C] with LabeledMutableDiscreteVarWithTarget[V] with TargetType[CategoricalTargetVar[V,C]] {
  //type TargetType <: CategoricalTargetVar[V,C]
  //def targetCategory: C = target.categoryValue.asInstanceOf[C]
}


/** A variable with a single index and a true value.
    Subclasses are allowed to override 'set' to coordinate the value of other variables with this one.
    @author Andrew McCallum
    @see LabelVariable
*/
abstract class CoordinatedLabeledCategoricalVariable[C](theTargetCategory:C) extends CategoricalVariable[C](theTargetCategory) with LabeledMutableCategoricalVarWithTarget[CategoricalValue[C],C] with TargetType[CategoricalVariable[C] with CategoricalTargetVar[CategoricalValue[C],C]] {
  self =>
  //type TargetType = CategoricalTargetVar[CategoricalValue[C],C]
  val target = new CategoricalTarget(theTargetCategory).asInstanceOf[TargetType]
  class CategoricalTarget(targetVal:C) extends CategoricalVariable(targetVal) with CategoricalTargetVar[CategoricalValue[C],C] /*with AimerType[CoordinatedLabeledCategoricalVariable[C]]*/ {
    def domain = self.domain
    def aimer = self
  }
}

/** A CategoricalVariable with a single value and a true value.
    Subclasses cannot override 'set' to coordinate the value of other variables with this one;
    hence belief propagation can be used with these variables.
    @author Andrew McCallum
    @see CoordinatedLabelVariable
 */
abstract class LabeledCategoricalVariable[T](targetVal:T) extends CoordinatedLabeledCategoricalVariable(targetVal) with NoVariableCoordination {
  // TODO Does this next line really provide the protection we want from creating variable-value coordination?  No.  But it does catch some errors.
  override final def set(newValue: Int)(implicit d: DiffList) = super.set(newValue)(d)
}


// For Booleans

//class BooleanTargetVar(t:Boolean, val aimer:A) extends BooleanVariable(t) with DiscreteTargetVar[BooleanValue] with AimerType[BooleanLabelVar]
trait LabeledBooleanVar extends LabeledMutableCategoricalVarWithTarget[BooleanValue,Boolean] with BooleanVar
class CoordinatedLabeledBooleanVariable(targetVal:Boolean) extends BooleanVariable(targetVal) with LabeledBooleanVar {
  self =>
  val target = new LabeledBooleanTarget(targetVal).asInstanceOf[TargetType] // new BooleanLabelTarget(targetVal, this)
  class LabeledBooleanTarget(targetVal:Boolean) extends BooleanVariable(targetVal) with CategoricalTargetVar[BooleanValue, Boolean] /*with AimerType[CoordinatedLabeledBooleanVariable]*/ {
    def aimer = self //CoordinatedLabeledBooleanVariable.this.asInstanceOf[AimerType]
  }
} 
class LabeledBooleanVariable(targetVal:Boolean) extends CoordinatedLabeledBooleanVariable(targetVal) with NoVariableCoordination {
  override final def set(newValue: Int)(implicit d: DiffList) = super.set(newValue)(d)
}


// Templates

class HammingLossTemplate[A<:LabeledVarWithTarget]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends TupleTemplateWithStatistics2[A,A#TargetType] {
  def unroll1(aimer:A) = Factor(aimer, aimer.target)
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 1.0 else 0.0 // TODO 
  //def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 1.0 else 0.0
}

//class HammingLossTemplate[A<:LabeledVarWithTarget[_]]()(implicit am:Manifest[A]) extends TupleTemplate1[A] {
//  def unroll1(aimer:A) = Factor(aimer, aimer.target)
//  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
//  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 1.0 else 0.0
//}

object HammingLossObjective extends HammingLossTemplate[LabeledVarWithTarget]

// Evaluation

/** Stores the results of evaluating per-label accuracy and other measures.
    Note, this is not per-field accuracy. */
class LabelEvaluation[C](val domain: CategoricalDomain[C]) {
  //val labelValue: String, var targetIndex:Int) {
  def this(labels:Iterable[LabeledCategoricalVariable[C]]) = { this(labels.head.domain); this ++= labels }

  private val _fp = new Array[Int](domain.size)
  private val _fn = new Array[Int](domain.size)
  private val _tp = new Array[Int](domain.size)
  private val _tn = new Array[Int](domain.size)
  private var _size: Int = 0
  def count = _size

  /*private val targetIndex = -1 // TODO replace this: Domain[L](m).index(labelValue) */

  //def ++=(tokenseqs:Seq[Seq[{def label:LabelVariable[String]}]]) = tokenseqs.foreach(ts => this += ts.map(_.label))

  def +=(label: LabeledCategoricalVariable[C]): this.type = {
    require(label.domain eq domain)
    _size += 1
    val trueIndex = label.target.intValue
    val predIndex = label.intValue
    for (targetIndex <- 0 until domain.size) {
      if (targetIndex == trueIndex) {
        if (trueIndex == predIndex)
          _tp(targetIndex) += 1
        else
          _fn(targetIndex) += 1
      } else if (targetIndex == predIndex) {
        if (trueIndex == predIndex)
          _tp(targetIndex) += 1
        else
          _fp(targetIndex) += 1
      }
    }
    this
  }
  def ++=(labels: Iterable[LabeledCategoricalVariable[C]]): this.type = { labels.foreach(+=(_)); this }
  def +++=(labels: Iterable[Iterable[LabeledCategoricalVariable[C]]]): this.type = { labels.foreach(_.foreach(+=(_))); this }
  // TODO Consider removing these
  def +=(a:Attr, f:Attr=>LabeledCategoricalVariable[C]): this.type = this += f(a)
  def ++=(as:Iterable[Attr], f:Attr=>LabeledCategoricalVariable[C]): this.type = { as.foreach(this += f(_)); this }
  def +++=(as:Iterable[Iterable[Attr]], f:Attr=>LabeledCategoricalVariable[C]): this.type = { as.foreach(_.foreach(this += f(_))); this }
  
  def accuracy: Double = (_tp.sum + _tn.sum).toDouble / _size
  def precision(labelIndex:Int): Double = if (_tp(labelIndex) + _fp(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fp(labelIndex))
  def precision(labelValue:DiscreteValue): Double = precision(labelValue.intValue)
  def precision(category:C): Double = precision(domain.getIndex(category))
  def precision: Double = precision(0)
  def recall(labelIndex:Int): Double = if (_tp(labelIndex) + _fn(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fn(labelIndex))
  def recall(labelValue:DiscreteValue): Double = recall(labelValue.intValue)
  def recall(category:C): Double = recall(domain.getIndex(category))
  def recall: Double = recall(0)
  def f1(labelIndex:Int): Double = if (precision(labelIndex) + recall(labelIndex) == 0.0) 0.0 else 2.0 * precision(labelIndex) * recall(labelIndex) / (precision(labelIndex) + recall(labelIndex))
  def f1(labelValue:DiscreteValue): Double = f1(labelValue.intValue)
  def f1(category:C): Double = f1(domain.getIndex(category))
  def f1: Double = f1(0)
  def tp(labelIndex:Int): Int = _tp(labelIndex)
  def tp(labelValue:DiscreteValue): Int = _tp(labelValue.intValue)
  def tp(category:C): Int = _tp(domain.getIndex(category))
  def tp: Int = _tp(0)
  def fp(labelIndex:Int): Int = _fp(labelIndex)
  def fp(labelValue:DiscreteValue): Int = _fp(labelValue.intValue)
  def fp(category:C): Int = _fp(domain.getIndex(category))
  def fp: Int = _fp(0)
  def tn(labelIndex:Int): Int = _tn(labelIndex)
  def tn(labelValue:DiscreteValue): Int = _tn(labelValue.intValue)
  def tn(category:C): Int = _tn(domain.getIndex(category))
  def tn: Int = _tn(0)
  def fn(labelIndex:Int): Int = _fn(labelIndex)
  def fn(labelValue:DiscreteValue): Int = _fn(labelValue.intValue)
  def fn(category:C): Int = _fn(domain.getIndex(category))
  def fn: Int = _fn(0)
  
  //def correctCount(labelIndex:Int) = _tp(labelIndex)
  //def missCount(labelIndex:Int) = _fn(labelIndex)
  //def alarmCount(labelIndex:Int) = _fp(labelIndex)
  def evalString(labelIndex:Int): String = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(domain.category(labelIndex).toString, f1(labelIndex), precision(labelIndex), recall(labelIndex), tp(labelIndex), fp(labelIndex), fn(labelIndex), tp(labelIndex)+fn(labelIndex), tp(labelIndex)+fp(labelIndex))
  def evalString: String = (0 until domain.size).map(evalString(_)).mkString("\n")
  override def toString = evalString
}

