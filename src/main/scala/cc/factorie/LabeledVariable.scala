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

import la._

// Variables in "aimer/target" pairs, used for labeled data for training.
// The "target" is a container for the true, correct value.
// The "aimer" is the variable that is supposed to have the true value; it is aiming to have the target value, 
//  but may have some other value temporarily. 

// The "aimer" is a variable that should have the target value.  
// The "target" is the container for that target value, which also has a pointer back to its aimer. 

/** Sets the TargetType, which is the type of the container of another variable's target value,
    doing so in a way that the type is both concrete and can be overridden in subclasses. */
trait TargetType[+T<:Var with AimerType[Var]] {
  type TargetType = T
}
/** Sets the AimerType, which is the type of the variable that aims to have this target value,
    doing so in a way that the type is both concrete and can be overridden in subclasses. */
trait AimerType[+A<:Var] {
  type AimerType = A
}

/** A trait for all variables that are containers of target values.  
    Having this trait allows ZeroOneLossTemplate to use this type as a neighbor,
    and, for example, avoid trying to unroll for all DiscreteVectorVar. */
trait TargetVar extends Var with AimerType[Var] {
  /** Returns the variable that "aims" to have to have its value match this variable's as its target */
  def aimer: AimerType
}

/** A Variable that has a desired correct "target" value, but not a method to directly obtain that value. */
trait LabeledVar extends Var {
  def targetValue: Value
  def valueIsTarget: Boolean = value == targetValue
}

/** A Variable that has a desired correct target value,
    and also a "target" method returning the Variable that holds this target value. 
    This "target" variable is of type TargetVar, and should have a "aimer" method that 
    returns a pointer back to this Variable. */
trait LabeledVarWithTarget extends LabeledVar with TargetType[TargetVar] {
  self =>
  /** Stores the intended true "target" value for this variable. */
  def target: TargetType { type Value = self.Value }
  override def valueIsTarget: Boolean = value == target.value
  def targetValue = target.value.asInstanceOf[Value]
}

trait LabeledMutableVar[A] extends MutableVar[A] with LabeledVar {
  def setToTarget(implicit d:DiffList = null): Unit = set(targetValue)
}
trait LabeledMutableVarWithTarget[A] extends LabeledVarWithTarget with LabeledMutableVar[A] 



// Discrete variables with targets.

/** A container of a target value for discrete variables.  */
trait DiscreteTargetVar[V<:DiscreteValue] extends MutableDiscreteVar[V] with TargetVar with AimerType[LabeledMutableDiscreteVarWithTarget[V]] //with AimerType[DiscreteVariable]

trait LabeledDiscreteVar extends DiscreteVar with LabeledVar {
  def targetIntValue: Int  
  override def valueIsTarget: Boolean = targetIntValue == intValue
  //def targetValue: A = domain(targetIntValue).asInstanceOf[A]
}

/** These variables have a target value, but it may not necessarily be stored in a separate TargetVar variable. */
trait LabeledMutableDiscreteVar[A<:DiscreteValue] extends LabeledDiscreteVar with MutableDiscreteVar[A] with LabeledMutableVar[A] {
  override def setToTarget(implicit d:DiffList = null): Unit = set(targetIntValue)
}

/** A discrete variable that has a true, target "labeled" value, 
    separate from its current value. 
    @author Andrew McCallum */
trait LabeledMutableDiscreteVarWithTarget[A<:DiscreteValue] extends LabeledVarWithTarget with LabeledMutableDiscreteVar[A] with TargetType[DiscreteTargetVar[A]] {
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  @inline final def targetIntValue: Int = if (target eq null) -1 else target.asInstanceOf[DiscreteVar].intValue // TODO Work on removing this cast
  def targetIntValue_=(newValue:Int): Unit = target.asInstanceOf[MutableDiscreteVar[DiscreteValue]].set(newValue)(null) // TODO Work on removing this cast
  override def targetValue: Value = if (target eq null) null.asInstanceOf[Value] else target.value.asInstanceOf[Value] // TODO Consider trying to reinstate this
  def isUnlabeled = target eq null
  def unlabel = if (targetIntValue >= 0) targetIntValue = (-targetIntValue - 1) else throw new Error("Already unlabeled.")
  def relabel = if (targetIntValue < 0) targetIntValue = -(targetIntValue+1) else throw new Error("Already labeled.")
}

abstract class LabeledDiscreteVariable(targetValue:Int) extends DiscreteVariable(targetValue) with LabeledMutableDiscreteVarWithTarget[DiscreteValue] {
  self =>
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

trait LabeledMutableCategoricalVarWithTarget[V<:CategoricalValue[C],C] extends LabeledMutableCategoricalVar[V,C] with LabeledMutableDiscreteVarWithTarget[V] with TargetType[CategoricalTargetVar[V,C]]


/** A CategoricalVariable with a single value and a target value.
    @author Andrew McCallum
 */
abstract class LabeledCategoricalVariable[C](theTargetCategory:C) extends CategoricalVariable[C](theTargetCategory) with LabeledMutableCategoricalVarWithTarget[CategoricalValue[C],C] with TargetType[CategoricalVariable[C] with CategoricalTargetVar[CategoricalValue[C],C]] {
  self =>
  val target = new CategoricalTarget(theTargetCategory).asInstanceOf[TargetType]
  class CategoricalTarget(targetVal:C) extends CategoricalVariable(targetVal) with CategoricalTargetVar[CategoricalValue[C],C] /*with AimerType[CoordinatedLabeledCategoricalVariable[C]]*/ {
    def domain = self.domain
    def aimer = self
  }
}


// For Booleans

trait LabeledBooleanVar extends LabeledMutableCategoricalVarWithTarget[BooleanValue,Boolean] with BooleanVar
class LabeledBooleanVariable(targetVal:Boolean) extends BooleanVariable(targetVal) with LabeledBooleanVar {
  self =>
  val target = new LabeledBooleanTarget(targetVal).asInstanceOf[TargetType] // new BooleanLabelTarget(targetVal, this)
  class LabeledBooleanTarget(targetVal:Boolean) extends BooleanVariable(targetVal) with CategoricalTargetVar[BooleanValue, Boolean] /*with AimerType[CoordinatedLabeledBooleanVariable]*/ {
    def aimer = self
  }
} 


// Templates

class HammingTemplate[A<:LabeledVarWithTarget]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends TupleTemplateWithStatistics2[A,A#TargetType] {
  def unroll1(aimer:A) = Factor(aimer, aimer.target)
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 1.0 else 0.0 // TODO
  def accuracy(context: Iterable[A]): Double = context.map(currentScore(_)).sum / context.size
}
object HammingObjective extends HammingTemplate[LabeledVarWithTarget]

class HammingLossTemplate[A<:LabeledVarWithTarget]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends TupleTemplateWithStatistics2[A,A#TargetType] {
  def unroll1(aimer:A) = Factor(aimer, aimer.target)
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 0.0 else 1.0
  override def valuesScore(t: Tensor) = t match {
    case v: SingletonBinaryTensorLike2 => if (v.singleIndex1 == v.singleIndex2) 0.0 else 1.0
    case v: SingletonBinaryLayeredTensorLike2 => if (v.singleIndex1 == v.inner.maxIndex) 0.0 else 1.0
    case v: Outer1Tensor2 =>
      (v.tensor1, v.tensor2) match {
        case (v1: SingletonBinaryTensorLike1, v2: Tensor1) =>
          if (v1.singleIndex == v2.maxIndex) 0.0 else 1.0
      }
  }
}
object HammingLoss extends HammingLossTemplate[LabeledVarWithTarget]

// Evaluation

/** Stores the results of evaluating per-label accuracy and other measures.
    Note, this is not per-field accuracy. */
class LabeledDiscreteEvaluation[C](val domain: DiscreteDomain) {
  def this(labels:Iterable[LabeledDiscreteVar]) = { this(labels.head.domain); this ++= labels }

  private val _fp = new Array[Int](domain.size)
  private val _fn = new Array[Int](domain.size)
  private val _tp = new Array[Int](domain.size)
  private val _tn = new Array[Int](domain.size)
  private var _size: Int = 0
  def count = _size

  def +=(label: LabeledDiscreteVar): this.type = {
    require(label.domain eq domain)
    _size += 1
    val trueIndex = label.targetIntValue
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
  def ++=(labels: Iterable[LabeledDiscreteVar]): this.type = { labels.foreach(+=(_)); this }
  def +++=(labels: Iterable[Iterable[LabeledDiscreteVar]]): this.type = { labels.foreach(_.foreach(+=(_))); this }
  // TODO Consider removing these
  def +=(a:Attr, f:Attr=>LabeledDiscreteVar): this.type = this += f(a)
  def ++=(as:Iterable[Attr], f:Attr=>LabeledDiscreteVar): this.type = { as.foreach(this += f(_)); this }
  def +++=(as:Iterable[Iterable[Attr]], f:Attr=>LabeledDiscreteVar): this.type = { as.foreach(_.foreach(this += f(_))); this }
  
  def accuracy: Double = (_tp.sum + _tn.sum).toDouble / _size
  def precision(labelIndex:Int): Double = if (_tp(labelIndex) + _fp(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fp(labelIndex))
  def precision(labelValue:DiscreteValue): Double = precision(labelValue.intValue)
  //def precision(category:C): Double = precision(domain.getIndex(category))
  def precision: Double = precision(0)
  def recall(labelIndex:Int): Double = if (_tp(labelIndex) + _fn(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fn(labelIndex))
  def recall(labelValue:DiscreteValue): Double = recall(labelValue.intValue)
  //def recall(category:C): Double = recall(domain.getIndex(category))
  def recall: Double = recall(0)
  def f1(labelIndex:Int): Double = if (precision(labelIndex) + recall(labelIndex) == 0.0) 0.0 else 2.0 * precision(labelIndex) * recall(labelIndex) / (precision(labelIndex) + recall(labelIndex))
  def f1(labelValue:DiscreteValue): Double = f1(labelValue.intValue)
  //def f1(category:C): Double = f1(domain.getIndex(category))
  def f1: Double = f1(0)
  def tp(labelIndex:Int): Int = _tp(labelIndex)
  def tp(labelValue:DiscreteValue): Int = _tp(labelValue.intValue)
  //def tp(category:C): Int = _tp(domain.getIndex(category))
  def tp: Int = _tp(0)
  def fp(labelIndex:Int): Int = _fp(labelIndex)
  def fp(labelValue:DiscreteValue): Int = _fp(labelValue.intValue)
  //def fp(category:C): Int = _fp(domain.getIndex(category))
  def fp: Int = _fp(0)
  def tn(labelIndex:Int): Int = _tn(labelIndex)
  def tn(labelValue:DiscreteValue): Int = _tn(labelValue.intValue)
  //def tn(category:C): Int = _tn(domain.getIndex(category))
  def tn: Int = _tn(0)
  def fn(labelIndex:Int): Int = _fn(labelIndex)
  def fn(labelValue:DiscreteValue): Int = _fn(labelValue.intValue)
  //def fn(category:C): Int = _fn(domain.getIndex(category))
  def fn: Int = _fn(0)
  
  //def correctCount(labelIndex:Int) = _tp(labelIndex)
  //def missCount(labelIndex:Int) = _fn(labelIndex)
  //def alarmCount(labelIndex:Int) = _fp(labelIndex)
  def overallEvalString: String = "accuracy=%-8f".format(accuracy)
  def evalString(labelIndex:Int): String = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(domain(labelIndex).toString, f1(labelIndex), precision(labelIndex), recall(labelIndex), tp(labelIndex), fp(labelIndex), fn(labelIndex), tp(labelIndex)+fn(labelIndex), tp(labelIndex)+fp(labelIndex))
  def evalString: String = (0 until domain.size).map(evalString(_)).mkString("\n")
  override def toString = evalString
}

