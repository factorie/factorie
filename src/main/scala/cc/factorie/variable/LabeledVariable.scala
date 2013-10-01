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

package cc.factorie.variable

import cc.factorie.util.Attr
import cc.factorie._
import cc.factorie.model.TupleTemplateWithStatistics2

// Naming explanation:
// Variables in "aimer/target" pairs, used for labeled data for training.
// The "target" is a container for the true, correct value.
// The "aimer" is the variable that is supposed to have the true value; it is aiming to have the target value, 
//  but may have some other value temporarily. 

// The "aimer" is a variable that should have the target value.  
// The "target" is the container for that target value, which also has a pointer back to its aimer. 

/** A trait for all variables that are containers of target values.
    Having this trait allows ZeroOneLossTemplate to use this type as a neighbor,
    and, for example, to avoid trying to unroll for all DiscreteVectorVar.
    @author Andrew McCallum */
trait TargetVar extends Var {
  type AimerType <: Var
  /** Returns the variable that "aims" to have to have its value match this variable's as its target */
  def aimer: AimerType
}

/** A Variable that has a desired correct "target" value, 
    but not a method to directly obtain the "target" object that holds value.
    @author Andrew McCallum */
trait LabeledVar extends Var {
  def targetValue: Value
  def valueIsTarget: Boolean = value == targetValue
}

/** An abstract variable that has a desired correct target value,
    and also a "target" method returning the variable that holds this target value. 
    This "target" variable is of type TargetVar, and should have a "aimer" method that 
    returns a pointer back to this Variable.
    @author Andrew McCallum */
trait LabeledVarWithTarget extends LabeledVar {
  self =>
  type TargetType <: TargetVar
  /** Stores the intended true "target" value for this variable. */
  def target: TargetType { type Value = self.Value }
  override def valueIsTarget: Boolean = value == target.value
  def targetValue = target.value.asInstanceOf[Value]
}

/** An abstract mutable variable that has a desired correct "target" value.
    @author Andrew McCallum  */
trait LabeledMutableVar extends MutableVar with LabeledVar {
  def setToTarget(implicit d:DiffList = null): Unit = set(targetValue)
}

/** An abstract mutable variable that has a desired correct target value,
    and also a "target" method returning the variable that holds this target value. 
    @author Andrew McCallum */
trait LabeledMutableVarWithTarget extends LabeledVarWithTarget with LabeledMutableVar



// Discrete variables with targets.

/** A container of a target value for discrete variables.
    @author Andrew McCallum  */
trait DiscreteTargetVar extends MutableDiscreteVar with TargetVar {
  type AimerType <: LabeledMutableDiscreteVarWithTarget
}

/** An abstract discrete variable that has a desired correct "target" value.
    author Andrew McCallum */
trait LabeledDiscreteVar extends DiscreteVar with LabeledVar {
  def targetIntValue: Int  
  override def valueIsTarget: Boolean = targetIntValue == intValue
  //def targetValue: A = domain(targetIntValue).asInstanceOf[A]
}

/** An abstract mutable discrete variable that has a desired correct "target" value.
    This variable has a target value, but it may not necessarily be stored in a separate TargetVar variable.
    @author Andrew McCallum */
trait LabeledMutableDiscreteVar extends LabeledDiscreteVar with MutableDiscreteVar with LabeledMutableVar {
  override def setToTarget(implicit d:DiffList = null): Unit = set(targetIntValue)
}

/** An abstract discrete variable that has a true, target "labeled" value, 
    separate from its current value. 
    @author Andrew McCallum */
trait LabeledMutableDiscreteVarWithTarget extends LabeledVarWithTarget with LabeledMutableDiscreteVar {
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  @inline final def targetIntValue: Int = if (target eq null) -1 else target.asInstanceOf[DiscreteVar].intValue // TODO Work on removing this cast
  def targetIntValue_=(newValue:Int): Unit = target.asInstanceOf[DiscreteTargetVar with MutableDiscreteVar].set(newValue)(null) // TODO Work on removing this cast
  override def targetValue: Value = if (target eq null) null.asInstanceOf[Value] else target.value.asInstanceOf[Value]
  // TODO Consider trying to reinstate this
  def isUnlabeled = target eq null
  def unlabel() = if (targetIntValue >= 0) targetIntValue = -targetIntValue - 1 else throw new Error("Already unlabeled.")
  def relabel() = if (targetIntValue < 0) targetIntValue = -(targetIntValue+1) else throw new Error("Already labeled.")
}

/** A mutable discrete variable that has a true, target "labeled" value, separate from its current value.
    The only abstract method is "domain".    
    @author Andrew McCallum */
abstract class LabeledDiscreteVariable(targetValue:Int) extends DiscreteVariable(targetValue) with LabeledMutableDiscreteVarWithTarget {
  self =>
  type TargetType = DiscreteTarget
  val target = new DiscreteTarget(targetValue) // TODO Consider making this a var, so that it can be set to null if we don't want one. -akm
  class DiscreteTarget(targetVal:Int) extends DiscreteVariable(targetVal) with DiscreteTargetVar {
    type AimerType = LabeledDiscreteVariable
    def domain = self.domain
    def aimer = self
  }
}


// Categorical variables with targets

/** A container of a target value for categorical variables.
    @author Andrew McCallum  */
trait CategoricalTargetVar[C] extends MutableCategoricalVar[C] with DiscreteTargetVar  {
  type AimerType <: LabeledMutableCategoricalVarWithTarget[C]
}

/** An abstract mutable categorical variable that has a desired correct "target" value.
    This variable has a target value, but it may not necessarily be stored in a separate TargetVar variable.
    @author Andrew McCallum */
trait LabeledMutableCategoricalVar[C] extends MutableCategoricalVar[C] with LabeledMutableDiscreteVar {
  def targetCategory: C = targetValue.category
} 

/** An abstract categorical variable that has a true, target "labeled" value, 
    separate from its current value. 
    @author Andrew McCallum */
trait LabeledMutableCategoricalVarWithTarget[C] extends LabeledMutableCategoricalVar[C] with LabeledMutableDiscreteVarWithTarget {
  type TargetType <: CategoricalTargetVar[C]
}


/** A mutable categorical variable that has a true, target "labeled" value, separate from its current value.
    The only abstract method is "domain".    
    @author Andrew McCallum */
abstract class LabeledCategoricalVariable[C](theTargetCategory:C) extends CategoricalVariable[C](theTargetCategory) with LabeledMutableCategoricalVarWithTarget[C] {
  self =>
  override type TargetType = CategoricalVariable[C] with CategoricalTargetVar[C]
  val target = new CategoricalTarget(theTargetCategory).asInstanceOf[TargetType] // TODO Consider making this a var, so that it can be set to null if we don't want one. -akm
  class CategoricalTarget(targetVal:C) extends CategoricalVariable(targetVal) with CategoricalTargetVar[C] {
    type AimerType = LabeledCategoricalVariable[C]
    def domain = LabeledCategoricalVariable.this.domain
    def aimer = self
  }
}


// For Booleans

trait LabeledBooleanVar extends LabeledMutableCategoricalVarWithTarget[Boolean] with BooleanVar
class LabeledBooleanVariable(targetVal:Boolean) extends BooleanVariable(targetVal) with LabeledBooleanVar {
  self =>
  type TargetType = LabeledBooleanTarget
  val target = new LabeledBooleanTarget(targetVal) // new BooleanLabelTarget(targetVal, this)
  class LabeledBooleanTarget(targetVal:Boolean) extends BooleanVariable(targetVal) with CategoricalTargetVar[Boolean] {
    type AimerType = LabeledBooleanVariable
    def aimer = self
  }
} 

// For Integers
trait LabeledIntegerVar extends IntegerVar with LabeledVar
class LabeledIntegerVariable(val targetValue:Int) extends IntegerVariable(targetValue) with LabeledIntegerVar {}

// For Doubles
trait LabeledDoubleVar extends DoubleVar with LabeledVar
class LabeledDoubleVariable(val targetValue:Double) extends DoubleVariable(targetValue) with LabeledDoubleVar

// For Reals
trait LabeledRealVar extends RealVar with LabeledVar
class LabeledRealVariable(val targetDouble:Double) extends RealVariable(targetDouble) with LabeledRealVar {
  def targetValue = new RealValue(targetDouble)
}

// For Strings
trait LabeledStringVar extends StringVar with LabeledVar
class LabeledStringVariable(val targetValue:String) extends StringVariable(targetValue) with LabeledStringVar


// Templates

class HammingTemplate[A<:LabeledVarWithTarget]()(implicit ma: Manifest[A], mt: Manifest[A#TargetType]) extends TupleTemplateWithStatistics2[A,A#TargetType]() {
  def unroll1(aimer:A) = Factor(aimer, aimer.target.asInstanceOf[A#TargetType])
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 1.0 else 0.0 // TODO
  def accuracy(variables: Iterable[A]): Double = variables.map(v => Factor(v, v.target.asInstanceOf[A#TargetType]).currentScore).sum / variables.size
}
object HammingObjective extends HammingTemplate[LabeledVarWithTarget]()(scala.reflect.ManifestFactory.classType(classOf[LabeledVarWithTarget]), scala.reflect.ManifestFactory.classType(classOf[TargetVar]))

class HammingLossTemplate[A<:LabeledVarWithTarget]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends TupleTemplateWithStatistics2[A,A#TargetType] {
  import cc.factorie.la._
  def unroll1(aimer:A) = Factor(aimer, aimer.target)
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:A#TargetType#Value) = if (value1 == value2) 0.0 else 1.0
  def accuracy(variables: Iterable[A]): Double = variables.map(v => Factor(v, v.target).currentScore).sum / variables.size
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
object HammingLoss extends HammingLossTemplate[LabeledVarWithTarget]()(scala.reflect.ManifestFactory.classType(classOf[LabeledVarWithTarget]), scala.reflect.ManifestFactory.classType(classOf[TargetVar]))


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

  def +=(label: LabeledDiscreteVar): this.type = +=(label, label.intValue)
  def +=(label: LabeledDiscreteVar, predIndex: Int): this.type = {
    require(label.domain eq domain)
    _size += 1
    val trueIndex = label.targetIntValue
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

