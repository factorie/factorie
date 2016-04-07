/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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

import cc.factorie._
import cc.factorie.model.TupleTemplateWithStatistics2
import cc.factorie.util.Attr

import scala.reflect._

// Naming explanation:
// Variables in "aimer/target" pairs, used for labeled data for training.
// The "target" is a container for the true, correct value.
// The "aimer" is the variable that is supposed to have that correct value; it is "aiming" to have the correct value, 
//  but may have some other value temporarily. 
// In other words:
// The "aimer" is a variable that should have the target value.  
// The "target" is the container for that target value, which also has a pointer back to its aimer.

// Further naming conventions:
// A "LabeledCategoricalVar" variable is an "aimer"
// A "CategoricalLabeling" is a trait that must be mixed with a CategoricalVariable that defines everything needed to make it "Labeled",
//  (unlike "LabeledCategoricalVar" which still leaves some members abstract so that LabeledBooleanVariable can inherit from it).
// A "Label" however could simply be a categorical variable associated with some other object, like a "SpanLabel(val span:Span) extends CategoricalVariable[String]".

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
//trait LabeledVar extends Var {
//  def targetValue: Value
//  def valueIsTarget: Boolean = value == targetValue
//}

/** An abstract variable that has a desired correct target value,
    and also a "target" method returning the variable that holds this target value. 
    This "target" variable is of type TargetVar, and should have a "aimer" method that 
    returns a pointer back to this Variable.
    @author Andrew McCallum */
trait LabeledVar extends Var {
  self =>
  type TargetType <: TargetVar
  /** Stores the intended true "target" value for this variable. */
  def target: TargetType { type Value = self.Value }
  def valueIsTarget: Boolean = value == target.value
  //def targetValue = target.value.asInstanceOf[Value]
}

/** An abstract mutable variable that has a desired correct "target" value.
    @author Andrew McCallum  */
trait LabeledMutableVar extends MutableVar with LabeledVar {
  def setToTarget(implicit d:DiffList = null): Unit = set(target.value)
}

/** An abstract mutable variable that has a desired correct target value,
    and also a "target" method returning the variable that holds this target value. 
    @author Andrew McCallum */
//trait LabeledMutableVar extends LabeledVar with LabeledMutableVar



// Discrete variables with targets.

/** An abstract container of a target value for discrete variables.
    @author Andrew McCallum  */
trait DiscreteTargetVar extends MutableDiscreteVar with TargetVar {
  type AimerType <: LabeledDiscreteVar
}

/** A concrete container of a target value for discrete variables.
    @author Andrew McCallum  */
class DiscreteTargetVariable(val aimer:LabeledDiscreteVar) extends DiscreteVariable with DiscreteTargetVar {
  type AimerType = LabeledDiscreteVar
  def this(targetInt:Int, aimer:LabeledMutableDiscreteVar) = { this(aimer); _initialize(targetInt) }
  def domain = aimer.domain
}

/** An abstract discrete variable that stores a desired correct "target" value separate from its current value. 
    @author Andrew McCallum */
trait LabeledDiscreteVar extends DiscreteVar with LabeledVar {
  type TargetType <: DiscreteTargetVar
  override def valueIsTarget: Boolean = target.intValue == intValue
}

/** An abstract mutable discrete variable that stores a desired correct "target" value separate from its current value. 
    @author Andrew McCallum */
trait LabeledMutableDiscreteVar extends LabeledDiscreteVar with MutableDiscreteVar with LabeledMutableVar {
  type TargetType <: DiscreteTargetVar with MutableDiscreteVar
  override def setToTarget(implicit d:DiffList = null): Unit = set(target.intValue)
  // Support for "temporarily" unlabeled data.  Unlabeled is represented by negative intValues.  (This is the only reason TargetType is declared to be mutable above.)
  def isUnlabeled = target.intValue < 0
  def unlabel() = if (target.intValue >= 0) target := -target.intValue - 1 else throw new Error("Already unlabeled.")
  def relabel() = if (target.intValue < 0) target := -(target.intValue+1) else throw new Error("Already labeled.")
}

/** A trait that is mixed into a DiscreteVariable, which provides it with everything it needs to also be a LabeledMutableDiscreteVar.
    The target value will be value to which this variable is initialized.
    @author Andrew McCallum */
trait DiscreteLabeling extends DiscreteVariable with LabeledMutableDiscreteVar {
  type TargetType = DiscreteTargetVariable
  val target = new DiscreteTargetVariable(intValue, this)
}

/** A mutable discrete variable that stores a desired correct "target" value separate from its current value.
    The only abstract method is "domain".    
    @author Andrew McCallum */
abstract class LabeledDiscreteVariable(targetIntValue:Int) extends DiscreteVariable(targetIntValue) with DiscreteLabeling


// Categorical variables with targets

/** An abstract container of a target value for categorical variables.
    @author Andrew McCallum  */
trait CategoricalTargetVar[C] extends MutableCategoricalVar[C] with DiscreteTargetVar  {
  type AimerType <: LabeledCategoricalVar[C]
}

/** A concrete container of a target value for categorical variables.
    @author Andrew McCallum  */
class CategoricalTargetVariable[C](val aimer:LabeledCategoricalVar[C]) extends CategoricalVariable[C] with CategoricalTargetVar[C] {
  type AimerType = LabeledCategoricalVar[C]
  def this(targetCategory:C, aimer:LabeledMutableCategoricalVar[C]) = { this(aimer); _initialize(domain.index(targetCategory)) }
  def this(targetInt:Int, aimer:LabeledMutableCategoricalVar[C]) = { this(aimer); _initialize(targetInt) }
  def domain = aimer.domain
}

/** An abstract categorical variable that stores a desired correct "target" value separate from its current value. 
    @author Andrew McCallum */
trait LabeledCategoricalVar[C] extends CategoricalVar[C] with LabeledDiscreteVar {
  type TargetType <: CategoricalTargetVar[C]
}

/** An abstract mutable categorical variable that has a desired correct "target" value.
    This variable has a target value, but it may not necessarily be stored in a separate TargetVar variable.
    @author Andrew McCallum */
trait LabeledMutableCategoricalVar[C] extends MutableCategoricalVar[C] with LabeledMutableDiscreteVar with LabeledCategoricalVar[C] {
  type TargetType <: CategoricalTargetVar[C] with MutableCategoricalVar[C]
}

/** A trait that is mixed into a CategoricalVariable, which provides it with everything it needs to also be a LabeledMutableCategoricalVar.
    The target value will be value to which this variable is initialized.
    @author Andrew McCallum */
trait CategoricalLabeling[C] extends CategoricalVariable[C] with LabeledMutableCategoricalVar[C] {
  type TargetType = CategoricalTargetVariable[C]
  val target = new CategoricalTargetVariable[C](intValue, this)
}

/** A mutable categorical variable that has a true, target "labeled" value, separate from its current value.
    The only abstract method is "domain".    
    @author Andrew McCallum */
abstract class LabeledCategoricalVariable[C](targetCategory:C) extends CategoricalVariable[C](targetCategory) with CategoricalLabeling[C]


// For Booleans

/** An abstract container of a target value for categorical variables.
    @author Andrew McCallum  */
trait BooleanTargetVar extends MutableBooleanVar with CategoricalTargetVar[Boolean] {
  type AimerType <: LabeledBooleanVar
}

/** A concrete container of a target value for categorical variables.
    @author Andrew McCallum  */
class BooleanTargetVariable(targetCategory:Boolean, val aimer:LabeledBooleanVar) extends BooleanVariable(targetCategory) with BooleanTargetVar {
  type AimerType = LabeledBooleanVar
}

trait LabeledBooleanVar extends LabeledCategoricalVar[Boolean] with BooleanVar

trait LabeledMutableBooleanVar extends MutableBooleanVar with LabeledBooleanVar with LabeledMutableCategoricalVar[Boolean]

/** A trait that is mixed into a BooleanVariable, which provides it with everything it needs to also be a LabeledMutableBooleanVar.
    The target value will be value to which this variable is initialized.
    @author Andrew McCallum */
trait BooleanLabeling extends BooleanVariable with LabeledMutableBooleanVar {
  type TargetType = BooleanTargetVariable
  val target = new BooleanTargetVariable(booleanValue, this)
}

/** A concrete mutable boolean variable that has a true, target "labeled" value, separate from its current value.
    @author Andrew McCallum */
abstract class LabeledBooleanVariable(targetCategory:Boolean) extends BooleanVariable(targetCategory) with BooleanLabeling


// For Integers
class IntegerTargetVariable(intValue:Int, val aimer:LabeledIntegerVar) extends IntegerVariable(intValue) with TargetVar { type AimerType = LabeledIntegerVar }
trait LabeledIntegerVar extends IntegerVar with LabeledVar
class LabeledIntegerVariable(targetValue:Int) extends IntegerVariable(targetValue) with LabeledIntegerVar {
  type TargetType = IntegerTargetVariable
  val target = new IntegerTargetVariable(targetValue, this)
}

// For Doubles
class DoubleTargetVariable(doubleValue:Double, val aimer:LabeledDoubleVar) extends DoubleVariable(doubleValue) with TargetVar { type AimerType = LabeledDoubleVar }
trait LabeledDoubleVar extends DoubleVar with LabeledVar
class LabeledDoubleVariable(targetValue:Double) extends DoubleVariable(targetValue) with LabeledDoubleVar {
  type TargetType = DoubleTargetVariable
  val target = new DoubleTargetVariable(targetValue, this)
}

// For Reals
class RealTargetVariable(doubleValue:Double, val aimer:LabeledRealVar) extends RealVariable(doubleValue) with TargetVar { type AimerType = LabeledRealVar }
trait LabeledRealVar extends RealVar with LabeledVar
class LabeledRealVariable(targetDouble:Double) extends RealVariable(targetDouble) with LabeledRealVar {
  type TargetType = RealTargetVariable
  val target = new RealTargetVariable(targetDouble, this)
}

// For Strings
class StringTargetVariable(stringValue:String, val aimer:LabeledStringVar) extends StringVariable(stringValue) with TargetVar { type AimerType = LabeledStringVar }
trait LabeledStringVar extends StringVar with LabeledVar
class LabeledStringVariable(targetValue:String) extends StringVariable(targetValue) with LabeledStringVar {
  type TargetType = StringTargetVariable
  val target = new StringTargetVariable(targetValue, this)
}


// Templates

/** A source of factors whose score is the Hamming objective, with score 1.0 for each variable whose current global value is its target value, and 0 for all other variables.
    @author Andrew McCallum */
class HammingTemplate[A<:LabeledVar:ClassTag, T <: TargetVar:ClassTag]() extends TupleTemplateWithStatistics2[A,T]() {
  def unroll1(aimer:A) = Factor(aimer, aimer.target.asInstanceOf[T])
  def unroll2(target:T) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:T#Value) = if (value1 == value2) 1.0 else 0.0 // TODO
  def accuracy(variables: Iterable[A]): Double = variables.map(v => Factor(v, v.target.asInstanceOf[T]).currentScore).sum / variables.size
}
/** A source of factors whose score is the Hamming objective, with score 1.0 for each variable whose current global value is its target value, and 0 for all other variables.
    @author Andrew McCallum */
object HammingObjective extends HammingTemplate[LabeledVar, TargetVar]()

/** A source of factors whose score is the Hamming loss, with score 0.0 for each variable whose current global value is its target value, and 1.0 for all other variables.
    @author Alexandre Passos */
class HammingLossTemplate[A<:LabeledVar:ClassTag, T <: TargetVar:ClassTag]() extends TupleTemplateWithStatistics2[A,T] {
  import cc.factorie.la._
  def unroll1(aimer:A) = Factor(aimer, aimer.target.asInstanceOf[T])
  def unroll2(target:T) = throw new Error("Cannot unroll from the target variable.")
  def score(value1:A#Value, value2:T#Value) = if (value1 == value2) 0.0 else 1.0
  def accuracy(variables: Iterable[A]): Double = variables.map(v => Factor(v, v.target.asInstanceOf[T]).currentScore).sum / variables.size
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
/** A source of factors whose score is the Hamming loss, with score 0.0 for each variable whose current global value is its target value, and 1.0 for all other variables.
    @author Alexandre Passos */
object HammingLoss extends HammingLossTemplate[LabeledVar, TargetVar]()


// Evaluation

/** Stores the results of evaluating per-label accuracy and other measures,
    providing accuracy and per-label-value precision, recall and F1.
    Note, this is not per-field accuracy.
    @author Andrew McCallum */
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
    val trueIndex = label.target.intValue
    for (targetIndex <- domain.indices) {
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
