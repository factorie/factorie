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

// Categorical variables that have target values are referred to as 'Labels'
// TODO Consider renaming all other "setting" names to "value" also

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

/** A Variable that has a desired correct "target" value, usually used for labeled training data. */
trait VarWithTargetValue extends Variable {
  def valueIsTarget: Boolean
  def setToTarget(implicit d:DiffList = null): Unit
}

/** A Variable that has a desired correct "target" value, 
    and also a "target" method returning the Variable that holds this target value. 
    This "target" variable is of type TargetVar, and should have a "aimer" method that 
    returns a pointer back to this Variable. */
trait VarWithTarget extends VarWithTargetValue with TargetType[TargetVar] {
  //type TargetType = TargetType with ValueType[this.Value]
  /** Stores the intended true "target" value for this variable. */
  def target: TargetType { type ValueType = VarWithTarget.this.ValueType }
  def valueIsTarget: Boolean = value == target.value
}

/** A trait for all variables that are containers of target values.  
    Having this trait allows ZeroOneLossTemplate to use this type as a neighbor,
    and, for example, avoid trying to unroll for all DiscreteVectorVar. */
trait TargetVar extends Variable with AimerType[Variable] {
  /** Returns the variable that "aims" to have to have its value match this variable's as its target */
  def aimer: AimerType
}


// Discrete variables with targets.

/** These variables have a target value, but it may not necessarily be stored in a separate TargetVar variable. */
trait DiscreteVarWithTargetValue extends DiscreteVariable with VarWithTargetValue {
  def targetIntValue: Int
}

/** A container of a target value for discrete variables.  */
trait DiscreteTargetVar extends DiscreteVariable with TargetVar with AimerType[DiscreteVar]

/** A discrete variable that has a true, target "labeled" value, 
    separate from its current value. 
    @author Andrew McCallum */
// TODO We could also make version of this for IntegerVar: IntegerTargetValue
// TODO Rename this DiscreteVariableWithTarget because it must include DiscreteVariable
trait DiscreteVarWithTarget extends DiscreteVarWithTargetValue with VarWithTarget with TargetType[DiscreteTargetVar] {
  //type TargetType = DiscreteValue
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  def targetIntValue: Int = if (target eq null) -1 else target.intValue
  def targetIntValue_=(newValue:Int): Unit = target.set(newValue)(null)
  def setToTarget(implicit d:DiffList): Unit = set(target.value.asInstanceOf[ValueType])
  //def valueIsTarget: Boolean = value == target.value
  def targetValue: ValueType = if (target eq null) null.asInstanceOf[ValueType] else target.value.asInstanceOf[ValueType]
  def isUnlabeled = target eq null
  //def unlabel = if (trueVariable ne null) hiddentrueIntValue = -trueIntValue - 1 else throw new Error("Already unlabeled.")
  //def relabel = if (trueIntValue < 0) trueIntValue = -(trueIntValue+1) else throw new Error("Already labeled.")
}

abstract class DiscreteVariableWithTarget(targetVal:Int) extends DiscreteVariable(targetVal) with DiscreteVarWithTarget {
  self =>
  val target = new DiscreteTarget(targetVal)
  class DiscreteTarget(targetVal:Int) extends DiscreteVariable(targetVal) with DiscreteTargetVar with AimerType[DiscreteVariableWithTarget] {
    def domain = self.domain
    def aimer = self
  }
  
}

trait CategoricalTargetVar[A] extends CategoricalVariable[A] with DiscreteTargetVar with AimerType[CategoricalVar[A]]

trait CategoricalVarWithTarget[A] extends CategoricalVariable[A] with DiscreteVarWithTarget with TargetType[CategoricalTargetVar[A]] {
  //def targetCategoryValue_=(x:A) = if (x == null) target = null else target.set(x)
  //def targetCategoryValue: CategoryType = target.categoryValue
  //def targetCategoryValue_=(newCategory:CategoryType): Unit = target.set(newCategory)(null)
}


/** A variable with a single index and a true value.
    Subclasses are allowed to override 'set' to coordinate the value of other variables with this one.
    @author Andrew McCallum
    @see LabelVariable
*/
abstract class CoordinatedLabelVariable[A](targetVal:A) extends CategoricalVariable[A](targetVal) with CategoricalVarWithTarget[A] with TargetType[CategoricalVariable[A] with CategoricalTargetVar[A]] {
  //type VariableType <: CoordinatedLabelVariable[A]
  val target = new LabelTarget(targetVal)
  class LabelTarget(targetVal:A) extends CategoricalVariable(targetVal) with CategoricalTargetVar[A] with AimerType[CoordinatedLabelVariable[A]] {
    def domain = CoordinatedLabelVariable.this.domain
    def aimer = CoordinatedLabelVariable.this
  }
}

/** A CategoricalVariable with a single value and a true value.
    Subclasses cannot override 'set' to coordinate the value of other variables with this one;
    hence belief propagation can be used with these variables.
    @author Andrew McCallum
    @see CoordinatedLabelVariable
 */
abstract class LabelVariable[T](targetVal:T) extends CoordinatedLabelVariable(targetVal) with NoVariableCoordination {
  //type VariableType <: LabelVariable[T]
  // TODO Does this next line really provide the protection we want from creating variable-value coordination?  No.  But it does catch some errors.
  override final def set(newValue: ValueType)(implicit d: DiffList) = super.set(newValue)(d)
}



// Templates

//class ZeroOneLossTemplate[A<:VarWithTarget[TargetVar[A,TargetVar[A,_]],A]]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends Template2[A,A#TargetType] with Statistics1[Boolean] 

class ZeroOneLossTemplate[A<:VarWithTarget]()(implicit am:Manifest[A], tm:Manifest[A#TargetType]) extends Template2[A,A#TargetType] with Statistics1[Boolean] {
  def unroll1(aimer:A) = Factor(aimer, aimer.target)
  def unroll2(target:A#TargetType) = throw new Error("Cannot unroll from the target variable.")
  def statistics(values:Values) = Stat(values._1 == values._2)
  def score(s:Stat) = if (s._1) 1.0 else 0.0
}

object ZeroOneLossObjective extends TemplateModel(new ZeroOneLossTemplate[VarWithTarget])

// Evaluation

/** Stores the results of evaluating per-label accuracy and other measures.
    Note, this is not per-field accuracy. */
class LabelEvaluation(val domain: CategoricalDomain[String]) {
  //val labelValue: String, var targetIndex:Int) {
  def this(labels:Iterable[LabelVariable[String]]) = { this(labels.head.domain); this ++= labels }

  private val _fp = new Array[Int](domain.size)
  private val _fn = new Array[Int](domain.size)
  private val _tp = new Array[Int](domain.size)
  private val _tn = new Array[Int](domain.size)
  private var _size: Int = 0
  def count = _size

  /*private val targetIndex = -1 // TODO replace this: Domain[L](m).index(labelValue) */

  //def ++=(tokenseqs:Seq[Seq[{def label:LabelVariable[String]}]]) = tokenseqs.foreach(ts => this += ts.map(_.label))

  def +=(label: LabelVariable[String]): this.type = {
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
  def ++=(labels: Iterable[LabelVariable[String]]): this.type = { labels.foreach(+=(_)); this }
  def +++=(labels: Iterable[Iterable[LabelVariable[String]]]): this.type = { labels.foreach(_.foreach(+=(_))); this }
  // TODO Consider removing these
  def +=(a:Attr, f:Attr=>LabelVariable[String]): this.type = this += f(a)
  def ++=(as:Iterable[Attr], f:Attr=>LabelVariable[String]): this.type = { as.foreach(this += f(_)); this }
  def +++=(as:Iterable[Iterable[Attr]], f:Attr=>LabelVariable[String]): this.type = { as.foreach(_.foreach(this += f(_))); this }
  
  def accuracy: Double = (_tp.sum + _tn.sum).toDouble / _size
  def precision(labelIndex:Int): Double = if (_tp(labelIndex) + _fp(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fp(labelIndex))
  def precision(labelValue:DiscreteValue): Double = precision(labelValue.intValue)
  def precision(category:String): Double = precision(domain.getIndex(category))
  def precision: Double = precision(0)
  def recall(labelIndex:Int): Double = if (_tp(labelIndex) + _fn(labelIndex) == 0.0) 0.0 else _tp(labelIndex).toDouble / (_tp(labelIndex) + _fn(labelIndex))
  def recall(labelValue:DiscreteValue): Double = recall(labelValue.intValue)
  def recall(category:String): Double = recall(domain.getIndex(category))
  def recall: Double = recall(0)
  def f1(labelIndex:Int): Double = if (precision(labelIndex) + recall(labelIndex) == 0.0) 0.0 else 2.0 * precision(labelIndex) * recall(labelIndex) / (precision(labelIndex) + recall(labelIndex))
  def f1(labelValue:DiscreteValue): Double = f1(labelValue.intValue)
  def f1(category:String): Double = f1(domain.getIndex(category))
  def f1: Double = f1(0)
  def tp(labelIndex:Int): Int = _tp(labelIndex)
  def tp(labelValue:DiscreteValue): Int = _tp(labelValue.intValue)
  def tp(category:String): Int = _tp(domain.getIndex(category))
  def tp: Int = _tp(0)
  def fp(labelIndex:Int): Int = _fp(labelIndex)
  def fp(labelValue:DiscreteValue): Int = _fp(labelValue.intValue)
  def fp(category:String): Int = _fp(domain.getIndex(category))
  def fp: Int = _fp(0)
  def tn(labelIndex:Int): Int = _tn(labelIndex)
  def tn(labelValue:DiscreteValue): Int = _tn(labelValue.intValue)
  def tn(category:String): Int = _tn(domain.getIndex(category))
  def tn: Int = _tn(0)
  def fn(labelIndex:Int): Int = _fn(labelIndex)
  def fn(labelValue:DiscreteValue): Int = _fn(labelValue.intValue)
  def fn(category:String): Int = _fn(domain.getIndex(category))
  def fn: Int = _fn(0)
  
  //def correctCount(labelIndex:Int) = _tp(labelIndex)
  //def missCount(labelIndex:Int) = _fn(labelIndex)
  //def alarmCount(labelIndex:Int) = _fp(labelIndex)
  def toString(labelIndex:Int) = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(domain.getCategory(labelIndex), f1(labelIndex), precision(labelIndex), recall(labelIndex), tp(labelIndex), fp(labelIndex), fn(labelIndex), tp(labelIndex)+fn(labelIndex), tp(labelIndex)+fp(labelIndex))
  override def toString = (0 until domain.size).map(toString(_)).mkString("\n")
}

