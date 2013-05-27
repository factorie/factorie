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
import cc.factorie.la._
import scala.util.Random

/** A single discrete variable */
trait DiscreteVar extends DiscreteDimensionTensorVar with ValueBound[DiscreteValue] {
  def domain: DiscreteDomain
  def value: DiscreteValue
  def intValue = value.intValue
  /** Return the distribution over values of this variable given some factors (which presumably neighbor this DiscreteVar)
      and given that all other variables' values are fixed. */
  def proportions(factors:Iterable[Factor]): Proportions1 = {
    val l = domain.size 
    val distribution = new DenseTensor1(l)
    val assignment = new DiscreteAssignment1(this, 0)
    var score = 0.0
    var i = 0
    while (i < l) {
      assignment.intValue1 = i
      score = 0.0; factors.foreach(f => score += f.assignmentScore(assignment))   // compute score of variable with value 'i'
      distribution(i) = score
      i += 1
    }
    distribution.expNormalize()
    new NormalizedTensorProportions1(distribution, checkNormalization=false)
  }
  /** Return the distribution over values of this variable given the model and given that all other variables' values are fixed. */
  def proportions(model:Model): Proportions1 = proportions(model.factors(this))

  /** Return the distribution over values of this variable given the model and given that all other variables' values are fixed. */
  def caseFactorProportions(model:Model): Proportions1 = {
    val l = domain.size 
    val distribution = new DenseTensor1(l)
    val assignment = new DiscreteAssignment1(this, 0)
    var i = 0
    while (i < l) {
      assignment.intValue1 = i
      distribution(i) = model.assignmentScore(this, assignment)  // compute score of variable with value 'i'
      i += 1
    }
    distribution.expNormalize()
    new NormalizedTensorProportions1(distribution, checkNormalization=false)
  }
  override def toString = printName+"("+intValue+")"
}

/** A single discrete variable whose value can be changed. */
trait MutableDiscreteVar[A<:DiscreteValue] extends DiscreteVar with MutableVar[A] with IterableSettings {
  private var __value: Int = 0
  @inline final protected def _value = __value
  @inline final protected def _set(newValue:Int): Unit = __value = newValue
  //final protected def _set(newValue:ValueType): Unit = _set(newValue.intValue)
  override def intValue = __value
  def value: A = domain.apply(__value).asInstanceOf[A] // TODO Is there a better way to coordinate A and domain?
  def tensor: Value = domain.apply(__value).asInstanceOf[Value] // TODO Consider removing this "tensor" method everywhere.  Yes! -akm
  //def set(newValue:Value)(implicit d:DiffList): Unit
  //def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.apply(newInt).asInstanceOf[Value])(d)
  @inline final def :=(i:Int): Unit = set(i)(null)
  def setRandomly(random:Random = cc.factorie.random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
  def settings: SettingIterator = new SettingIterator {
    // TODO Base this on a domain.iterator instead, for efficiency
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable: MutableDiscreteVar.this.type = MutableDiscreteVar.this
  }
  @inline final def set(newValue:Value)(implicit d:DiffList): Unit = set(newValue.intValue)(d)
  def set(newValue:Int)(implicit d:DiffList): Unit = if (newValue != __value) {
    assert(newValue < domain.size)
    if (d ne null) d += new DiscreteVariableDiff(__value, newValue)
    __value = newValue
  }

  case class DiscreteVariableDiff(oldValue: Int, newValue: Int) extends Diff {
    @inline final def variable = MutableDiscreteVar.this
    @inline final def redo = MutableDiscreteVar.this.set(newValue)(null)
    @inline final def undo = MutableDiscreteVar.this.set(oldValue)(null)
    override def toString = variable match { 
      case cv:CategoricalVar[_,_] if (oldValue >= 0) => "MutableDiscreteVarDiff("+cv.domain.category(oldValue)+"="+oldValue+","+cv.domain.category(newValue)+"="+newValue+")"
      case _ => "MutableDiscreteVarDiff("+oldValue+","+newValue+")"
    }
  }
}

// TODO What is this?  Get rid of it. -akm
trait IntMutableDiscreteVar[A<:DiscreteValue] extends MutableDiscreteVar[A] with IterableSettings {
}

/** A concrete single discrete variable whose value can be changed. */
abstract class DiscreteVariable extends IntMutableDiscreteVar[DiscreteValue]  {
  def this(initialValue:Int) = { this(); _set(initialValue) }
  def this(initialValue:DiscreteValue) = { this(); require(initialValue.dim1 == domain.size); _set(initialValue.intValue) }
}


object MaximizeDiscrete extends Maximize {
  def maxIndex(d:MutableDiscreteVar[_], model:Model): Int = {
    val origI = d.intValue
    var maxScore = Double.MinValue
    var maxI = -1
    for (i <- 0 until d.domain.size) {
      d := i // Careful!  Doesn't work if d has variable value coordination!
      val score = model.currentScore(d)
      if (score > maxScore) { maxScore = score; maxI = i }
    }
    assert(maxI != -1)
    d := origI
    maxI
  }
  def apply(d:MutableDiscreteVar[_], model:Model): Unit = d := maxIndex(d, model)
  def apply(varying:Iterable[MutableDiscreteVar[_]], model:Model): Unit = for (d <- varying) apply(d, model)
  def infer[V<:MutableDiscreteVar[_]](varying:V, model:Model): Option[DiscreteMarginal1[V]] =
    Some(new DiscreteMarginal1(varying, new SingletonProportions1(varying.domain.size, maxIndex(varying, model))))
  override def infer(variables:Iterable[Var], model:Model, summary:Summary[Marginal] = null): Option[DiscreteSummary1[DiscreteVar]] = {
    if (summary ne null) return None
    if (!variables.forall(_.isInstanceOf[MutableDiscreteVar[_]])) return None
    val result = new DiscreteSummary1[DiscreteVar]
    for (v <- variables) {
      infer(v.asInstanceOf[MutableDiscreteVar[DiscreteValue]], model) match {
        case Some(dm) => result += dm.asInstanceOf[DiscreteVar]
        case _ => return None
      }
    }
    Some(result)
  }
}

// Why can't this be accomplished merely by overriding set(Int)(DiffList)?  -akm
// I think we should just get rid of this trait. -akm
// TODO Remove this
//@deprecated("Will be removed")
//trait HookedDiscreteVariable extends DiscreteVariable { // Changed class name from "HookedVariable" because non-DiscreteVariables might also want to be hooked.
//  def valueChangeHook(old: Int, newValue: Int): Unit
//  override def set(newValue:Int)(implicit d:DiffList) {
//    valueChangeHook(_value, newValue)
//    super.set(newValue)(d)
//  }
//}
