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

import cc.factorie.la._
import scala.util.Random
import cc.factorie.model._
import cc.factorie.infer.{Maximize, DiscreteSummary1, SimpleDiscreteMarginal1, Summary}

/** A single discrete variable */
trait DiscreteVar extends VectorVar with VarWithDomain {
  type Value <: DiscreteValue
  def domain: DiscreteDomain
  def value: Value
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
    new DenseTensorProportions1(distribution.asArray, checkNormalization=false)
  }
  /** Return the distribution over values of this variable given the model and given that all other variables' values are fixed. */
  def proportions(model:Model): Proportions1 = proportions(model.factors(this))
  
// TODO Not sure this really belongs here.  Similar functionality in MaximizeDiscrete. -akm
//  /** Return the integer value of this variable would maximize the model's score, given that all other variables' values are fixed.
//      The current value of this variable remains unchanged. */
//  def maximizingIntValue(factors:Iterable[Factor]): Int = {
//    val l = domain.size 
//    val assignment = new DiscreteAssignment1(this, 0)
//    var score = 0.0
//    var maxScore = Double.NegativeInfinity
//    var maxIntValue = -1
//    var i = 0
//    while (i < l) {
//      assignment.intValue1 = i
//      score = 0.0; factors.foreach(f => score += f.assignmentScore(assignment))   // compute score of variable with value 'i'
//      if (score > maxScore) { maxScore = score; maxIntValue = i }
//      i += 1
//    }
//    maxIntValue
//  }
//  /** Return the integer value of this variable would maximize the model's score, given that all other variables' values are fixed.
//      The current value of this variable remains unchanged. */
//  def maximizingIntValue(model:Model): Int = maximizingIntValue(model.factors(this))

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
    new DenseTensorProportions1(distribution.asArray, checkNormalization=false)
  }
  override def toString = printName+"("+intValue+")"
}

/** A single discrete variable whose value can be changed. */
trait MutableDiscreteVar extends DiscreteVar with MutableVar with IterableSettings with VarWithDomain {
  self =>
  private var __value: Int = 0
  def domain: DiscreteDomain
  @inline final protected def _value = __value
  @inline final protected def _set(newValue:Int): Unit = __value = newValue
  //final protected def _set(newValue:ValueType): Unit = _set(newValue.intValue)
  override def intValue = __value
  def value: Value = domain.apply(__value).asInstanceOf[Value] // TODO Is there a better way to coordinate A and domain?
  //def set(newValue:Value)(implicit d:DiffList): Unit
  //def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.apply(newInt).asInstanceOf[Value])(d)
  @inline final def :=(i:Int): Unit = set(i)(null)
  def setRandomly(implicit random:Random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
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
    @inline final def redo() = MutableDiscreteVar.this.set(newValue)(null)
    @inline final def undo() = MutableDiscreteVar.this.set(oldValue)(null)
    override def toString = variable match { 
      case cv:CategoricalVar[_] if oldValue >= 0 => "MutableDiscreteVarDiff("+cv.domain.category(oldValue)+"="+oldValue+","+cv.domain.category(newValue)+"="+newValue+")"
      case _ => "MutableDiscreteVarDiff("+oldValue+","+newValue+")"
    }
  }
}

// TODO What is this?  Get rid of it. -akm
trait IntMutableDiscreteVar[A<:DiscreteValue] extends MutableDiscreteVar with IterableSettings {
}

/** A concrete single discrete variable whose value can be changed. */
abstract class DiscreteVariable extends IntMutableDiscreteVar[DiscreteValue] {
  self =>
  type Value = DiscreteValue
  def domain: DiscreteDomain
  def this(initialValue:Int) = { this(); _set(initialValue) }
  def this(initialValue:DiscreteValue) = { this(); require(initialValue.dim1 == domain.size); _set(initialValue.intValue) }
}


object MaximizeDiscrete extends Maximize[Iterable[MutableDiscreteVar],Model] {
  def intValue(d:DiscreteVar, factors:Iterable[Factor]): Int = {
    val l = d.domain.size 
    val assignment = new DiscreteAssignment1(d, 0)
    var score = 0.0
    var maxScore = Double.NegativeInfinity
    var maxIntValue = -1
    var i = 0
    while (i < l) {
      assignment.intValue1 = i
      score = 0.0; factors.foreach(f => score += f.assignmentScore(assignment))   // compute score of variable with value 'i'
      if (score > maxScore) { maxScore = score; maxIntValue = i }
      i += 1
    }
    maxIntValue
  }
  def intValue(d:DiscreteVar, model:Model): Int = intValue(d, model.factors(d))
  def caseFactorIntValue(d:MutableDiscreteVar, model:Model): Int = MaximizeDiscrete.intValue(d, model)
  def apply(d:MutableDiscreteVar, model:Model): Unit = d := intValue(d, model)
  def apply(varying:Iterable[MutableDiscreteVar], model:Model): Unit = for (d <- varying) apply(d, model)
  def infer(varying:DiscreteVar, model:Model) =
    new SimpleDiscreteMarginal1(varying, new SingletonProportions1(varying.domain.size, intValue(varying, model)))
  def infer(variables:Iterable[MutableDiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    val result = new DiscreteSummary1[DiscreteVar]
    for (v <- variables) result += infer(v, model)
    result
  }
}
