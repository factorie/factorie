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
import scala.collection.mutable.ArrayBuffer

/** A single discrete variable */
trait DiscreteVar extends DiscreteTensorVar with VarAndValueType[DiscreteVar,DiscreteValue] {
  def domain: DiscreteDomain
  def intValue = value.intValue
  override def toString = printName+"("+intValue+")"
}

/** A single discrete variable whose value can be changed. */
trait MutableDiscreteVar extends DiscreteVar with MutableVar {
  def set(newValue:Value)(implicit d:DiffList): Unit
  def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.apply(newInt).asInstanceOf[ValueType])(d)
  @inline final def :=(i:Int): Unit = set(i)(null)
  def setRandomly(random:Random = cc.factorie.random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
}


/** A concrete single discrete variable whose value can be changed. */
abstract class DiscreteVariable extends MutableDiscreteVar with IterableSettings {
  def this(initialValue:Int) = { this(); __value = initialValue }
  def this(initialValue:DiscreteValue) = { this(); require(initialValue.domain == domain); _set(initialValue.intValue) }
  private var __value: Int = 0
  protected def _value = __value
  protected def _set(newValue:Int): Unit = __value = newValue
  //final protected def _set(newValue:ValueType): Unit = _set(newValue.intValue)
  override def intValue = __value
  def value: Value = domain.apply(__value).asInstanceOf[Value]
  def tensor: Value = domain.apply(__value).asInstanceOf[Value]
  @inline final def set(newValue:ValueType)(implicit d:DiffList): Unit = set(newValue.intValue)(d)
  override def set(newValue:Int)(implicit d:DiffList): Unit = if (newValue != __value) {
    assert(newValue < domain.size)
    if (d ne null) d += new DiscreteVariableDiff(__value, newValue)
    __value = newValue
  }
  def settings = new SettingIterator {
    // TODO Base this on a domain.iterator instead, for efficiency
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable: DiscreteVariable.this.type = DiscreteVariable.this
  }
  /** Return the distribution over values of this variable given the model and given that all other variables' values are fixed. */
  def proportions(model:Model): Proportions1 = {
    val origIntValue = intValue
    val l = domain.size 
    val distribution = new DenseTensor1(l)
    var i = 0
    while (i < l) {
      //model.factors(Seq(this)).sumBy(_.values.set(this, i).score) // a version that doesn't change the value of this variable
      __value = i
      distribution(i) = model.score(this)  // compute score of variable with value 'i'
      i += 1
    }
    distribution.expNormalize()
    __value = origIntValue
    new DenseProportions1(distribution)
  }

  case class DiscreteVariableDiff(oldValue: Int, newValue: Int) extends Diff {
    @inline final def variable: DiscreteVariable = DiscreteVariable.this
    @inline final def redo = DiscreteVariable.this.set(newValue)(null)
    @inline final def undo = DiscreteVariable.this.set(oldValue)(null)
    override def toString = variable match { 
      case cv:CategoricalVar[_] if (oldValue >= 0) => "DiscreteVariableDiff("+cv.domain.category(oldValue)+"="+oldValue+","+cv.domain.category(newValue)+"="+newValue+")"
      case _ => "DiscreteVariableDiff("+oldValue+","+newValue+")"
    }
  }
}


object MaximizeDiscrete extends Maximize {
  def maxIndex(d:DiscreteVariable, model:Model): Int = {
    val origI = d.intValue
    var maxScore = Double.MinValue
    var maxI = -1
    for (i <- 0 until d.domain.size) {
      d := i // Careful!  Doesn't work if d has variable value coordination!
      val score = model.score(d)
      if (score > maxScore) { maxScore = score; maxI = i }
    }
    assert(maxI != -1)
    d := origI
    maxI
  }
  def apply(d:DiscreteVariable, model:Model): Unit = d := maxIndex(d, model)
  def apply(varying:Iterable[DiscreteVariable], model:Model): Unit = for (d <- varying) apply(d, model)
  def infer[V<:DiscreteVariable](varying:V, model:Model): Option[DiscreteMarginal1[V]] =
    Some(new DiscreteMarginal1(varying, new SingletonProportions1(varying.domain.size, maxIndex(varying, model))))
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[DiscreteSummary1[DiscreteVariable]] = {
    if (summary ne null) return None
    if (!variables.forall(_.isInstanceOf[DiscreteVariable])) return None
    val result = new DiscreteSummary1[DiscreteVariable]
    for (v <- variables) {
      infer(v.asInstanceOf[DiscreteVariable], model) match {
        case Some(dm) => result += dm
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
