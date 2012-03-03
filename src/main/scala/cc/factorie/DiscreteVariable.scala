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
trait DiscreteVar extends DiscreteVectorVar with VarAndValueType[DiscreteVar,DiscreteValue] {
  def domain: DiscreteDomain
  /*@inline final*/ def intValue = value.intValue
  //def activeDomain = List(intValue) // TODO try to make this implementation possible: = value
}

trait MutableDiscreteVar extends DiscreteVar with MutableVar {
  def set(newValue:ValueType)(implicit d:DiffList): Unit
  def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.getValue(newInt))(d)
  def setRandomly(random:Random = cc.factorie.random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
}


// TODO Consider the following
abstract class DiscreteVariable extends VectorVar with MutableDiscreteVar with IterableSettings {
  def this(initialValue:Int) = { this(); _value = initialValue }
  def this(initialValue:DiscreteValue) = { this(); require(initialValue.domain == domain); _value = initialValue.intValue }
  private var _value: Int = 0
  override def intValue = _value
  def value: Value = domain.getValue(_value)
  def set(newValue:ValueType)(implicit d:DiffList): Unit = if (newValue.intValue != _value) {
    assert((newValue eq null) || newValue.domain == domain)
    if (d ne null) d += new DiscreteVariableDiff(_value, newValue.intValue)
    _value = newValue.intValue
  }
  override def set(newValue:Int)(implicit d:DiffList): Unit = if (newValue != _value) {
    assert(newValue < domain.size)
    if (d ne null) d += new DiscreteVariableDiff(_value, newValue)
    _value = newValue
  }
  @inline protected final def _set(newValue:ValueType): Unit = _value = newValue.intValue
  def settings = new SettingIterator {
    // TODO Base this on a domain.iterator instead, for efficiency
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable: DiscreteVariable.this.type = DiscreteVariable.this
  }
  // TODO Keep this?  Or replace later by appropriate methods somewhere among the "Inferencer"s?
  def proportions(model:Model): cc.factorie.generative.Proportions = {
    val origIntValue = intValue
    val distribution = new Array[Double](domain.size)
    forIndex(distribution.length)(i => {
      //model.factors(Seq(this)).sumBy(_.values.set(this, i).score) // a version that doesn't change the value of this variable
      this.set(i)(null)
      // compute score of variable with value 'i'
      distribution(i) = model.score(Seq(this))
    })
    maths.expNormalize(distribution)
    set(origIntValue)(null)
    new cc.factorie.generative.DenseProportions(distribution)
  }

  case class DiscreteVariableDiff(oldValue: Int, newValue: Int) extends Diff {
    @inline final def variable: DiscreteVariable = DiscreteVariable.this
    @inline final def redo = _value = newValue
    @inline final def undo = _value = oldValue
    override def toString = variable match { 
      case cv:CategoricalVar[_] if (oldValue >= 0) => "DiscreteVariableDiff("+cv.domain.getCategory(oldValue)+"="+oldValue+","+cv.domain.getCategory(newValue)+"="+newValue+")"
      case _ => "DiscreteVariableDiff("+oldValue+","+newValue+")"
    }
  }
}

