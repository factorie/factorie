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

package cc.factorie.generative
import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable.{HashSet,ArrayBuffer}

class DiscreteTemplate extends GenerativeTemplateWithStatistics2[GeneratedDiscreteVar,Proportions] {
  //class Factor(d:Discrete, p:Proportions) extends super.Factor(d, p) with GenerativeFactor
  def unroll1(d:GeneratedDiscreteVar) = Factor(d, d.proportions)
  def unroll2(p:Proportions) = for (d <- p.childrenOfClass[Discrete]) yield Factor(d, p)
  def logpr(s:Stat) = math.log(pr(s))
  def pr(s:Stat): Double = pr(s._1, s._2)
  def pr(d:DiscreteValue, p:IndexedSeq[Double]): Double = p(d.intValue)
  //def sampledValue(s:Stat) = s._1.domain(maths.nextDiscrete(s._2))
  def sampledValue(s:Stat): DiscreteValue = sampledValue(s._1.domain, s._2)
  def sampledIntValue(s:Stat): Int = s._2.sampleInt
  def sampledValue(d:DiscreteDomain, p:ProportionsValue): DiscreteValue = d(p.sampleInt)
  def sampledIntValue(d:DiscreteDomain, p:ProportionsValue): DiscreteValue = d(p.sampleInt)
}
object DiscreteTemplate extends DiscreteTemplate

trait GeneratedDiscreteVar extends GeneratedVar with DiscreteVar {
  private var _proportions: Proportions = null
  def proportions: Proportions = _proportions
  def setProportions(p:Proportions): Unit = {
    if (_proportions ne null) _proportions.removeChild(this)(null)
    _proportions = p
    _proportions.addChild(this)(null)
  }
  val generativeTemplate = DiscreteTemplate
  def generativeFactor = new DiscreteTemplate.Factor(this, proportions)
  override def parents = Seq(proportions)
  override def pr = proportions(this.intValue)
}

abstract class Discrete(proportions:Proportions, initialValue: Int = 0) extends DiscreteVariable(initialValue) with GeneratedDiscreteVar with MutableGeneratedVar {
  setProportions(proportions)
  def maximize(implicit d:DiffList): Unit = set(proportions.maxPrIndex)
}

/*class Binomial(p:RealVarParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/

trait GeneratedCategoricalVar[A] extends GeneratedDiscreteVar with CategoricalVar[A]
//trait GeneratedCategoricalVariable[A] extends CategoricalVariable[A] with GeneratedDiscreteVariable with GeneratedCategoricalVar[A]

abstract class Categorical[A](proportions:Proportions, initialValue:A) extends CategoricalVariable(initialValue) with GeneratedCategoricalVar[A] with MutableGeneratedVar {
  setProportions(proportions)
  def maximize(implicit d:DiffList): Unit = set(proportions.maxPrIndex)
}



/** Appears as a single GeneratedDiscreteVar, but actually contains a compact array, each accessible by changing 'mux' from 0 to muxSize-1. */
abstract class DiscreteMux(proportions:Proportions, initialIndices:Seq[Int]) extends GeneratedDiscreteVar with MutableGeneratedVar {
  setProportions(proportions)
  var mux: Int = 0
  def muxSize = _values.length
  private val _values = initialIndices.map(i => domain.getValue(i)).toArray
  def value = _values(mux)
  def set(newValue:ValueType)(implicit d:DiffList): Unit = {
    require (d eq null) // DiffList not yet implemented for this change
    _values(mux) = newValue
  }
  def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.getValue(newInt))(d)
}

abstract class CategoricalMux[A](proportions:Proportions, initialValues:Seq[CategoricalValue[A]]) 
extends DiscreteMux(proportions, initialValues.map(_.intValue)) with GeneratedCategoricalVar[A]
