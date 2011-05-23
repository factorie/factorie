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
import scala.collection.mutable.ArrayBuffer


class DiscreteSeqTemplate extends GenerativeTemplateWithStatistics2[GeneratedDiscreteSeqVar,Proportions] {
  def unroll1(d:GeneratedDiscreteSeqVar) = Factor(d, d.proportions)
  def unroll2(p:Proportions) = for (d <- p.childrenOfClass[GeneratedDiscreteSeqVar]) yield Factor(d, p)
  def logpr(s:Stat) = math.log(pr(s))
  def pr(s:Stat): Double = pr(s._1, s._2)
  def pr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def logpr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => math.log(p(dv.intValue))).sum
  def sampledValue(s:Stat): Seq[DiscreteValue] = sampledValue(s._1.first.domain, s._1.length, s._2)
  def sampledValue(d:DiscreteDomain, length:Int, p:ProportionsValue): Seq[DiscreteValue] = 
    ArrayBuffer.fill(length)(d.getValue(p.sampleInt))
  //def sampledIntValue(s:Stat): Int = s._2.sampleInt
  //def sampledIntValue(d:DiscreteSeqDomain, p:ProportionsValue): DiscreteValue = d(p.sampleInt)
}
object DiscreteSeqTemplate extends DiscreteSeqTemplate

trait GeneratedDiscreteSeqVar extends DiscreteSeqVariable with MutableGeneratedVar {
  val generativeTemplate = DiscreteSeqTemplate
  def generativeFactor = new DiscreteSeqTemplate.Factor(this, proportions)
  private var _proportions: Proportions = null
  def proportions: Proportions = _proportions
  def setProportions(p:Proportions): Unit = {
    if (_proportions ne null) _proportions.removeChild(this)(null)
    _proportions = p
    _proportions.addChild(this)(null)
  }
  override def parents = Seq(proportions)
  //override def pr = proportions(this.intValue)
}

abstract class DiscreteSeq(proportions:Proportions, initialValue:Seq[Int]) extends DiscreteSeqVariable(initialValue) with GeneratedDiscreteSeqVar {
  setProportions(proportions)
}

trait GeneratedCategoricalSeqVar[A] extends CategoricalSeqVariable[A] with GeneratedDiscreteSeqVar

abstract class CategoricalSeq[A](proportions:Proportions, initialValue:Seq[A]) extends CategoricalSeqVariable(initialValue) with GeneratedCategoricalSeqVar[A] {
  setProportions(proportions)
}
