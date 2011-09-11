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

trait PlatedGeneratedDiscreteVar extends DiscreteSeqVariable with MutableGeneratedVar
abstract class PlatedDiscrete(initialValue:Seq[Int]) extends DiscreteSeqVariable(initialValue) with PlatedGeneratedDiscreteVar

trait PlatedGeneratedCategoricalVar[A] extends CategoricalSeqVariable[A] with PlatedGeneratedDiscreteVar
abstract class PlatedCategorical[A](initialValue:Seq[A]) extends CategoricalSeqVariable(initialValue) with PlatedGeneratedCategoricalVar[A]

trait PlatedDiscreteGeneratingFactor extends GenerativeFactor {
  def prValue(s:StatisticsType, value:Int, index:Int): Double
  def prValue(value:Int, index:Int): Double = prValue(statistics, value, index)
}

object PlatedDiscrete extends GenerativeFamily2[PlatedGeneratedDiscreteVar,Proportions] {
  self =>
  def pr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def logpr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => math.log(p(dv.intValue))).sum
  def sampledValue(d:DiscreteDomain, length:Int, p:ProportionsValue): Seq[DiscreteValue] = 
    ArrayBuffer.fill(length)(d.getValue(p.sampleInt))
  case class Factor(_1:PlatedGeneratedDiscreteVar, _2:Proportions) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2)
    override def logpr(s:Statistics): Double = self.logpr(s._1, s._2)
    def sampledValue(s:Statistics): Seq[DiscreteValue] = self.sampledValue(s._1.first.domain, s._1.length, s._2)
    def updateCollapsedParents(index:Int, weight:Double): Unit = {
      _2 match {
        case p:DenseCountsProportions => { p.increment(_1(index).intValue, weight)(null); true }
        case _ => false
      }
    }
  }
  def newFactor(a:PlatedGeneratedDiscreteVar, b:Proportions) = Factor(a, b)
}
