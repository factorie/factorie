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

// Move this out of generative
trait PlatedDiscreteVar extends DiscreteSeqVariable with MutableVar
abstract class PlatedDiscreteVariable(initialValue:Seq[Int]) extends DiscreteSeqVariable(initialValue) with PlatedDiscreteVar

trait PlatedCategoricalVar[A] extends CategoricalSeqVariable[A] with PlatedDiscreteVar
abstract class PlatedCategoricalVariable[A](initialValue:Seq[A]) extends CategoricalSeqVariable(initialValue) with PlatedCategoricalVar[A]

trait PlatedDiscreteGeneratingFactor extends GenerativeFactor {
  def prValue(s:StatisticsType, value:Int, index:Int): Double
  def prValue(value:Int, index:Int): Double = prValue(statistics, value, index)
}

object PlatedDiscrete extends GenerativeFamily2[PlatedDiscreteVar,Proportions] {
  self =>
  def pr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def logpr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => math.log(p(dv.intValue))).sum
  def sampledValue(d:DiscreteDomain, length:Int, p:ProportionsValue): Seq[DiscreteValue] = 
    Vector.fill(length)(d.getValue(p.sampleInt))
  case class Factor(_1:PlatedDiscreteVar, _2:Proportions) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2)
    override def logpr(s:Statistics): Double = self.logpr(s._1, s._2)
    override def sampledValue: Any = self.sampledValue(_1.first.domain, _1.length, _2) // Avoid creating a Statistics
    def sampledValue(s:Statistics): Seq[DiscreteValue] = {
      if (s._1.length == 0) Nil
      else self.sampledValue(s._1.first.domain, s._1.length, s._2)
    }
    def updateCollapsedParents(index:Int, weight:Double): Unit = {
      _2 match {
        case p:DenseCountsProportions => { p.increment(_1(index).intValue, weight)(null); true }
        case _ => false
      }
    }
  }
  def newFactor(a:PlatedDiscreteVar, b:Proportions) = Factor(a, b)
}
