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

package cc.factorie.directed

import cc.factorie.variable._

/*
trait PlatedDiscreteGeneratingFactor extends DirectedFactor {
  def prValue(s:StatisticsType, value:Int, index:Int): Double
  def prValue(value:Int, index:Int): Double = prValue(statistics, value, index)
}
*/

object PlatedDiscrete extends DirectedFamily2[DiscreteSeqVariable,ProportionsVariable] {
  self =>
  //def pr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def pr(ds:DiscreteSeqVariable#Value, p:Proportions): Double = ds.map(dv => p(dv.intValue)).product // TODO Make this more efficient; this current boxes
  def logpr(ds:IndexedSeq[DiscreteValue], p:Proportions): Double = ds.map(dv => math.log(p(dv.intValue))).sum // TODO Make this more efficient
  def sampledValue(d:DiscreteDomain, length:Int, p:Proportions)(implicit random: scala.util.Random): IndexedSeq[DiscreteValue] =
    Vector.fill(length)(d.apply(p.sampleIndex))
  case class Factor(override val _1:DiscreteSeqVariable, override val _2:ProportionsVariable) extends super.Factor(_1, _2) {
    def pr(child:DiscreteSeqVariable#Value, p:Proportions): Double = self.pr(child, p)
    //override def logpr(s:Statistics): Double = self.logpr(s._1, s._2)
    override def sampledValue(implicit random: scala.util.Random): IndexedSeq[DiscreteValue] = self.sampledValue(_1.domain.elementDomain, _1.length, _2.value) // Avoid creating a Statistics
    def sampledValue(p:Proportions)(implicit random: scala.util.Random): IndexedSeq[DiscreteValue] = {
      if (_1.length == 0) IndexedSeq[DiscreteValue]()
      else self.sampledValue(_1.domain.elementDomain, _1.length, p)
    }
    def updateCollapsedParents(index:Int, weight:Double): Boolean = { _2.value.+=(_1(index).intValue, weight); true }
  }
  def newFactor(a:DiscreteSeqVariable, b:ProportionsVariable) = Factor(a, b)
}

object PlatedCategorical extends DirectedFamily2[CategoricalSeqVariable[String],ProportionsVariable] {
  self =>
  //def pr(ds:Seq[CategoricalValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def pr(ds:IndexedSeq[CategoricalValue[String]], p:Proportions): Double = ds.map(dv => p(dv.intValue)).product // TODO Make this more efficient; this current boxes
  def logpr(ds:IndexedSeq[CategoricalValue[String]], p:Proportions): Double = ds.map(dv => math.log(p(dv.intValue))).sum // TODO Make this more efficient
  def sampledValue(d:CategoricalDomain[String], length:Int, p:Proportions)(implicit random: scala.util.Random): IndexedSeq[CategoricalValue[String]] =
    Vector.fill(length)(d.apply(p.sampleIndex))
  case class Factor(override val _1:CategoricalSeqVariable[String], override val _2:ProportionsVariable) extends super.Factor(_1, _2) {
    def pr(child:IndexedSeq[CategoricalValue[String]], p:Proportions): Double = self.pr(child, p)
    //override def logpr(s:Statistics): Double = self.logpr(s._1, s._2)
    override def sampledValue(implicit random: scala.util.Random): CategoricalSeqVariable[String]#Value = self.sampledValue(_1.head.domain, _1.length, _2.value) // Avoid creating a Statistics
    def sampledValue(p:Proportions)(implicit random: scala.util.Random): IndexedSeq[CategoricalValue[String]] = {
      if (_1.length == 0) IndexedSeq[CategoricalValue[String]]()
      else self.sampledValue(_1.head.domain, _1.length, p)
    }
    def updateCollapsedParents(index:Int, weight:Double): Boolean = { _2.value.+=(_1(index).intValue, weight); true }
  }
  def newFactor(a:CategoricalSeqVariable[String], b:ProportionsVariable) = Factor(a, b)
}
