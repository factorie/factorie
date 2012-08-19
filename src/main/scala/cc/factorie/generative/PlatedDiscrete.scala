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
import cc.factorie.util.DoubleSeq
import scala.collection.mutable.ArrayBuffer

/*
trait PlatedDiscreteGeneratingFactor extends GenerativeFactor {
  def prValue(s:StatisticsType, value:Int, index:Int): Double
  def prValue(value:Int, index:Int): Double = prValue(statistics, value, index)
}
*/

object PlatedDiscrete extends GenerativeFamily2[DiscreteSeqVar,ProportionsVar] {
  self =>
  //def pr(ds:Seq[DiscreteValue], p:IndexedSeq[Double]): Double = ds.map(dv => p(dv.intValue)).product
  def pr(ds:Seq[DiscreteValue], p:Proportions): Double = ds.map(dv => p(dv.intValue)).product // TODO Make this more efficient; this current boxes
  def logpr(ds:Seq[DiscreteValue], p:Proportions): Double = ds.map(dv => math.log(p(dv.intValue))).sum // TODO Make this more efficient
  def sampledValue(d:DiscreteDomain, length:Int, p:Proportions): Seq[DiscreteValue] = 
    Vector.fill(length)(d.apply(p.sampleIndex))
  case class Factor(_1:DiscreteSeqVar, _2:ProportionsVar) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2)
    override def logpr(s:Statistics): Double = self.logpr(s._1, s._2)
    override def sampledValue: Any = self.sampledValue(_1.discreteValues.head.domain, _1.length, _2.value) // Avoid creating a Statistics
    def sampledValue(s:Statistics): Seq[DiscreteValue] = {
      if (s._1.length == 0) Nil
      else self.sampledValue(s._1.head.domain, s._1.length, s._2)
    }
    def updateCollapsedParents(index:Int, weight:Double): Boolean = { _2.tensor.+=(_1(index).intValue, weight); true }
  }
  def newFactor(a:DiscreteSeqVar, b:ProportionsVar) = Factor(a, b)
}
