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
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random

object PlatedDiscreteMixture extends GenerativeFamily3[DiscreteSeqVar,Mixture[ProportionsVar],DiscreteSeqVariable] {
  self =>
  def pr(ds:Seq[DiscreteValue], mixture:Seq[Proportions], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product // Make product more efficient
  //def pr(ds:Seq[DiscreteValue], mixture:Seq[DoubleSeq], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product
  def logpr(ds:Seq[DiscreteValue], mixture:Seq[Proportions], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  //def logpr(ds:Seq[DiscreteValue], mixture:Seq[DoubleSeq], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  def sampledValue(d:DiscreteDomain, mixture:Seq[Proportions], gates:Seq[DiscreteValue]): Seq[DiscreteValue] = 
    for (i <- 0 until gates.length) yield d.apply(mixture(gates(i).intValue).sampleIndex) 
  case class Factor(_1:DiscreteSeqVar, _2:Mixture[ProportionsVar], _3:DiscreteSeqVariable) extends super.Factor with MixtureFactor {
    def gate = throw new Error("Not yet implemented. Need to make PlatedGate be a Gate?") // f._3
    def pr(s:Statistics): Double = self.pr(s._1, s._2, s._3)
    override def logpr(s:Statistics): Double = self.logpr(s._1, s._2, s._3)
    def sampledValue(s:Statistics): Seq[DiscreteValue] = self.sampledValue(s._1.head.domain, s._2, s._3)
    def prChoosing(s:Statistics, mixtureIndex:Int): Double = throw new Error("Not yet implemented")
    def sampledValueChoosing(s:Statistics, mixtureIndex:Int): ChildType#Value = throw new Error("Not yet implemented")
    def prValue(s:Statistics, value:Int, index:Int): Double = throw new Error("Not yet implemented")
  }
  def newFactor(a:DiscreteSeqVar, b:Mixture[ProportionsVar], c:DiscreteSeqVariable) = Factor(a, b, c)
}
