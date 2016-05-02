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

object PlatedDiscreteMixture extends DirectedFamily3[DiscreteSeqVariable,Mixture[ProportionsVariable],DiscreteSeqVariable] {
  self =>
  //type Seq[+A] = scala.collection.Seq[A]
  def pr(ds:IndexedSeq[DiscreteValue], mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product // Make product more efficient
  //def pr(ds:Seq[DiscreteValue], mixture:Seq[DoubleSeq], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product
  def logpr(ds:IndexedSeq[DiscreteValue], mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  //def logpr(ds:Seq[DiscreteValue], mixture:Seq[DoubleSeq], gates:Seq[DiscreteValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  def sampledValue(d:DiscreteDomain, mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue])(implicit random: scala.util.Random): IndexedSeq[DiscreteValue] =
    for (i <- 0 until gates.length) yield d.apply(mixture(gates(i).intValue).sampleIndex) 
  case class Factor(override val _1:DiscreteSeqVariable, override val _2:Mixture[ProportionsVariable], override val _3:DiscreteSeqVariable) extends super.Factor(_1, _2, _3) with MixtureFactor {
    def gate = throw new Error("Not yet implemented. Need to make PlatedGate be a Gate?") // f._3
    def pr(child:IndexedSeq[DiscreteValue], mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue]): Double = self.pr(child, mixture, zs)
    override def logpr(child:IndexedSeq[DiscreteValue], mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue]): Double = self.logpr(child, mixture, zs)
    def sampledValue(mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue])(implicit random: scala.util.Random): IndexedSeq[DiscreteValue] = self.sampledValue(_1.domain.elementDomain, mixture, zs)
    def prChoosing(child:IndexedSeq[DiscreteValue], mixture:scala.collection.Seq[Proportions], mixtureIndex:Int): Double = throw new Error("Not yet implemented")
    def sampledValueChoosing(mixture:scala.collection.Seq[Proportions], mixtureIndex:Int)(implicit random: scala.util.Random): ChildType#Value = throw new Error("Not yet implemented")
    //def prValue(s:Statistics, value:Int, index:Int): Double = throw new Error("Not yet implemented")
  }
  def newFactor(a:DiscreteSeqVariable, b:Mixture[ProportionsVariable], c:DiscreteSeqVariable) = Factor(a, b, c)
}

object PlatedCategoricalMixture extends DirectedFamily3[CategoricalSeqVariable[String],Mixture[ProportionsVariable],DiscreteSeqVariable] {
  self =>
  //type Seq[+A] = scala.collection.Seq[A]
  def pr(ds:IndexedSeq[CategoricalValue[String]], mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product // Make product more efficient
  //def pr(ds:Seq[CategoricalValue], mixture:Seq[DoubleSeq], gates:Seq[CategoricalValue]): Double = ds.zip(gates).map(tuple => mixture(tuple._2.intValue).apply(tuple._1.intValue)).product
  def logpr(ds:IndexedSeq[CategoricalValue[String]], mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  //def logpr(ds:Seq[CategoricalValue], mixture:Seq[DoubleSeq], gates:Seq[CategoricalValue]): Double = ds.zip(gates).map(tuple => math.log(mixture(tuple._2.intValue).apply(tuple._1.intValue))).sum  
  def sampledValue(d:CategoricalDomain[String], mixture:scala.collection.Seq[Proportions], gates:IndexedSeq[DiscreteValue])(implicit random: scala.util.Random): IndexedSeq[CategoricalValue[String]] =
    for (i <- 0 until gates.length) yield d.apply(mixture(gates(i).intValue).sampleIndex) 
  case class Factor(override val _1:CategoricalSeqVariable[String], override val _2:Mixture[ProportionsVariable], override val _3:DiscreteSeqVariable) extends super.Factor(_1, _2, _3) with MixtureFactor {
    def gate = throw new Error("Not yet implemented. Need to make PlatedGate be a Gate?") // f._3
    def pr(child:IndexedSeq[CategoricalValue[String]], mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue]): Double = self.pr(child, mixture, zs)
    override def logpr(child:IndexedSeq[CategoricalValue[String]], mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue]): Double = self.logpr(child, mixture, zs)
    def sampledValue(mixture:scala.collection.Seq[Proportions], zs:IndexedSeq[DiscreteValue])(implicit random: scala.util.Random): IndexedSeq[CategoricalValue[String]] = self.sampledValue(_1.head.domain, mixture, zs)
    def prChoosing(child:IndexedSeq[CategoricalValue[String]], mixture:scala.collection.Seq[Proportions], mixtureIndex:Int): Double = throw new Error("Not yet implemented")
    def sampledValueChoosing(mixture:scala.collection.Seq[Proportions], mixtureIndex:Int)(implicit random: scala.util.Random): ChildType#Value = throw new Error("Not yet implemented")
    //def prValue(s:Statistics, value:Int, index:Int): Double = throw new Error("Not yet implemented")
  }
  def newFactor(a:CategoricalSeqVariable[String], b:Mixture[ProportionsVariable], c:DiscreteSeqVariable) = Factor(a, b, c)
}
