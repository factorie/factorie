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

trait MixtureSeqGenerativeTemplate extends GenerativeTemplate {
  type TemplateType <: MixtureSeqGenerativeTemplate
  type ChildType <: MixtureSeqGeneratedVar
  def prChoosing(s:StatisticsType, seqIndex:Int, mixtureIndex:Int): Double
  def prChoosing(s:cc.factorie.Statistics, seqIndex:Int, mixtureIndex:Int): Double = 
    prChoosing(s.asInstanceOf[StatisticsType], seqIndex, mixtureIndex)
  def logprChoosing(s:StatisticsType, seqIndex:Int, mixtureIndex:Int): Double
  def logprChoosing(s:cc.factorie.Statistics, seqIndex:Int, mixtureIndex:Int): Double = 
    logprChoosing(s.asInstanceOf[StatisticsType], seqIndex, mixtureIndex)
  def sampledValueChoosing(s:StatisticsType, mixtureIndices:Seq[Int]): ChildType#Value
  def sampledValueChoosing(s:cc.factorie.Statistics, mixtureIndices:Seq[Int]): ChildType#Value = sampledValueChoosing(s.asInstanceOf[StatisticsType], mixtureIndices)
}

trait MixtureSeqGeneratedVar extends GeneratedVar {
  override val generativeTemplate: MixtureSeqGenerativeTemplate
  def choice: MixtureChoiceSeqVar
  //def chosenParents: Seq[Parameter]
  def prChoosing(seqIndex:Int, mixtureIndex:Int): Double = generativeTemplate.prChoosing(generativeFactor.statistics, seqIndex, mixtureIndex)
  def logprChoosing(seqIndex:Int, mixtureIndex:Int): Double = generativeTemplate.logprChoosing(generativeFactor.statistics, seqIndex, mixtureIndex)
  def sampledValueChoosing(mixtureIndices:Seq[Int]): Value = generativeTemplate.sampledValueChoosing(generativeFactor.statistics, mixtureIndices).asInstanceOf[Value]  // TODO Can we get rid of this cast?
}

trait MixtureChoiceSeqVar extends DiscreteSeqVariable with MutableGeneratedVar {
  private var _outcomes: List[MixtureSeqGeneratedVar] = Nil
  def outcomes = _outcomes
  def outcomesOfClass[A<:MixtureSeqGeneratedVar](implicit m:Manifest[A]): Iterable[A] = outcomes.filter(o => m.erasure.isAssignableFrom(o.getClass)).asInstanceOf[Iterable[A]]
  def addOutcome(o:MixtureSeqGeneratedVar): Unit = _outcomes = o +: _outcomes
  def removeOutcome(o:MixtureSeqGeneratedVar): Unit = _outcomes = _outcomes.filterNot(_ == o)
  def prChoosing(value:Int): Double
  def update(seqIndex:Int, intValue:Int): Unit = update(seqIndex, domain.elementDomain.getValue(intValue))(null)
}

abstract class MixtureChoiceSeq(p:Proportions, initialValue: Seq[Int] = Nil) extends DiscreteSeq(p, initialValue) with MixtureChoiceSeqVar {
  def prChoosing(value:Int): Double = proportions(value)
}
