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
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random

class GaussianMixtureTemplate extends GenerativeTemplateWithStatistics4[GaussianMixture,MixtureComponents[GaussianMeanParameter],MixtureComponents[GaussianVarianceParameter],MixtureChoiceVar] with MixtureGenerativeTemplate[GaussianMixture] {
  def unroll1(g:GaussianMixture) = Factor(g, g.meanComponents, g.varianceComponents, g.choice)
  def unroll2(m:MixtureComponents[GaussianMeanParameter]) = for (g <- m.childrenOfClass[GaussianMixture]) yield Factor(g, m, g.varianceComponents, g.choice)
  def unroll3(p:MixtureComponents[GaussianVarianceParameter]) = for (g <- p.childrenOfClass[GaussianMixture]) yield Factor(g, g.meanComponents, p, g.choice)
  def unroll4(c:MixtureChoiceVar) = for (g <- c.outcomesOfClass[GaussianMixture]) yield Factor(g, g.meanComponents, g.varianceComponents, c)
  def pr(s:Stat) = math.exp(logpr(s))
  def logpr(s:Stat): Double = logpr(s._1, s._2, s._3, s._4.intValue)
  def logpr(value:Double, meanComponents:Seq[RealVar], varianceComponents:Seq[RealVar], mixtureIndex:Int): Double = 
    GaussianTemplate.logpr(value, meanComponents(mixtureIndex).doubleValue, varianceComponents(mixtureIndex).doubleValue)
  def prChoosing(s:Stat, mixtureIndex:Int) = math.exp(logprChoosing(s, mixtureIndex))
  def logprChoosing(s:Stat, mixtureIndex:Int) = logpr(s._1, s._2, s._3, mixtureIndex)
  def sampledValue(s:Stat): Double = sampledValueChoosing(s, s._4.intValue)
  def sampledValueChoosing(s:Stat, mixtureIndex:Int): Double = 
    GaussianTemplate.sampledValue(s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue)
}
object GaussianMixtureTemplate extends GaussianMixtureTemplate


class GaussianMixture(val meanComponents:MixtureComponents[GaussianMeanParameter], 
                      val varianceComponents:MixtureComponents[GaussianVarianceParameter], 
                      val choice:MixtureChoiceVar,
                      initialValue:Double = 0.0)
extends RealVariable(initialValue) with MixtureGeneratedVar {
  choice.addOutcome(this)
  meanComponents.addChild(this)(null)
  varianceComponents.addChild(this)(null)
  val generativeTemplate = GaussianMixtureTemplate
  def generativeFactor = new GaussianMixtureTemplate.Factor(this, meanComponents, varianceComponents, choice)
  def mean = meanComponents(choice.intValue)
  def variance = varianceComponents(choice.intValue)
  def parentsFromMixtureComponent(index:Int) = List(meanComponents(index), varianceComponents(index))
  def chosenParents = List(meanComponents(choice.intValue), varianceComponents(choice.intValue))
  override def parents = List[Parameter](meanComponents, varianceComponents)
  // Above also used to include: ++ super.parents
}



