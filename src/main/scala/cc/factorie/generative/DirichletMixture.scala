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

class DirichletMixtureTemplate extends GenerativeTemplateWithStatistics4[DirichletMixture,MixtureComponents[Proportions],MixtureComponents[RealVarParameter],MixtureChoiceVar] {
  def unroll1(d:DirichletMixture) = Factor(d, d.meanComponents, d.precisionComponents, d.choice)
  def unroll2(m:MixtureComponents[Proportions]) = for (d <- m.childrenOfClass[DirichletMixture]) yield Factor(d, m, d.precisionComponents, d.choice)
  def unroll3(p:MixtureComponents[RealVarParameter]) = for (d <- p.childrenOfClass[DirichletMixture]) yield Factor(d, d.meanComponents, p, d.choice)
  def unroll4(mc:MixtureChoiceVar) = for (d <- mc.outcomesOfClass[DirichletMixture]) yield Factor(d, d.meanComponents, d.precisionComponents, mc)
  def logpr(s:Stat) = math.log(pr(s))
  def pr(s:Stat): Double = pr(s._1, s._2, s._3, s._4.intValue)
  def pr(value:ProportionsValue, meanComponents:Seq[Proportions], precisionComponents:Seq[RealVarParameter], mixtureIndex:Int): Double = 
    DirichletTemplate.pr(value, meanComponents(mixtureIndex), precisionComponents(mixtureIndex).doubleValue)
  def logprChoosing(s:Stat, mixtureIndex:Int) = math.log(prChoosing(s, mixtureIndex))
  def prChoosing(s:Stat, mixtureIndex:Int) = pr(s._1, s._2, s._3, mixtureIndex)
  def sampledValue(s:Stat): ProportionsValue = sampledValueChoosing(s, s._4.intValue)
  def sampledValueChoosing(s:Stat, mixtureIndex:Int): ProportionsValue = 
    DirichletTemplate.sampledValue(s._2(mixtureIndex), s._3(mixtureIndex).doubleValue)
}
object DirichletMixtureTemplate extends DirichletMixtureTemplate


/** A Proportions generated from a Mixture of Dirichlet(mean,precision). 
    @author Andrew McCallum */
// TODO Make this no longer abstract
abstract class DirichletMixture(val meanComponents:FiniteMixture[Proportions], 
                       val precisionComponents:FiniteMixture[RealVarParameter],
                       val choice:MixtureChoiceVar,
                       initialValue:ProportionsValue = Nil)
extends DenseCountsProportions(meanComponents(choice.intValue)) with MixtureGeneratedVar with CollapsibleParameter {
  meanComponents.addChild(this)(null)
  precisionComponents.addChild(this)(null)
  choice.addOutcome(this)
  // TODO Fix this next line.  How do I return more than one MixtureComponents
  def components = throw new Error
  def mean = meanComponents(choice.intValue)
  def precision = precisionComponents(choice.intValue)
  //def parentsFromMixtureComponent(index:Int) = List(meanComponents(index), precisionComponents(index))
  //def chosenParents = Seq(meanComponents(choice.intValue), precisionComponents(choice.intValue))
  override def parents: Seq[Parameter] = List[Parameter](meanComponents, precisionComponents) ++ super.parents
  // TODO But note that this below will not yet support sampling of 'choice' with collapsing.
  type CollapsedType = DenseDirichletMultinomial // Make this DenseDirichletMultinomialMixture to support sampling 'choice' with collapsing
  def newCollapsed = {
    //println("DenseDirichletMixture.newCollapsed mean.size="+mean.size)
    new DenseDirichletMultinomial(mean, precision)
  }
  def setFromCollapsed(c:CollapsedType)(implicit d:DiffList): Unit = set(c)(d)
}

