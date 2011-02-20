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

class DiscreteMixtureTemplate extends GenerativeTemplateWithStatistics3[DiscreteMixtureVar,MixtureComponents[Proportions],MixtureChoiceVar] with MixtureGenerativeTemplate {
  //class Factor(o:DiscreteMixtureVar, mc:MixtureComponents[Proportions], c:MixtureChoiceVar) extends super.Factor(o, mc, c)
  def unroll1(d:DiscreteMixtureVar) = Factor(d, d.components, d.choice)
  def unroll2(m:MixtureComponents[Proportions]) = for (d <- m.childrenOfClass[DiscreteMixtureVar]) yield Factor(d, m, d.choice)
  def unroll3(g:MixtureChoiceVar) = for (d <- g.outcomesOfClass[DiscreteMixtureVar]; if (classOf[DiscreteMixtureVar].isAssignableFrom(d.getClass))) yield Factor(d, d.components, g)
  def prChoosing(s:StatisticsType, mixtureIndex:Int) = pr(s._1.intValue, s._2, mixtureIndex)
  def logprChoosing(s:StatisticsType, mixtureIndex:Int) = math.log(prChoosing(s, mixtureIndex))
  def pr(s:StatisticsType): Double = pr(s._1.intValue, s._2, s._3.intValue)
  def pr(value:Int, mixture:Seq[Proportions], mixtureIndex:Int): Double = {
    //println("DiscreteMixtureTemplate component="+mixture(mixtureIndex).getClass+" "+mixture(mixtureIndex).getClass.getSuperclass)
    mixture(mixtureIndex).apply(value)
  }
  def logpr(s:StatisticsType) = math.log(pr(s))
  def sampledValue(s:StatisticsType): DiscreteValue = sampledValue(s._1.domain, s._2, s._3.intValue)
  def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): DiscreteValue = sampledValue(s._1.domain, s._2, mixtureIndex)
  def sampledValue(domain:DiscreteDomain, proportions:Seq[Proportions], mixtureIndex:Int): DiscreteValue = domain.getValue(proportions(mixtureIndex).sampleInt)
}
object DiscreteMixtureTemplate extends DiscreteMixtureTemplate

trait DiscreteMixtureVar extends DiscreteVar with MixtureGeneratedVar {
  val generativeTemplate = DiscreteMixtureTemplate
  def generativeFactor = new DiscreteMixtureTemplate.Factor(this, components, choice)
  private var _components: FiniteMixture[Proportions] = null
  def components: FiniteMixture[Proportions] = _components
  def setComponents(c:FiniteMixture[Proportions]): Unit = {
    if (_components ne null) _components.removeChild(this)(null)
    _components = c
    _components.addChild(this)(null)
  }
  private var _choice: MixtureChoiceVar = null
  def choice: MixtureChoiceVar = _choice
  def setChoice(c:MixtureChoiceVar): Unit = {
    if (_choice ne null) _choice.removeOutcome(this)
    _choice = c
    _choice.addOutcome(this)
  }
  def chosenParents = List(components(choice.intValue))
  def proportions = components(choice.intValue)
  // override for efficiency
  override def prChoosing(mixtureIndex:Int) = components(choice.intValue).apply(this.intValue) 
  override def sampledValueChoosing(mixtureIndex:Int): Value = domain.getValue(components(choice.intValue).sampleInt)
}

abstract class DiscreteMixture(components: FiniteMixture[Proportions], choice:MixtureChoiceVar, initialValue:Int = 0) 
         extends DiscreteVariable(initialValue) with DiscreteMixtureVar with MutableGeneratedVar 
{
  setComponents(components)
  setChoice(choice)
}

abstract class CategoricalMixture[A](components: FiniteMixture[Proportions], choice:MixtureChoiceVar, initialValue:A)
         extends CategoricalVariable(initialValue) with DiscreteMixtureVar with MutableGeneratedVar 
{
  setComponents(components)
  setChoice(choice)
}


abstract class DiscreteMixtureMux(components: FiniteMixture[Proportions], choice:MixtureChoiceMux, initialValues:Seq[Int]) 
         extends DiscreteMuxVariable(initialValues) with DiscreteMixtureVar with MutableGeneratedVar 
{
  setComponents(components)
  setChoice(choice)
}

abstract class CategoricalMixtureMux[A](components: FiniteMixture[Proportions], choice:MixtureChoiceVar, initialCategories:Seq[A])
         extends CategoricalMuxVariable(initialCategories) with DiscreteMixtureVar with MutableGeneratedVar 
{
  setComponents(components)
  setChoice(choice)
}

