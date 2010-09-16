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

trait GaussianMixtureVar extends GaussianVar with MixtureOutcome {
  choice.addOutcome(this)
  meanComponents.addChild(this)(null)
  varianceComponents.addChild(this)(null)
  def choice: MixtureChoiceVariable
  def meanComponents: MixtureComponents[RealVarParameter]
  def varianceComponents: MixtureComponents[RealVarParameter]
  def mean = meanComponents(choice.intValue)
  def variance = varianceComponents(choice.intValue)
  def logprFromMixtureComponent(index:Int): Double = logprFrom(meanComponents(index).doubleValue, varianceComponents(index).doubleValue)
  def prFromMixtureComponent(index:Int) = math.exp(logprFromMixtureComponent(index))
  def logprFromMixtureComponent(map:scala.collection.Map[Parameter,Parameter], index:Int): Double = {
    val mean = meanComponents(index)
    val variance = varianceComponents(index)
    logprFrom(map.getOrElse(mean, mean).asInstanceOf[RealVar].doubleValue, map.getOrElse(variance, variance).asInstanceOf[RealVar].doubleValue)
  }
  def prFromMixtureComponent(map:scala.collection.Map[Parameter,Parameter], index:Int) = math.exp(logprFromMixtureComponent(map, index))
  def parentsFromMixtureComponent(index:Int) = List(meanComponents(index), varianceComponents(index))
  def chosenParents = List(meanComponents(choice.intValue), varianceComponents(choice.intValue))
  override def parents = List[Parameter](meanComponents, varianceComponents)
  // Above also used to include: ++ super.parents
}

class GaussianMixture(val meanComponents:MixtureComponents[RealVarParameter], 
                      val varianceComponents:MixtureComponents[RealVarParameter], 
                      val choice:MixtureChoiceVariable, 
                      initialValue:Double = 0.0)
extends RealVariable(initialValue) with MixtureOutcome with GaussianMixtureVar {
  //println("new GaussianMixture hashCode="+System.identityHashCode(this))
  //println("meanComponents.children: "+meanComponents.children.mkString(", "))
  //meanComponents.addChild(this)(null)
  //varianceComponents.addChild(this)(null)
}



