/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

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
  def meanComponents: FiniteMixture[RealVarParameter]
  def varianceComponents: FiniteMixture[RealVarParameter]
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
  override def parents = List[Parameter](meanComponents, varianceComponents) ++ super.parents
}

class GaussianMixture(val meanComponents:FiniteMixture[RealVarParameter], 
                      val varianceComponents:FiniteMixture[RealVarParameter], 
                      val choice:MixtureChoiceVariable, 
                      initialValue:Double = 0.0)
extends RealVariable(initialValue) with MixtureOutcome with GaussianMixtureVar {
  meanComponents.addChild(this)(null)
  varianceComponents.addChild(this)(null)
}



