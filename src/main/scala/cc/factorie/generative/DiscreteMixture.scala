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

trait DiscreteMixtureGeneratingFamily extends DiscreteGeneratingFamily with MixtureFamily {
  type FamilyType <: DiscreteGeneratingFamily with MixtureFamily
}

object DiscreteMixture extends DiscreteMixtureGeneratingFamily /* TODO DiscreteGeneratingFamily with MixtureFamily */ with GenerativeFamilyWithStatistics3[GeneratedDiscreteVar,Mixture[Proportions],Gate] {
  def gate(f:Factor) = f._3
  def pr(s:StatisticsType) = s._2(s._3.intValue).apply(s._1.intValue)
  def sampledValue(s:StatisticsType): DiscreteValue = s._1.domain.getValue(s._2(s._3.intValue).sampleInt)
  def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = s._2(mixtureIndex).apply(s._1.intValue)
  def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): ChildType#Value = s._1.domain.getValue(s._2(mixtureIndex).sampleInt)
  def prValue(f:Stat, intValue:Int): Double = f._2.apply(f._3.intValue).apply(intValue)
  def prValue(f:Factor, intValue:Int): Double = f._2.apply(f._3.intValue).apply(intValue)
  override def updateCollapsedParents(f:Factor, weight:Double): Boolean = {
    f._2(f._3.intValue) match {
      case p:DenseCountsProportions => { p.increment(f._1.intValue, weight)(null); true }
      case _ => false // Just throw Error instead; change API to return Unit also and always throw Error for unsupported cases
    }
  }
}

