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

object GaussianMixture extends GenerativeFamilyWithStatistics4[GeneratedRealVar,Mixture[RealVarParameter],Mixture[RealVarParameter],Gate] with MixtureFamily {
  def gate(f:Factor) = f._4
  override def logpr(s:StatisticsType) = Gaussian.logpr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
  def pr(s:StatisticsType) = Gaussian.pr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
  def sampledValue(s:StatisticsType): Double = Gaussian.sampledValue(s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
  def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.pr(s._1.doubleValue, s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue) 
  def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.sampledValue(s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue)
}
