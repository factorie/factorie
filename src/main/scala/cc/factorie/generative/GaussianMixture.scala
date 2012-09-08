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

object GaussianMixture extends GenerativeFamily4[DoubleVar,Mixture[DoubleVar],Mixture[DoubleVar],DiscreteVariable] {
  case class Factor(_1:DoubleVar, _2:Mixture[DoubleVar], _3:Mixture[DoubleVar], _4:DiscreteVariable) extends super.Factor {
    def gate = _4
    override def logpr(s:StatisticsType) = Gaussian.logpr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def pr(s:StatisticsType) = Gaussian.pr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def sampledValue(s:StatisticsType): Double = Gaussian.sampledValue(s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.pr(s._1.doubleValue, s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue) 
    def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.sampledValue(s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue)
  }
  def newFactor(a:DoubleVar, b:Mixture[DoubleVar], c:Mixture[DoubleVar], d:DiscreteVariable) = Factor(a, b, c, d)
  
  // A different version in which all the components share the same variance
  case class FactorSharedVariance(_1:DoubleVar, _2:Mixture[DoubleVar], _3:DoubleVar, _4:DiscreteVariable) extends GenerativeFactorWithStatistics4[DoubleVar,Mixture[DoubleVar],DoubleVar,DiscreteVariable]  {
    def gate = _4
    override def logpr(s:StatisticsType) = Gaussian.logpr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3.doubleValue) 
    def pr(s:StatisticsType) = Gaussian.pr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3.doubleValue) 
    def sampledValue(s:StatisticsType): Double = Gaussian.sampledValue(s._2(s._4.intValue).doubleValue, s._3.doubleValue) 
    def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.pr(s._1.doubleValue, s._2(mixtureIndex).doubleValue, s._3.doubleValue) 
    def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): Double = Gaussian.sampledValue(s._2(mixtureIndex).doubleValue, s._3.doubleValue)
  }
  def newFactor(a:DoubleVar, b:Mixture[DoubleVar], c:DoubleVar, d:DiscreteVariable) = FactorSharedVariance(a, b, c ,d)
  def apply(p1:Mixture[DoubleVar], p2:DoubleVar, p3:DiscreteVariable) = new Function1[DoubleVar,FactorSharedVariance] {
    def apply(c:DoubleVar) = newFactor(c, p1, p2, p3)
  }

}
