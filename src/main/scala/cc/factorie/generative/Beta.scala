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

/** Beta distribution.
    http://en.wikipedia.org/wiki/Beta_distribution */
object Beta extends GenerativeFamily3[DoubleVar,DoubleVar,DoubleVar] { self =>
  def mode(alpha:Double, beta:Double): Double = 
    if (alpha > 1 && beta > 1) (alpha - 1) / (alpha + beta - 2)
    else Double.NaN
  def mean(alpha:Double, beta:Double): Double = alpha / (alpha + beta)
  def variance(alpha:Double, beta:Double): Double = { val sum = alpha + beta; alpha * beta / (sum * sum * (sum + 1) ) }
  def logpr(value:Double, alpha:Double, beta:Double): Double = math.log(pr(value, alpha, beta))
  def pr(value:Double, alpha:Double, beta:Double): Double = {
    require(value >= 0.0 && value <= 1.0)
    val result = math.pow(value, alpha-1.0) * math.pow(1.0-value, beta-1.0) / maths.beta(alpha, beta)
    require(result >= 0.0)
    require(result <= 1.0, "value="+value+" alpha="+alpha+" beta="+beta+" result="+result)
    result
  }
  def sampledValue(alpha:Double, beta:Double): Double = {
    val x = maths.nextGamma(alpha, 1.0)(cc.factorie.random) 
    val y = maths.nextGamma(beta, 1.0)(cc.factorie.random)
    x / (x + y)
  }
  case class Factor(_1:DoubleVar, _2:DoubleVar, _3:DoubleVar) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2, s._3)
    def sampledValue(s:Statistics): Double = self.sampledValue(s._2, s._3)
  }
  def newFactor(_1:DoubleVar, _2:DoubleVar, _3:DoubleVar) = Factor(_1, _2, _3)
}


object BetaMixture extends GenerativeFamily4[DoubleVar,Mixture[DoubleVar],Mixture[DoubleVar],DiscreteVariable] {
  case class Factor(_1:DoubleVar, _2:Mixture[DoubleVar], _3:Mixture[DoubleVar], _4:DiscreteVariable) extends super.Factor {
    def gate = _4
    override def logpr(s:StatisticsType) = Beta.logpr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def pr(s:StatisticsType) = Beta.pr(s._1.doubleValue, s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def sampledValue(s:StatisticsType): Double = Beta.sampledValue(s._2(s._4.intValue).doubleValue, s._3(s._4.intValue).doubleValue) 
    def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = Beta.pr(s._1.doubleValue, s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue) 
    def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): Double = Beta.sampledValue(s._2(mixtureIndex).doubleValue, s._3(mixtureIndex).doubleValue)
  }
  def newFactor(a:DoubleVar, b:Mixture[DoubleVar], c:Mixture[DoubleVar], d:DiscreteVariable) = Factor(a, b, c, d)
}

object MaximizeBetaByMomentMatching {
  def maxAlpha(mean:Double, variance:Double): Double = {
    require(mean >= 0.0 && mean <= 1.0)
    require(variance >= 0.0 && variance <= 1.0)
    val result = mean * (((mean * (1 - mean)) / variance) - 1)
    println("MaximizeBetaByMomentMatching.maxAlpha mean="+mean+" variance="+variance+" alpha="+result)
    require(result >= 0.0, "mean="+mean+" variance="+variance)
    result
  }
  def maxBeta(mean:Double, variance:Double): Double = {
    require(mean >= 0.0 && mean <= 1.0)
    require(variance >= 0.0 && variance <= 1.0)
    val result = (1 - mean) * (((mean * (1 - mean)) / variance) - 1)
    require(result >= 0.0, "mean="+mean+" variance="+variance)
    result
  }
  def apply(alpha:DoubleVariable, beta:DoubleVariable, model:GenerativeModel): Unit = {
    val childFactors = model.extendedChildFactors(alpha) // Assume that beta has all the same children
    val ds = new cc.factorie.util.ArrayDoubleSeq(childFactors.size)
    var i = 0
    for (factor <- childFactors) factor match {
      case f:Beta.Factor => { ds(i) = f._1.doubleValue; i += 1 }
      case f:BetaMixture.Factor if (f._2(f._4.intValue) == alpha) => {
        assert(f._3(f._4.intValue) == beta)
        ds(i) = f._1.doubleValue; i += 1
      }
    }
    val mean = maths.sampleMean(ds)
    val variance = maths.sampleVariance(ds, mean)
    alpha := maxAlpha(mean, variance)
    beta := maxBeta(mean, variance)
  }
}
