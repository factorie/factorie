/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.directed

import cc.factorie._
import cc.factorie.variable.{DiscreteValue, DiscreteVariable, DoubleVar, DoubleVariable}

/** Beta distribution.
    http://en.wikipedia.org/wiki/Beta_distribution */
object Beta extends DirectedFamily3[DoubleVar,DoubleVar,DoubleVar] { self =>
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
  def sampledValue(alpha:Double, beta:Double)(implicit random: scala.util.Random): Double = {
    val x = maths.nextGamma(alpha, 1.0)
    val y = maths.nextGamma(beta, 1.0)
    x / (x + y)
  }
  // TODO Consider making this not a case class, but Factor1 be a case class?
  case class Factor(override val _1:DoubleVar, override val _2:DoubleVar, override val _3:DoubleVar) extends super.Factor(_1, _2, _3) {
    def pr(child:Double, alpha:Double, beta:Double): Double = self.pr(child, alpha, beta)
    def sampledValue(alpha:Double, beta:Double)(implicit random: scala.util.Random): Double = self.sampledValue(alpha, beta)
  }
  def newFactor(_1:DoubleVar, _2:DoubleVar, _3:DoubleVar) = Factor(_1, _2, _3)
}


object BetaMixture extends DirectedFamily4[DoubleVariable,Mixture[DoubleVariable],Mixture[DoubleVariable],DiscreteVariable] {
  type Seq[+A] = scala.collection.Seq[A]
  case class Factor(override val _1:DoubleVariable, override val _2:Mixture[DoubleVariable], override val _3:Mixture[DoubleVariable], override val _4:DiscreteVariable) extends super.Factor(_1, _2, _3, _4) {
    def gate = _4
    def pr(child:Double, alpha:Seq[Double], beta:Seq[Double], z:DiscreteValue) = Beta.pr(child, alpha(z.intValue), beta(z.intValue)) 
    def sampledValue(alpha:Seq[Double], beta:Seq[Double], z:DiscreteValue)(implicit random: scala.util.Random): Double = Beta.sampledValue(alpha(z.intValue), beta(z.intValue))
    def prChoosing(child:Double, alpha:Seq[Double], beta:Seq[Double], z:Int): Double = Beta.pr(child, alpha(z), beta(z)) 
    def sampledValueChoosing(alpha:Seq[Double], beta:Seq[Double], z:Int)(implicit random: scala.util.Random): Double = Beta.sampledValue(alpha(z), beta(z))
  }
  def newFactor(a:DoubleVariable, b:Mixture[DoubleVariable], c:Mixture[DoubleVariable], d:DiscreteVariable) = Factor(a, b, c, d)
}

object MaximizeBetaByMomentMatching {
  def maxAlpha(mean:Double, variance:Double): Double = {
    require(mean >= 0.0 && mean <= 1.0)
    require(variance >= 0.0 && variance <= 1.0)
    val result = mean * (((mean * (1 - mean)) / variance) - 1)
    //println("MaximizeBetaByMomentMatching.maxAlpha mean="+mean+" variance="+variance+" alpha="+result)
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
  def apply(alpha:DoubleVariable, beta:DoubleVariable, model:DirectedModel): Unit = {
    val childFactors = model.extendedChildFactors(alpha) // Assume that beta has all the same children
    val ds = new cc.factorie.util.ArrayDoubleSeq(childFactors.size)
    var i = 0
    for (factor <- childFactors) factor match {
      case f:Beta.Factor => { ds(i) = f._1.doubleValue; i += 1 }
      case f:BetaMixture.Factor if f._2(f._4.intValue) == alpha => {
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
