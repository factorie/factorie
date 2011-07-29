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

// TODO Consider something like this?
//trait Distribution extends Variable
//class GaussianDistribution(val mean:RealParameter, val variance:RealParameter) extends Distribution {
//  mean.addCascade(this)
//  variance.addCascade(this)
//}

object Gaussian extends GenerativeFamilyWithStatistics3[GeneratedRealVar,RealVarParameter,RealVarParameter] {
  def logpr(value:Double, mean:Double, variance:Double): Double = {
    val diff = value - mean
    return - diff * diff / (2 * variance) - 0.5 * math.log(2.0 * math.Pi * variance)
  }
  override def logpr(s:StatisticsType): Double = logpr(s._1, s._2, s._3)
  def pr(value:Double, mean:Double, variance:Double): Double = math.exp(logpr(value, mean, variance))
  def pr(s:StatisticsType) = math.exp(logpr(s))
  def sampledValue(mean:Double, variance:Double): Double = maths.nextGaussian(mean, variance)(cc.factorie.random)
  def sampledValue(s:StatisticsType): Double = sampledValue(s._2, s._3)
}

// TODO Complete something like this
//object PlatedGaussian extends GenerativeFamilyWithStatistics3[GeneratedRealVar,RealVarParameter,RealVarParameter] 

object GaussianEstimator {
  def minSamplesForVarianceEstimate = 5
  /** This implements a moment-matching estimator. */
  def estimate(meanVar:MutableRealVarParameter, varianceVar:MutableRealVarParameter, map:scala.collection.Map[Variable,Variable] = null): Unit = {
    require(map eq null) // TODO Otherwise not yet supported
    // TODO Ignores the parents of 'mean' and 'variance'.  Fix this.
    require(Set(meanVar.childFactors) == Set(varianceVar.childFactors)) // Expensive check may be unnecessary?
    var mean = 0.0
    var sum = 0.0
    for (factor <- meanVar.childFactors) factor match {
      case g:Gaussian.Factor if (g._2 == meanVar) => { mean += g._1.doubleValue; sum += 1.0 }
      //case gm:GaussianMixture.Factor
    }
    mean /= sum
    meanVar.set(mean)(null)
    if (sum < minSamplesForVarianceEstimate) 
      varianceVar.set(1.0)(null)
    else {
      var v = 0.0
      for (factor <- varianceVar.childFactors) factor match {
        case g:Gaussian.Factor if (g._3 == varianceVar) => { val diff = mean - g._1.doubleValue; v += diff * diff }
        //  case gm:GaussianMixture.Factor
      }
      v = math.sqrt(v / (sum - 1))
      varianceVar.set(v)(null)
      // TODO Note this doesn't work for weighted children
    }
  }
}
