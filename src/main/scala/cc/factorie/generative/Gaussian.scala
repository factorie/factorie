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

class GaussianTemplate extends GenerativeTemplateWithStatistics3[Gaussian,GaussianMeanParameter,GaussianVarianceParameter] {
  def unroll1(g:Gaussian) = Factor(g, g.mean, g.variance)
  def unroll2(m:GaussianMeanParameter) = for (g <- m.childrenOfClass[Gaussian]) yield Factor(g, m, g.variance)
  def unroll3(v:GaussianVarianceParameter) = for (g <- v.childrenOfClass[Gaussian]) yield Factor(g, g.mean, v)
  def logpr(s:Stat): Double = logpr(s._1, s._2, s._3)
  def logpr(value:Double, mean:Double, variance:Double): Double = {
    val diff = value - mean
    return - diff * diff / (2 * variance) - 0.5 * math.log(2.0 * math.Pi * variance)
  }
  def pr(s:Stat) = math.exp(logpr(s))
  def sampledValue(s:Stat): Double = sampledValue(s._2, s._3)
  def sampledValue(mean:Double, variance:Double): Double = maths.nextGaussian(mean, variance)(cc.factorie.random)
}
object GaussianTemplate extends GaussianTemplate
object GaussianVarianceConstant1 extends GaussianVarianceParameter { def doubleValue = 1.0 }

class Gaussian(val mean:GaussianMeanParameter, val variance:GaussianVarianceParameter = GaussianVarianceConstant1, initialValue: Double = 0.0) extends RealVariable(initialValue) with MutableGeneratedVar {
  mean.addChild(this)(null)
  variance.addChild(this)(null)
  val generativeTemplate = GaussianTemplate
  def generativeFactor = new GaussianTemplate.Factor(this, mean, variance)
  override def toString = "Gaussian(mean="+mean.doubleValue+",variance="+variance.doubleValue+")"
}

trait GaussianMeanParameter extends RealVarParameter
class GaussianMeanVariable(x:Double) extends RealVariableParameter(x) with GaussianMeanParameter with Estimation[GaussianMeanVariable] {
  def defaultEstimator = GaussianMeanEstimator
}

trait GaussianVarianceParameter extends RealVarParameter
class GaussianVarianceVariable(x:Double) extends RealVariableParameter(x) with GaussianVarianceParameter

// TODO or consider instead the following, so that the same RealVar can be used as
// multiple different parameter types?
/*class GaussianMeanVariable(x:RealVar) extends RealFunction {
  def doubleValue = x.doubleValue
}*/

object GaussianMeanEstimator extends Estimator[GaussianMeanVariable] {
  def estimate(g:GaussianMeanVariable, map:scala.collection.Map[Variable,Variable]): Unit = {
    var m = 0.0
    var sum = 0.0
    for ((child, weight) <- g.weightedGeneratedChildren(map)) child match {
      case x:RealVar => { m += x.doubleValue * weight; sum += weight }
      //case g:GaussianDistribution...
    }
    g.set(m/sum)(null)
  }
}

object Gaussian {
  def minSamplesForVarianceEstimate = 5
  /** This implements a moment-matching estimator. */
  /*def estimate: Unit = {
  	throw new Error
    if (generatedSamples.size == 0) { mean = 0.0; variance = 1.0; return }
    mean = 0.0
    var weightSum = 0.0
    for ((s,w) <- weightedGeneratedSamples) { mean += s.doubleValue * w; weightSum += w }
    mean /= weightSum
    if (weightSum < minSamplesForVarianceEstimate) { variance = 1.0; return }
    variance = 0.0
    for ((s,w) <- weightedGeneratedSamples) { 
      val diff = mean - s.doubleValue
      variance += diff * diff * w
    }
    variance = math.sqrt(variance / (weightSum - 1))
  }
    */
}
