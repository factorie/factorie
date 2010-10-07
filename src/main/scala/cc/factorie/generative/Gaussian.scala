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

// TODO I am now storing the mean and variance as Real variables, so that they can, in turn, be generated from other distributions.
// Perhaps we need to do this for all other Distributions also?

trait GaussianVar extends RealVariable with GeneratedVariable {
  def mean:RealVarParameter
  def variance:RealVarParameter
  def parents: Seq[Parameter] = List(mean, variance)
  def logprFrom(mean:Double, variance:Double): Double = {
    val diff = this.doubleValue - mean
    return - diff * diff / (2 * variance) - 0.5 * math.log(2 * math.Pi * variance)
  }
  override def logpr: Double = logprFrom(mean.doubleValue, variance.doubleValue)
  def logprFrom(parents:Seq[Variable]): Unit = parents match {
    case Seq(mean:RealVar, variance:RealVar) => logprFrom(mean.doubleValue, variance.doubleValue)
  }
  def pr: Double = math.exp(logpr)
  def prFrom(parents:Seq[Parameter]): Double = logprFrom(parents)
  def sampleFrom(mean:RealVar, variance:RealVar)(implicit d:DiffList) = 
    set(maths.nextGaussian(mean.doubleValue, variance.doubleValue)(cc.factorie.random))
  def sampleFromParents(implicit d:DiffList = null): this.type = { sampleFrom(mean, variance); this }
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): this.type = {
    parents match {
      case Seq(mean:RealVar, variance:RealVar) => sampleFrom(mean, variance)
    }
    this
  }
  override def toString = "Gaussian(mean="+mean.doubleValue+",variance="+variance.doubleValue+")"
}

/** A one-dimensional Gaussian distribution, generating Real (valued) variables.  Default estimation by moment-matching. 
    @author Andrew McCallum */
class Gaussian(val mean:RealVarParameter, val variance:RealVarParameter = new RealVariableParameter(1.0), initialValue:Double = 0.0) extends RealVariable(initialValue) with GaussianVar {
  mean.addChild(this)(null)
  variance.addChild(this)(null)
}

class GaussianMeanVariable(x:Double) extends RealVariableParameter(x) with Estimation[GaussianMeanVariable] {
  def defaultEstimator = GaussianMeanEstimator
}
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
