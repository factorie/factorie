/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

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
    return - diff * diff / (2 * variance) - 0.5 * Math.log(2 * Math.Pi * variance)
  }
  override def logpr: Double = logprFrom(mean.doubleValue, variance.doubleValue)
  def logprFrom(parents:Seq[Variable]): Unit = parents match {
    case Seq(mean:RealVar, variance:RealVar) => logprFrom(mean.doubleValue, variance.doubleValue)
  }
  def pr: Double = Math.exp(logpr)
  def prFrom(parents:Seq[Parameter]): Double = logprFrom(parents)
  def sampleFrom(mean:RealVar, variance:RealVar)(implicit d:DiffList) = 
    set(Maths.nextGaussian(mean.doubleValue, variance.doubleValue)(cc.factorie.random))
  def sample(implicit d:DiffList): Unit = sampleFrom(mean, variance)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(mean:RealVar, variance:RealVar) => sampleFrom(mean, variance)
  }
  override def toString = "Gaussian(mean="+mean.doubleValue+",variance="+variance.doubleValue+")"
}

/** A one-dimensional Gaussian distribution, generating Real (valued) variables.  Default estimation by moment-matching. 
    @author Andrew McCallum */
class Gaussian(val mean:RealVarParameter, val variance:RealVarParameter = new RealVariableParameter(1.0), initialValue:Double = 0.0) extends RealVariable(initialValue) with GaussianVar {
  mean.addChild(this)(null)
  variance.addChild(this)(null)
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
    variance = Math.sqrt(variance / (weightSum - 1))
  }
    */
}
