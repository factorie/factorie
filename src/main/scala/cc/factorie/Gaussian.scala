/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

// TODO I am now storing the mean and variance as Real variables, so that they can, in turn, be generated from other distributions.
// Perhaps we need to do this for all other Distributions also?

/** A one-dimensional Gaussian distribution, generating Real (valued) variables.  Default estimation by moment-matching. 
    @author Andrew McCallum */
class Gaussian(val mean:RealVarParameter, val variance:RealVarParameter = new RealVariableParameter(1.0)) extends RealVariable with GeneratedVariable {
  mean.addChild(this)(null)
  variance.addChild(this)(null)
  def parents = List(mean, variance)
  override def logpr: Double = {
    val x = this.doubleValue
    val diff = x - mean.doubleValue
    return - diff * diff / (2 * variance.doubleValue) - 0.5 * Math.log(2 * Math.Pi * variance.doubleValue)
  }
  def pr: Double = Math.exp(logpr)
  def sampleFrom(mean:RealVar, variance:RealVar)(implicit d:DiffList) = 
    set(Maths.nextGaussian(mean.doubleValue, variance.doubleValue)(Global.random))
  def sample(implicit d:DiffList): Unit = sampleFrom(mean, variance)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(mean:RealVar, variance:RealVar) => sampleFrom(mean, variance)
  }

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
  override def toString = "Gaussian("+mean.doubleValue+","+variance.doubleValue+")"
}

// /** A real value, integrated out with a Gaussian prior. */
// class GaussianReal[R<:GeneratedRealVariable[R]] extends GeneratedRealVariable[R] {
//   private var evidenceSum = 0.0
//   private var evidenceNormalizer = 0.0
//   def increment(e:Double): Unit = { evidenceSum += e; evidenceNormalizer += 1.0 }
//   def decrement(e:Double): Unit = { evidenceSum -= e; evidenceNormalizer -= 1.0 }
// }

// // TODO Make WishartGaussian also.
// /** A one-dimensional Gaussian, with integrated out mean, having a Gaussian prior. */
// class GaussianGaussian1[R<:GeneratedRealVar[R]](override val variance:RealVariable) extends Gaussian1[R](new Real(0.0), variance) {
//   // The evidence
//   private val meanSum: Double = 0.0
//   private val meanTotal: Double = 0.0
//   override def preChange(o:R)(implicit d:DiffList) = {
//     o.generativeSource match {
//       case mixture:MarginalizedMixtureChoice[_] => {
//         val index = this.asInstanceOf[MixtureComponent].index
//         increment(index, -mixture.multinomial(index))
//       }
//       case _ => increment(o.index, -1.0)
//     }
//   }
//   override def postChange(o:R)(implicit d:DiffList) = {
//     o.generativeSource match {
//       case mixture:MarginalizedMixtureChoice[_,] =>
//         for (i <- 0 until o.domain.size) increment(i, mixture.multinomial(i))
//       case _ => increment(o.index, 1.0)
//     }
//   }
// }
