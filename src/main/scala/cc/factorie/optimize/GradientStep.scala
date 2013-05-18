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

package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._

/** Change the weightsSet in the direction of the gradient by a factor of "rate" for each step. */
trait GradientStep extends GradientOptimizer {
  var it = 0
  def processGradient(gradient: TensorSet, weights: WeightsSet): Unit = {}
  def lRate(gradient: TensorSet, weights: WeightsSet, value: Double): Double = 1.0
  def doGradStep(weights: WeightsSet, gradient: TensorSet, rate: Double): Unit = weights += (gradient, rate)
  def step(weights: WeightsSet, gradient: TensorSet, value: Double): Unit = {
    it += 1
    processGradient(gradient, weights)
    val rate = lRate(gradient, weights, value)
    doGradStep(weights, gradient, rate)
  }
  def isConverged = false
  // TODO What to put here?
  def reset(): Unit = { it = 0 }
}

trait MarginScaled extends GradientStep {
  val C: Double = 1.0
  override def lRate(gradient: TensorSet, weights: WeightsSet, value: Double) = math.max(-C, math.min(C, -value/(gradient.twoNormSquared)))
}

trait ParameterAveraging extends GradientStep {
  var wTmp: TensorSet = null
  override def doGradStep(weights: WeightsSet, gradient: TensorSet, rate: Double): Unit = {
    if (wTmp eq null) wTmp = weights.blankDenseCopy
    super.doGradStep(weights, gradient, rate)
    wTmp += (gradient, rate*it)
  }

  def setWeightsToAverage(weights: WeightsSet): Unit = weights += (wTmp,-1.0/it)
  def unSetWeightsToAverage(weights: WeightsSet): Unit = weights += (wTmp,1.0/it)
  override def reset(): Unit = { super.reset(); wTmp = null }
}

// This implements the AdaGrad algorithm (with Composite Mirror Descent update) from
// "Adaptive Subgradient Methods for Online Learning and Stochastic Optimization" by Duchi et al.
trait AdaptiveLearningRate extends GradientStep {
  val rate: Double = 1.0
  val delta: Double = 0.1
  private var HSq: TensorSet = null
  var printed = false
  override def processGradient(gradient: TensorSet, weights: WeightsSet): Unit = {
    val eta = rate
//    val l2 = 0.1
//    gradient += (weightsSet, -l2)
    if (HSq == null) { HSq = weights.blankDenseCopy }
    for (template <- gradient.keys)
      (gradient(template), HSq(template)) match {
        case (g: DenseTensor, hSq: DenseTensor) =>
//          println(hSq)
          val gArr = g.asArray
          val hArr = hSq.asArray
          var i = 0
          val len = gArr.length
          while (i < len) {
            if (gArr(i) != 0) {
              hArr(i) += gArr(i) * gArr(i)
              val h = math.sqrt(hArr(i)) + delta
              val t1 = eta / h
              gArr(i) *= t1
            }
            i += 1
          }
        case (g: SparseIndexedTensor,  hSq: DenseTensor) =>
          val hArr = hSq.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            if (g != 0) {
              val idx = indices(i)
              hArr(idx) += g*g
              val h = math.sqrt(hArr(idx)) + delta
              val t1 = eta / h
              values(i) *= t1
            }
            i += 1
          }
        case (g: SparseIndexedTensor,  hSq: Tensor) =>
          if (!printed) {
            printed = true
            println("No implementations for: " + weights(template).getClass.getName + " " +
              gradient(template).getClass.getName +" " + HSq(template).getClass.getName)
          }
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            if (g != 0) {
              val idx = indices(i)
              hSq(idx) += g*g
              val h = math.sqrt(hSq(idx)) + delta
              val t1 = eta / h
              values(i) *= t1
            }
            i += 1
          }
      }
  }
  override def reset(): Unit = {
    super.reset()
    HSq = null
  }
}

trait InvSqrtTLearningRate extends GradientStep {
  val baseRate = 1.0
  override def lRate(gradient: TensorSet, weights: WeightsSet, value: Double): Double = baseRate/math.sqrt(it+1)
}

trait InvTLearningRate extends GradientStep {
  val baseRate = 1.0
  override def lRate(gradient: TensorSet, weights: WeightsSet, value: Double): Double = baseRate/(it+1)
}

class AdaGrad(override val rate: Double = 1.0, override val delta: Double = 0.1) extends AdaptiveLearningRate


trait ConstantLR extends GradientStep {
  val baseRate = 1.0
  override def lRate(gradient: TensorSet, weights: WeightsSet, value: Double): Double = baseRate
}

class ConstantLearningRate(override val baseRate: Double = 1.0) extends ConstantLR

class MIRA(override val C: Double = 1.0) extends MarginScaled

class AdaMira(override val rate: Double, override val delta: Double = 0.1, override val C: Double = 1.0) extends AdaptiveLearningRate with MarginScaled

class Perceptron extends ConstantLearningRate

class AveragedPerceptron extends ConstantLearningRate with ParameterAveraging
