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
package cc.factorie.optimize

import cc.factorie.la.{DenseTensor, SparseIndexedTensor, Tensor}
import cc.factorie.model.{WeightsMap, WeightsSet}

// TODO this should really store weights in log-space, and have an unnormalized version

/**
 * This implements the Exponentiated Gradient algorithm of Kivinen and Warmuth
 * - also known as Entropic Mirror Descent (Beck and Teboulle)
 * @param rate The base learning rate
 */
class ExponentiatedGradient(rate: Double = 1.0) extends GradientOptimizer {
  private var initialized = false

  def initializeWeights(weights: WeightsSet) = MutableScalableWeights.initializeWeights(weights)
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) {
      initializeWeights(weights)
      val len = weights.length * 1.0
      weights.tensors.foreach(t => t += Array.fill(t.length)(1 / len)) // need to initialize it to lie on the simplex
      initialized = true
    }
    var newWeightsSum = 1.0
    for (template <- gradient.keys)
      (weights(template), gradient(template)) match {
        case (weights: Tensor, gradient: DenseTensor) =>
          val gArr = gradient.asArray
          val len = gradient.length
          var i = 0
          while (i < len) {
            val oldWeight = weights(i)
            newWeightsSum -= oldWeight
            val expGrad = math.exp(rate * gArr(i))
            val newWeight = oldWeight * expGrad
            weights(i) = newWeight
            newWeightsSum += newWeight
            i += 1
          }
        case (weights: Tensor, gradient: SparseIndexedTensor) =>
          val len = gradient.activeDomainSize
          val indices = gradient._indices
          val values = gradient._values
          var i = 0
          while (i < len) {
            val idx = indices(i)
            val oldWeight = weights(idx)
            newWeightsSum -= oldWeight
            val expGrad = math.exp(rate * values(i))
            val newWeight = oldWeight * expGrad
            weights(idx) = newWeight
            newWeightsSum += newWeight
            i += 1
          }
      }
    weights *= (1 / newWeightsSum)
    println(weights.oneNorm)
  }
  def isConverged: Boolean = false
  def reset(): Unit = {
    initialized = false
  }
}

// NOTE: this includes an annealed learning rate schedule at rate 1/sqrt(k) -luke
// This also uses sum-log-probs and so should have better numerical stability than the regular exponentiated gradient optimizer
class DenseExponentiatedGradient(rate: Double = 1.0, annealRate: Boolean = true) extends GradientOptimizer {
  var initialized = false
  var iter = 0
  def initializeWeights(weights: WeightsSet) =
    if (!initialized) {
      val len = weights.length * 1.0
      weights.tensors.foreach(t => t := Array.fill(t.length)(1 / len)) // need to initialize it to lie on the simplex
      initialized = true
    } else
      sys.error("already initialized")
  def finalizeWeights(weights: WeightsSet) = { }

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) initializeWeights(weights)
    iter += 1
    val mult = if (annealRate) rate / math.sqrt(iter) else rate
    for (template <- gradient.keys)
      (weights(template), gradient(template)) match {
        case (weights: Tensor, gradient: DenseTensor) =>
          val wArr = weights.asArray
          val gArr = gradient.asArray
          val len = weights.length
          var i = 0
          while (i < len) {
            wArr(i) = math.log(wArr(i)) + gArr(i) * mult
            i += 1
          }
          val logZ = cc.factorie.maths.sumLogProbs(weights.asArray)
          i = 0
          while (i < len) {
            wArr(i) = math.exp(wArr(i) - logZ)
            i += 1
          }
      }
  }
  def isConverged: Boolean = false
  def reset(): Unit = {
    initialized = false
    iter = 0
  }
}