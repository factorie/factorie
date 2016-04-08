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

import cc.factorie.model.{WeightsMap, WeightsSet}

/**
 * Simple efficient l2-regularized SGD with a constant learning rate
 *
 * Note that we must have |rate * l2 / numExamples| < 1.0 or the weights will oscillate.
 *
 * @param l2 The l2 regularization parameter
 * @param rate The learning rate
 * @param numExamples The number of examples for online training, used to scale regularizer
 */
class L2RegularizedConstantRate(l2: Double = 0.1, rate: Double = 0.1, numExamples: Int = 1) extends GradientOptimizer {
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) initializeWeights(weights)
    weights += (gradient, rate)
    weights *= (1.0 - rate * l2 / numExamples)
  }

  def initializeWeights(weights: WeightsSet): Unit = {
    if (initialized) return
    MutableScalableWeights.initializeWeights(weights)
    initialized = true
  }
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  def isConverged = false
  def reset(): Unit = { }
}