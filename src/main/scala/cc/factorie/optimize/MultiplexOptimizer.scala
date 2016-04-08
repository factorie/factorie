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

import cc.factorie.model.{Weights, WeightsMap, WeightsSet}

import scala.collection.mutable

class MultiplexOptimizer(optimizers: Seq[GradientOptimizer], mapper: Weights => GradientOptimizer) extends GradientOptimizer {
  var initialized = false
  val optimizersToWeightsSets = new mutable.HashMap[GradientOptimizer, WeightsSet]
  override def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) initializeWeights(weights)
    for (opt <- optimizers) {
      val grad = gradient.filter(w => mapper(w) == opt)
      if (grad.length > 0)
        opt.step(optimizersToWeightsSets(opt), grad, value)
    }
  }
  override def initializeWeights(weights: WeightsSet): Unit = this.synchronized {
    if (!initialized) {
      initialized = true
      for (opt <- optimizers) optimizersToWeightsSets(opt) = weights.filter(w => mapper(w) == opt)
      for (opt <- optimizers) opt.initializeWeights(optimizersToWeightsSets(opt))
    } else
      sys.error("already initialized")
  }
  override def reset(): Unit = this.synchronized {
    initialized = false
    optimizersToWeightsSets.clear()
    optimizers.foreach(_.reset())
  }
  override def finalizeWeights(weights: WeightsSet): Unit = this.synchronized {
    if (initialized) {
      for (opt <- optimizers) opt.finalizeWeights(optimizersToWeightsSets(opt))
      optimizersToWeightsSets.clear()
    } else
      sys.error("not initialized")
  }
  override def isConverged: Boolean = optimizers.forall(_.isConverged)
}
