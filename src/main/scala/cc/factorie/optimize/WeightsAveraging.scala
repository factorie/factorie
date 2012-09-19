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

/** Keeps an average of all weight settings throughout steps. 
    To get "average perceptron" use "new WeightsAveraging(new StepwiseGradientAscent)" */
class WeightsAveraging(val inner:GradientOptimizer) extends GradientOptimizer {
  var weightsSum: Tensor = null
  var normalizer = 0.0
  def reset(): Unit = {
    weightsSum = null
    normalizer = 0.0
    inner.reset()
  }
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    if (weightsSum eq null) weightsSum = weights.copy
    else weightsSum += weights // TODO  Yipes, this is not sparse, not efficient
    normalizer += 1.0
    inner.step(weights, gradient, value, margin)
  }
  def averageWeights: Tensor = weightsSum / normalizer 
  def isConverged: Boolean = inner.isConverged
}







