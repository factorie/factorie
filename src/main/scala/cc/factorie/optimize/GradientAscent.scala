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
import scala.collection.mutable.IndexedSeq
import cc.factorie.maths._

/**Maximizes an Optimizable object by successive linear searches in gradient directions.
@author Andrew McCallum */
class GradientAscent(val optimizable: OptimizableByValueAndGradient) extends Optimizer {
  var isConverged = false
  var maxStep = 1.0
  var tolerance = 0.001
  var eps = 1.0e-10
  var gradientNormMax = 100.0
  var initialStepSize = 0.2
  var step = initialStepSize

  val lineMaximizer = new BackTrackLineOptimizer(optimizable)

  def optimize(numIterations: Int = Int.MaxValue): Boolean = {
    var value = optimizable.optimizableValue
    var gradient = new Array[Double](optimizable.numOptimizableParameters)
    optimizable.getOptimizableGradient(gradient)
    for (iteration <- 0 until numIterations) {
      // Ensure step size not to large
      val sum = gradient.twoNorm
      if (sum > gradientNormMax) gradient *= (gradientNormMax / sum)
      step = lineMaximizer.optimize(gradient, step)
      val newValue = optimizable.optimizableValue
      optimizable.getOptimizableGradient(gradient)
      println("objective=" + newValue)
      if (2.0 * math.abs(newValue - value) < tolerance * (math.abs(newValue) + math.abs(value) + eps) ||
        ArrayOps.twoNorm(gradient) == 0) {
        isConverged = true
        return true
      }
      value = newValue

    }
    return false
  }

}
