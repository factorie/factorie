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
import cc.factorie.maths._

/**
 * Maximize the Optimizable object by line search on successive conjugate gradients,
 * in the Polak and Ribiere version,
 * as described in "Numeric Recipes in C", Section 10.6.
 * @author Andrew McCallum
 * @author Gregory Druck
 *
 */

class ConjugateGradient(val optimizable: OptimizableByValueAndGradient, initialStepSize: Double = 1.0) extends Optimizer with FastLogging {
  var isConverged = false
  var lineOptimizer = new BackTrackLineOptimizer(optimizable)
  var tolerance = 0.0001
  var gradientTolerance = 0.001
  var maxIterations = 1000
  // "eps" is a small number to recitify the special case of converging to exactly zero function value
  val eps = 1.0e-10

  // The state of a conjugate gradient search
  var fp = 0.0;
  var gg = 0.0;
  var gam = 0.0;
  var dgg = 0.0;
  var step = 0.0;
  var fret = 0.0
  var xi: Array[Double] = null;
  var g: Array[Double] = null;
  var h: Array[Double] = null
  var iterations = 0

  def reset(): Unit = xi = null

  def partialReset() {
    fp = optimizable.optimizableValue
    xi = new Array[Double](optimizable.numOptimizableParameters)
    optimizable.getOptimizableGradient(xi)
    g = maths.copy(xi)
    h = maths.copy(xi)
    step = initialStepSize
    iterations = 0
  }

  def optimize(numIterations: Int = maxIterations): Boolean = {
    if (isConverged) return true;

    if (xi == null) partialReset()

    for (iterationCount <- 0 until numIterations) {
      logger.info("ConjugateGradient: At iteration " + iterations + ", cost = " + fp);

      // take a step in the current search direction
      step = lineOptimizer.optimize(xi, step);

      // re-compute value and gradient
      fret = optimizable.optimizableValue
      optimizable.getOptimizableGradient(xi)

      // This termination provided by "Numeric Recipes in C".
      if (2.0 * math.abs(fret - fp) <= tolerance * (math.abs(fret) + math.abs(fp) + eps)) {
        logger.info("ConjugateGradient converged: old value= " + fp
          + " new value= " + fret + " tolerance=" + tolerance);
        isConverged = true;
        return true;
      }
      fp = fret;

      // This termination provided by McCallum
      if (maths.twoNorm(xi) < gradientTolerance) {
        logger.info("ConjugateGradient converged: maximum gradient component "
          + maths.twoNorm(xi) + ", less than " + tolerance)
        isConverged = true;
        return true;
      }

      dgg = 0.0;
      gg = 0.0
      forIndex(xi.length)(j => {
        // prev gradient
        gg += g(j) * g(j)
        // curr gradient
        //dgg += xi(j) * xi(j)
        dgg += xi(j) * (xi(j) - g(j))
      })

      // compute gamma
      gam = dgg / gg

      forIndex(xi.length)(j => {
        g(j) = xi(j)
        // here g(j) is the current gradient
        // and h(j) is the previous search direction
        h(j) = g(j) + gam * h(j)
      })
      assert(!maths.isNaN(h))

      // gdruck
      // If using the BackTrackLineSearch, then the search stops whenever
      // a step is found that increases the value significantly (according
      // to a threshold from Numerical Recipes).  ConjugateGradient
      // assumes that line maximization finds something close
      // to the maximum in that direction.  In tests, sometimes the
      // direction suggested by CG points downhill.  Consequently, here I am
      // setting the search direction to the gradient if the slope is
      // negative or 0.
      // TODO Implement GradientBracketLineMaximizer (used in Numerical Recipes)
      // which should avoid this problem!
      if ((xi dot h) > 0) {
        maths.set(xi, h)
      }
      else {
        maths.set(h, xi)
      }

      iterations += 1
      if (iterations > maxIterations) {
        logger.warn("Too many iterations in ConjugateGradient.java")
        isConverged = true
        return true
      }
    }
    false
  }
}
