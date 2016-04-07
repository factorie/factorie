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
import cc.factorie.util.FastLogging

/**
 * A conjugate gradient optimizer. Should not be used unless you know you want it because LBFGS is often better.
 * @param initialStepSize The initial step size. Not too important because line search is performed.
 * @author Andrew McCallum, Alexandre Passos
 */
class ConjugateGradient(val initialStepSize: Double = 1.0) extends GradientOptimizer with FastLogging {
  private var _isConverged = false
  def isConverged = _isConverged
   
  var tolerance = 0.0001
  var gradientTolerance = 0.001
  var maxIterations = 1000
  val eps = 1.0e-10 // a small number to rectify the special case of converging to exactly zero function value

  // The state of a conjugate gradient search
  //var fp = 0.0
  var oldValue = 0.0
  var gg = 0.0
  var gam = 0.0
  var dgg = 0.0
  var stepSize = 0.0
  var xi: WeightsMap = null
  var g: WeightsMap = null
  var h: WeightsMap = null
  var iterations = 0
  var lineOptimizer: BackTrackLineOptimizer = null

  def reset(): Unit = {
    xi = null
    _isConverged = false
  }

  def initializeWeights(weights: WeightsSet): Unit = { }
  def finalizeWeights(weights: WeightsSet): Unit = { }

  def step(weights:WeightsSet, gradient:WeightsMap, value:Double): Unit = {
    if (_isConverged) return
    
    // If this is our first time in, then initialize
    if (xi eq null) {
      xi = gradient.copy
      g = xi.copy
      h = xi.copy
      stepSize = initialStepSize
    }
    
    // Take a step in the current search direction, xi
    if (lineOptimizer eq null) lineOptimizer = new BackTrackLineOptimizer(gradient, xi.copy, stepSize)
    lineOptimizer.step(weights, xi, value)
    // If the lineOptimizer has not yet converged, then don't yet do any of the ConjugateGradient-specific things below
    if (lineOptimizer.isConverged){
    lineOptimizer = null // So we create a new one next time around
    xi = gradient.copy
    // This termination provided by "Numeric Recipes in C".
    if (2.0 * math.abs(value - oldValue) <= tolerance * (math.abs(value) + math.abs(oldValue) + eps)) {
      logger.info("ConjugateGradient converged: old value="+oldValue+" new value="+value+" tolerance="+tolerance)
      _isConverged = true
      return
    }
    // This termination provided by McCallum
    if (xi.twoNorm < gradientTolerance) {
      logger.info("ConjugateGradient converged: maximum gradient component: "+xi.twoNorm+" less than "+tolerance)
      _isConverged = true
      return
    }

    oldValue = value

    // compute gamma, new g and new h
    {
      dgg = 0.0
      gg = 0.0
      val xia = xi.toArray
      val ga = g.toArray
      var i = 0
      while (i < ga.length) {
        gg += ga(i) * ga(i) // previous gradient
        dgg += xia(i) * (xia(i) - ga(i)) // current gradient
        i += 1
      }
      gam = dgg / gg
      g.keys.foreach(k => g(k) := xi(k))
      h.keys.foreach(k => h(k) *= gam)
      h += g
      assert(!h.containsNaN())
    }
    
    /* gdruck: If using the BackTrackLineSearch, then the search stops whenever
       a step is found that increases the value significantly (according
       to a threshold from Numerical Recipes).  ConjugateGradient
       assumes that line maximization finds something close
       to the maximum in that direction.  In tests, sometimes the
       direction suggested by CG points downhill.  Consequently, here I am
       setting the search direction to the gradient if the slope is
       negative or 0. */
    // TODO Implement GradientBracketLineMaximizer (used in Numerical Recipes) which should avoid this problem!
    if (xi.dot(h) > 0) xi := h  else h := xi

    iterations += 1

    lineOptimizer = new BackTrackLineOptimizer(gradient, xi.copy, stepSize)
    lineOptimizer.step(weights, xi, value)
  }
  }
}