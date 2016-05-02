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

import scala.collection.mutable.ArrayBuffer

// TODO What kind of regularization would be used with LBFGS other than L2?
// If nothing, then incorporate it directly into LBFGS. -akm

/** A quasi-Newton batch gradient optimizer.
    Limited-memory BFGS, as described in Byrd, Nocedal, and Schnabel, "Representations of Quasi-Newton Matrices and Their Use in Limited Memory Methods" */
class LBFGS(var numIterations: Double = 1000,
            var maxIterations: Int = 1000,
            var tolerance: Double = 0.0001,
            var gradientTolerance : Double= 0.001,
            val eps : Double = 1.0e-5,
            val rankOfApproximation : Int =  4,
            val initialStepSize : Double = 1.0) extends GradientOptimizer with FastLogging {
  private var _isConverged = false
  def isConverged = _isConverged

  case class StepTooSmallException(msg:String) extends Exception(msg)

  var lineMaximizer: BackTrackLineOptimizer = null

  // The number of corrections used in BFGS update
  // ideally 3 <= m <= 7. Larger m means more cpu time, memory.

  // State of search
  // g = gradient
  // s = list of m previous "parameters" values
  // y = list of m previous "g" values
  // rho = intermediate calculation
  var g: WeightsMap = null
  var oldg: WeightsMap = null
  var direction: WeightsMap = null
  var params: WeightsSet = null
  var oldParams: WeightsMap = null
  var s: ArrayBuffer[WeightsMap] = null
  var y: ArrayBuffer[WeightsMap] = null
  var rho: ArrayBuffer[Double] = null
  var alpha: Array[Double] = null
  var step = 1.0
  var iterations: Int = 0
  var oldValue: Double  = Double.NegativeInfinity

  // override to evaluate on dev set, save the intermediate model, etc.
  def postIteration(iter: Int): Unit = ()

  def reset(): Unit = {
    _isConverged = false
    step = 1.0
    iterations = 0
    oldValue = Double.NegativeInfinity
    g = null
    s = null
    y = null
    rho = null
    alpha = null
    params = null
    oldParams = null
    direction = null
    oldg = null

  }

  def initializeWeights(weights: WeightsSet): Unit = { }
  def finalizeWeights(weights: WeightsSet): Unit = { }

  def step(weights:WeightsSet, gradient:WeightsMap, value:Double): Unit = {
    if (_isConverged) return
    //todo: is the right behavior to set _isConverged = true if exceeded numIters?
    if (iterations > numIterations) { logger.warn("LBFGS: Failed to converge: too many iterations"); _isConverged = true; return }

    //if first time in, initialize
    if (g == null) {
      logger.debug("LBFGS: Initial value = " + value)

      iterations = 0
      s = new ArrayBuffer[WeightsMap]
      y = new ArrayBuffer[WeightsMap]
      rho = new ArrayBuffer[Double]
      alpha = new Array[Double](rankOfApproximation)

      params = weights
      oldParams = params.copy
      //use copy to get the right size
      g = gradient
      oldg = gradient.copy
      direction = gradient.copy

      if (direction.twoNorm == 0) {
        logger.info("LBFGS: Initial initial gradient is zero; saying converged")
        g = null
        _isConverged = true
        //return true;
      }
      direction.*=(1.0 / direction.twoNorm)

      // take a step in the direction
      lineMaximizer = new BackTrackLineOptimizer(gradient, direction, initialStepSize)
      lineMaximizer.step(weights, gradient, value)

      //todo: change this to just check if lineOptimizer has converged
      //      if (step == 0.0) {
      //        // could not step in this direction
      //        // give up and say converged
      //        g = null // reset search
      //        step = 1.0
      //        logger.error("Line search could not step in the current direction. " +
      //                "(This is not necessarily cause for alarm. Sometimes this happens close to the maximum," +
      //                " where the function may be very flat.)")
      //        //throw new StepTooSmallException("Line search could not step in current direction.")
      //        return false
      //      }
      oldValue = value
    }else if(!lineMaximizer.isConverged){
      lineMaximizer.step(weights, gradient, value)
    }
    //else{
    if (lineMaximizer.isConverged) {
      //first, check for convergence:
      iterations += 1
      logger.debug("LBFGS: At iteration " + iterations + ", value = " + value)
      //params and g are just aliases for the names of the variables passed in
      g = gradient
      params = weights


      if (2.0 * math.abs(value - oldValue) <= tolerance * (math.abs(value) + math.abs(oldValue) + eps)) {
        logger.debug("LBFGS: Exiting on termination #1: value difference below tolerance (oldValue: " + oldValue + " newValue: " + value)
        _isConverged = true
        return
      }
      val gg = g.twoNorm
      if (gg < gradientTolerance) {
        logger.trace("LBFGS: Exiting on termination #2: gradient=" + gg + " < " + gradientTolerance)
        _isConverged = true
        return
      }

      if (gg == 0.0) {
        logger.trace("LBFGS: Exiting on termination #3: gradient==0.0")
        _isConverged = true
        return
      }
      logger.trace("Gradient = " + gg)
      iterations += 1
      if (iterations > maxIterations) {
        logger.warn("Too many iterations in L-BFGS.java. Continuing with current parameters.")
        _isConverged = true
        return
      }


      // get difference between previous 2 gradients and parameters
      var sy = 0.0
      var yy = 0.0
      //todo: these next check are quite inefficient, but is a hack to avoid doing the following line on tensors:
      //params(i).isInfinite && oldParams(i).isInfinite && (params(i) * oldParams(i) > 0)) 0.0

      if(!params.toArray.forall(d => !(d == Double.PositiveInfinity || d == Double.NegativeInfinity))) throw new IllegalStateException("Weight value can't be infinite")
      if(!gradient.toArray.forall(d => !(d == Double.PositiveInfinity || d == Double.NegativeInfinity))) throw new IllegalStateException("gradient value can't be infinite")

      oldParams = params - oldParams
      oldg = g - oldg
      sy = oldParams dot oldg
      yy = oldg.twoNormSquared
      direction := gradient

      if (sy > 0) throw new IllegalStateException("sy=" + sy + "> 0")
      val gamma = sy / yy // scaling factor
      if (gamma > 0) throw new IllegalStateException("gamma=" + gamma + "> 0")

      pushDbl(rho, 1.0 / sy)
      pushTensor(s, oldParams)
      pushTensor(y, oldg)

      // calculate new direction
      assert(s.size == y.size)


      for (i <- s.size -1 to 0 by -1) {
        // alpha(i) = rho(i) * ArrayOps.dot(direction, s(i))
        alpha(i) = rho(i) *  (direction dot s(i))

        // ArrayOps.incr(direction, y(i), -1.0 * alpha(i))
        direction.+=(y(i),-1.0 * alpha(i))

      }
      direction.*=(gamma)

      for (i <- 0 until s.size) {
        //val beta = rho(i) * ArrayOps.dot(direction, y(i))
        val beta = rho(i) * (direction dot y(i))
        //ArrayOps.incr(direction, s(i), alpha(i) - beta)
        direction.+=(s(i),alpha(i) - beta)
      }

      oldParams := params
      oldValue = value
      oldg := g
      direction.*=(-1)
      lineMaximizer = null
      postIteration(iterations)

      lineMaximizer = new BackTrackLineOptimizer(gradient, direction, initialStepSize)
      lineMaximizer.step(weights, gradient, value)


    }


  }
  def pushTensor(l: ArrayBuffer[WeightsMap], toadd: WeightsMap): Unit = {
    assert(l.size <= rankOfApproximation)

    if (l.size == rankOfApproximation) {
      l.remove(0)
      l += toadd.copy
      //todo: change back to this circular thing below
      //      val last = l(0)
      //      Array.copy(toadd, 0, last, 0, toadd.length)
      //      forIndex(l.size - 1)(i => {l(i) = l(i + 1)})
      //      l(m - 1) = last
    } else {
      l += toadd.copy
    }
  }

  def pushDbl(l: ArrayBuffer[Double], toadd: Double): Unit = {
    assert(l.size <= rankOfApproximation)
    if (l.size == rankOfApproximation) l.remove(0)
    l += toadd
  }
}

//class L2RegularizedLBFGS(var l2: Double = 0.1) extends LBFGS {
//  override def step(weightsSet: Tensor, gradient: Tensor, value: Double, margin: Double) {
//    gradient += (weightsSet, -l2)
//    super.step(weightsSet, gradient, value - l2 * (weightsSet dot weightsSet), margin)
//  }
//}