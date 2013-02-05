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

/** Change the weights in the direction of the gradient by a factor of "rate" for each step. */
class StepwiseGradientAscent(var rate: Double = 1.0) extends GradientOptimizer {
  def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    weights.+=(gradient, rate)
    rate = nextRate(rate)
  }
  def nextRate(oldRate: Double): Double = oldRate
  // TODO What should go here?
  def isConverged = false
  // TODO What to put here?
  def reset(): Unit = {}
}

// This implements the AdaGrad algorithm (with Composite Mirror Descent update) from
// "Adaptive Subgradient Methods for Online Learning and Stochastic Optimization" by Duchi et al.
class AdaGrad(/*l1: Double = 0.0,*/ rate: Double = 10.0, delta: Double = 0.1) extends GradientOptimizer {
  var HSq: Tensor = null
  def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    val eta = rate
//    val l2 = 0.1
//    gradient += (weights, -l2)
    if (HSq == null) { HSq = weights.blankCopy }
    for (template <- gradient.asInstanceOf[WeightsTensor].families)
      (weights.asInstanceOf[WeightsTensor](template),
       gradient.asInstanceOf[WeightsTensor](template),
       HSq.asInstanceOf[WeightsTensor](template)) match {
        case (w: DenseTensor, g: DenseTensor, hSq: DenseTensor) =>
//          println(hSq)
          val wArr = w.asArray
          val gArr = g.asArray
          val hArr = hSq.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            hArr(i) += gArr(i) * gArr(i)
            val h = math.sqrt(hArr(i)) + delta
            val t1 = eta / h
            val t2 = wArr(i) + t1 * gArr(i)
//            val t3 = l1 * eta / h
//            wArr(i) = math.signum(t2) * math.max(0, math.abs(t2) - t3)
            wArr(i) = t2
            i += 1
          }
        case (w: DenseTensor, g: SparseIndexedTensor,  hSq: DenseTensor) =>
          val wArr = w.asArray
          val hArr = hSq.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            hArr(idx) += g*g
            val h = math.sqrt(hArr(idx)) + delta
            val t1 = eta / h
            val t2 = wArr(idx) + t1 * g
            wArr(idx) = t2
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor,  hSq: Tensor) =>
          println("No implementations for: " + weights.asInstanceOf[WeightsTensor](template).getClass.getName + " " +
                  gradient.asInstanceOf[WeightsTensor](template).getClass.getName +" " + HSq.asInstanceOf[WeightsTensor](template).getClass.getName)
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            hSq(idx) += g*g
            val h = math.sqrt(hSq(idx)) + delta
            val t1 = eta / h
            val t2 = w(idx) + t1 * g
            w(idx) = t2
            i += 1
          }
      }
  }
  def reset(): Unit = {
    HSq = null
  }
  def isConverged: Boolean = false
}

// This implements the AdaGrad algorithm with primal-dual updates and support for l1 regularization
class AdaGradDualAveraging(var l1: Double = 0.0, var rate: Double = 10.0, var delta: Double = 0.1) extends GradientOptimizer {
  var HSq: Tensor = null
  var sumGs: Tensor = null
  var t = 0

  @inline final def truncate(x0: Double, l1: Double): Double = {
    if (x0 > l1)
      x0-l1
    else if (x0 < -l1)
      x0+l1
    else 0.0
  }

  def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    val eta = rate
    t += 1
    if (HSq == null) { HSq = weights.blankCopy }
    if (sumGs == null) { sumGs = weights.blankCopy }
    for (template <- gradient.asInstanceOf[WeightsTensor].families)
      (weights.asInstanceOf[WeightsTensor](template),
        gradient.asInstanceOf[WeightsTensor](template),
        HSq.asInstanceOf[WeightsTensor](template),
        sumGs.asInstanceOf[WeightsTensor](template)) match {
        case (w: DenseTensor, g: DenseTensor, hSq: DenseTensor, sGs: DenseTensor) =>
          val wArr = w.asArray
          val gArr = g.asArray
          val hArr = hSq.asArray
          val sgArr = sGs.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            hArr(i) += gArr(i) * gArr(i)
            sgArr(i) += gArr(i)
            val h = math.sqrt(hArr(i)) + delta
            val t1 = eta / h
            val t2 = t1 * truncate(sgArr(i), t*l1)
            wArr(i) = t2
            i += 1
          }
        case (w: DenseTensor, g: SparseIndexedTensor, hSq: DenseTensor, sGs: DenseTensor) =>
          val wArr = w.asArray
          val hArr = hSq.asArray
          val sgArr = sGs.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            hArr(idx) += g*g
            sgArr(idx) += g
            val h = math.sqrt(hArr(idx)) + delta
            val t1 = eta / h
            val t2 = t1 * truncate(sgArr(idx), t*l1)
            wArr(idx) = t2
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor, hSq: Tensor, sGs: Tensor) =>
          println("No implementations for: " + weights.asInstanceOf[WeightsTensor](template).getClass.getName + " " +
            gradient.asInstanceOf[WeightsTensor](template).getClass.getName +" " + HSq.asInstanceOf[WeightsTensor](template).getClass.getName)
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            hSq(idx) += g*g
            sGs(idx) += g
            val h = math.sqrt(hSq(idx)) + delta
            val t1 = eta / h
            val t2 = t1 * truncate(sGs(idx), t*l1)
            w(idx) = t2
            i += 1
            }
      }
  }
  def reset(): Unit = {
    HSq = null
    sumGs = null
  }
  def isConverged: Boolean = false
}


// This implements the Pegasos algorithm from "Pegasos: Primal Estimated sub-GrAdient SOlver for SVM" by Shalev-Shwartz et al.
class L2ProjectedGradientAscent(l2: Double = 0.1, k: Int = 1, rate: Double = 1.0) extends GradientOptimizer {
  private var step = 1
  def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    if (step == 1) {
      // make sure weights start off with ||w|| <= 1 / sqrt(rate)
      val weightsNorm = weights.twoNorm
      if (weightsNorm > 1 / math.sqrt(l2))
        weights /= (weightsNorm * math.sqrt(l2))
    }
    val eta_t = rate / (l2 * step)
    weights *= (1 - eta_t * l2)
    weights += (gradient, eta_t / k)
    val projCoeff = math.min(1, (1 / math.sqrt(l2)) / weights.twoNorm)
    weights *= projCoeff
    step += 1
  }
  def isConverged = false
  def reset(): Unit = {
    step = 1
  }
}

// Note: This implementation is slower than it should be in the online case, but should be fast enough in batch mode
class L2RegularizedGradientAscent(var l2: Double = 0.1, rate: Double = 1.0) extends StepwiseGradientAscent(rate) {
  override def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    gradient +=(weights, -l2)
    super.step(weights, gradient, value, margin)
  }
}

class SparseL2RegularizedGradientAscent(var l2: Double = 0.1, rate: Double = 1.0) extends StepwiseGradientAscent(rate) {
  override def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    super.step(weights, gradient, value, margin)
    weights *= (1.0 - rate * l2)
  }
}

/** Change the weights in the direction of the gradient by using back-tracking line search to make sure we step up hill. */
class LineSearchGradientAscent(var stepSize: Double = 1.0) extends GradientOptimizer with FastLogging {
  private var _isConverged = false
  def isConverged = _isConverged
  var gradientTolerance = 0.001
  var valueTolerance = 0.0001
  var gradientNormMax = 100.0
  var eps = 1.0e-10
  var oldValue = Double.NaN
  var lineOptimizer: BackTrackLineOptimizer = null
  def reset(): Unit = {
    _isConverged = false
    oldValue = Double.NaN
  }
  def step(weights: Tensor, gradient: Tensor, value: Double, margin: Double): Unit = {
    if (_isConverged) return
    // Check for convergence by value
    if (2.0 * math.abs(value - oldValue) < valueTolerance * (math.abs(value) + math.abs(oldValue) + eps)) {
      logger.info("GradientAscent converged: old value=" + oldValue + " new value=" + value + " tolerance=" + valueTolerance)
      _isConverged = true
      return
    }
    // Check for convergence by gradient
    val gradientTwoNorm = gradient.twoNorm
    if (gradientTwoNorm < gradientTolerance) {
      logger.info("GradientAscent converged: gradient twoNorm=" + gradient.twoNorm + " tolerance=" + gradientTolerance)
      _isConverged = true
      return
    }

    if (lineOptimizer eq null) {
      // Before giving the BackTrackLineOptimizer a line direction to search, ensure it isn't too steep
      // if (gradientTwoNorm > gradientNormMax) gradient.*=(gradientNormMax / gradientTwoNorm)
      lineOptimizer = new BackTrackLineOptimizer(gradient, gradient.copy, stepSize)
      oldValue = value
    }
    lineOptimizer.step(weights, gradient, value, margin)
    if (!lineOptimizer.isConverged) return
    lineOptimizer = null // So we create a new one next time
    lineOptimizer = new BackTrackLineOptimizer(gradient, gradient.copy, stepSize)
    lineOptimizer.step(weights, gradient, value, margin)
    oldValue = value
  }
}

