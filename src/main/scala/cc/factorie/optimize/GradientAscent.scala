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
trait GradientStep extends GradientOptimizer {
  var it = 0
  def processGradient(gradient: Tensors, weights: Tensors): Unit = {}
  def lRate(gradient: Tensors, weights: Tensors, value: Double): Double = 1.0
  def doGradStep(weights: Tensors, gradient: Tensors, rate: Double): Unit = weights += (gradient, rate)
  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
    it += 1
    processGradient(gradient, weights)
    val rate = lRate(gradient, weights, value)
    doGradStep(weights, gradient, rate)
  }
  def isConverged = false
  // TODO What to put here?
  def reset(): Unit = { it = 0 }
}

class StepwiseGradientAscent(var rate: Double = 1.0) extends GradientStep {
  override def lRate(gradient: Tensors, weights: Tensors, value: Double): Double = rate
}

class SimpleMIRA(var C: Double = 1.0) extends GradientStep {
  override def lRate(gradient: Tensors, weights: Tensors, value: Double) = -value/(C + gradient.twoNormSquared)
}

class MIRA(var C: Double = 1.0) extends GradientStep {
  override def lRate(gradient: Tensors, weights: Tensors, value: Double) = math.max(-C, math.min(C, -value/(gradient.twoNormSquared)))
}

trait ParameterAveraging extends GradientStep {
  var wTmp: Tensors = null
  override def doGradStep(weights: Tensors, gradient: Tensors, rate: Double): Unit = {
    if (wTmp eq null) wTmp = weights.blankDenseCopy
    super.doGradStep(weights, gradient, rate)
    wTmp += (gradient, rate*it)
  }

  def setWeightsToAverage(weights: Tensors): Unit = weights += (wTmp,-1.0/it)
  def unSetWeightsToAverage(weights: Tensors): Unit = weights += (wTmp,1.0/it)
  override def reset(): Unit = { super.reset(); wTmp = null }
}

class LazyL2ProjectedGD(var l2: Double = 0.0, rate: Double = 1.0) extends GradientOptimizer {
  var lastUpdate: Tensors = null
  var t = 0
  @inline final def learningRate(t: Double): Double = {
    rate / math.sqrt(t)
  }
  var printed = false

  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
    t += 1
    val eta = rate
    if (lastUpdate == null) { lastUpdate = weights.blankDenseCopy }
    for (template <- gradient.keys)
      (weights(template), gradient(template), lastUpdate(template)) match {
        case (w: DenseTensor, g: DenseTensor, lastUpdate: DenseTensor) =>
          val wArr = w.asArray
          val gArr = g.asArray
          val lastArr = lastUpdate.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            lastArr(i) += 1
            wArr(i) *= (1 - l2*learningRate(lastArr(i)))
            val t2 = wArr(i) + learningRate(lastArr(i)) * gArr(i)
            wArr(i) = t2
            i += 1
          }
        case (w: DenseTensor, g: SparseIndexedTensor,  lastUpdate: DenseTensor) =>
          val wArr = w.asArray
          val lastArr = lastUpdate.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            lastArr(idx) += 1
            wArr(idx) *= (1 - l2*learningRate(lastArr(idx)))
            val t2 = wArr(idx) + learningRate(lastArr(idx)) * g
            wArr(idx) = t2
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor,  lastUpdated: Tensor) =>
          if (!printed) {
            printed = true
            println("No implementations for: " + weights(template).getClass.getName + " " +
              gradient(template).getClass.getName +" " + lastUpdate(template).getClass.getName)
          }
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            lastUpdated(idx) += 1
            w(idx) *= (1 - l2*learningRate(lastUpdated(idx)))
            val t2 = w(idx) + learningRate(lastUpdated(idx)) * g
            w(idx) = t2
            i += 1
          }
      }
  }
  def reset(): Unit = {
    lastUpdate = null
  }
  def isConverged: Boolean = false
}

// This implements the AdaGrad algorithm (with Composite Mirror Descent update) from
// "Adaptive Subgradient Methods for Online Learning and Stochastic Optimization" by Duchi et al.
class AdaGrad(/*l1: Double = 0.0,*/ rate: Double = 1.0, delta: Double = 0.1) extends GradientStep {
  var HSq: Tensors = null
  var printed = false
  override def processGradient(gradient: Tensors, weights: Tensors): Unit = {
    val eta = rate
//    val l2 = 0.1
//    gradient += (weights, -l2)
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

object DualAveraging {
  @inline final def truncate(x0: Double, l1: Double): Double = {
    if (x0 > l1)
      x0-l1
    else if (x0 < -l1)
      x0+l1
    else 0.0
  }
}

// This implements the AdaGrad algorithm with primal-dual updates and support for l1 regularization
class AdaGradDualAveraging(var l1: Double = 0.0, var l2: Double = 0.0, var rate: Double = 1.0, var delta: Double = 0.1) extends GradientOptimizer {
  var HSq: Tensors = null
  var sumGs: Tensors = null
  var t = 0

  import DualAveraging.truncate
  var printed = false

  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
    val eta = rate
    t += 1
    if (HSq == null) { HSq = weights.blankDenseCopy }
    if (sumGs == null) { sumGs = weights.blankDenseCopy }
    for (template <- gradient.keys)
      (weights(template), gradient(template), HSq(template), sumGs(template)) match {
        case (w: DenseTensor, g: DenseTensor, hSq: DenseTensor, sGs: DenseTensor) =>
          val wArr = w.asArray
          val gArr = g.asArray
          val hArr = hSq.asArray
          val sgArr = sGs.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            if (gArr(i) != 0) {
              hArr(i) += gArr(i) * gArr(i)
              sgArr(i) += gArr(i)
              val h = (1.0/eta) *(math.sqrt(hArr(i)) + delta) + t*l2
              val t1 = 1.0/h
              val t2 = t1 * truncate(sgArr(i), t*l1)
              wArr(i) = t2
            }
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
            if (g != 0) {
              val idx = indices(i)
              hArr(idx) += g*g
              sgArr(idx) += g
              val h = (1.0/eta)*(math.sqrt(hArr(idx)) + delta) + t*l2
              val t1 = 1.0 / h
              val t2 = t1 * truncate(sgArr(idx), t*l1)
              wArr(idx) = t2
            }
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor, hSq: Tensor, sGs: Tensor) =>
          if (!printed) {
            printed = true
            println("AdaradDualAveaging: no implementations for: " + weights(template).getClass.getName + " " +
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
              sGs(idx) += g
              val h = (1.0/eta)*(math.sqrt(hSq(idx)) + delta) + t*l2
              val t1 = 1.0 / h
              val t2 = t1 * truncate(sGs(idx), t*l1)
              w(idx) = t2
            }
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
  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
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
    lineOptimizer.step(weights, gradient, value)
    if (!lineOptimizer.isConverged) return
    lineOptimizer = null // So we create a new one next time
    lineOptimizer = new BackTrackLineOptimizer(gradient, gradient.copy, stepSize)
    lineOptimizer.step(weights, gradient, value)
    oldValue = value
  }
}

