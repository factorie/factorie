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

import cc.factorie.la._
import cc.factorie.model.{WeightsMap, WeightsSet}

/**
 * Base trait for optimizers whose operational form can be described as
 *    1. get a gradient
 *    2. do some transformation to it
 *    3. compute a learning rate
 *    4. add it to the weights
 *
 * Traits which extend this one can have things like parameter averaging or MIRA learning rates
 * or adaptive learning rates for free.
 */
trait GradientStep extends GradientOptimizer {
  var it = 0

  /**
   * Override this method do to some transformation to the gradient before going on with optimization
   * @param weights The weights
   * @param gradient The gradient
   */
  def processGradient(weights: WeightsSet, gradient: WeightsMap): Unit = {}

  /**
   * Override this method to change the learning rate
   * @param weights The weights
   * @param gradient The gradient
   * @param value The value
   * @return The learning rate
   */
  def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = 1.0

  /**
   * Actually adds the gradient to the weights. ParameterAveraging overrides this.
   * @param weights The weights
   * @param gradient The gradient
   * @param rate The learning rate
   */
  def doGradStep(weights: WeightsSet, gradient: WeightsMap, rate: Double): Unit = weights += (gradient, rate)

  /**
   * Should not be overriden. The main flow of a GradientStep optimizer.
   * @param weights The weights
   * @param gradient The gradient
   * @param value The value
   */
  final def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    it += 1
    processGradient(weights, gradient)
    val rate = lRate(weights, gradient, value)
    doGradStep(weights, gradient, rate)
  }

  /**
   * Online optimizers generally don't converge
   * @return Always false
   */
  def isConverged = false

  /**
   * To override if you want to reset internal state.
   */
  def reset(): Unit = { it = 0 }

  def initializeWeights(weights: WeightsSet): Unit = { }

  def finalizeWeights(weights: WeightsSet): Unit = { }
}

/**
 * Mixin trait to add parameter averaging to any GradientStep
 */
trait ParameterAveraging extends GradientStep {
  var wTmp: WeightsMap = null
  var isSetToAverage = false
  private def initWtmp(weights: WeightsSet): Unit = {
    wTmp = weights.blankDenseMap
    // need these two lines to de-sparsify the wTmp matrix so we don't have thread-safety issues
    wTmp += weights
    wTmp.zero()
  }
  override def doGradStep(weights: WeightsSet, gradient: WeightsMap, rate: Double): Unit = {
    super.doGradStep(weights, gradient, rate)
    if (wTmp == null) initWtmp(weights)
    wTmp += (gradient, rate*it)
  }
  def setWeightsToAverage(weights: WeightsSet): Unit = if (!isSetToAverage && (wTmp ne null)) {
    weights += (wTmp,-1.0/it)
    isSetToAverage = true
  }
  def unSetWeightsToAverage(weights: WeightsSet): Unit = if (isSetToAverage && (wTmp ne null)) {
    weights += (wTmp,1.0/it)
    isSetToAverage = false
  }
  override def reset(): Unit = { super.reset(); wTmp = null }
  override def initializeWeights(weights: WeightsSet): Unit = {
    super.initializeWeights(weights)
    if (wTmp == null) initWtmp(weights)
    unSetWeightsToAverage(weights)
  }
  override def finalizeWeights(weights: WeightsSet): Unit = {
    super.finalizeWeights(weights)
    setWeightsToAverage(weights)
  }
}

/**
 * This implements the adaptive learning rates from the AdaGrad algorithm
 * (with Composite Mirror Descent update) from "Adaptive Subgradient Methods for
 * Online Learning and Stochastic Optimization" by Duchi et al.
 *
 * Can be mixed into any GradientStep.
 */
trait AdaptiveLearningRate extends GradientStep {
  /**
   * The base learning rate
   */
  val rate: Double = 1.0
  /**
   * The learning rate decay factor.
   */
  val delta: Double = 0.1
  private var HSq: WeightsMap = null
  var printed = false
  private def initHSq(weights: WeightsSet): Unit = {
    HSq = weights.blankDenseMap
    // need these two lines to de-sparsify the H^2 matrix so we don't have thread-safety issues
    HSq += weights
    HSq.zero()
  }
  override def initializeWeights(weights: WeightsSet): Unit = {
    super.initializeWeights(weights)
    if (HSq == null) initHSq(weights)
  }
  override def reset(): Unit = {
    super.reset()
    HSq = null
  }
  override def processGradient(weights: WeightsSet, gradient: WeightsMap): Unit = {
    val eta = rate
//    val l2 = 0.1
//    gradient += (weightsSet, -l2)
    if (HSq == null) initHSq(weights)
    for (template <- gradient.keys) {
      gradient(template) match {
        case t: Outer1Tensor2 if t.tensor1.isDense && t.tensor2.isDense =>
          gradient(template) = new DenseTensor2(t.dim1, t.dim2)
          gradient(template) += t
        case t: Outer1Tensor2 =>
          gradient(template) = new SparseIndexedTensor2(t.dim1, t.dim2)
          gradient(template) += t
        case t: SparseBinaryTensor1 =>
          gradient(template) = new SparseIndexedTensor1(t.dim1)
          gradient(template) += t
        case t: SparseBinaryTensor2 =>
          gradient(template) = new SparseIndexedTensor2(t.dim1, t.dim2)
          gradient(template) += t
        case t: SparseBinaryTensor3 =>
          gradient(template) = new SparseIndexedTensor3(t.dim1, t.dim2, t.dim3)
          gradient(template) += t
        case t: SparseBinaryTensor4 =>
          gradient(template) = new SparseIndexedTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
          gradient(template) += t
        case _ =>
      }
    }
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
//              assert(!gArr(i).isNaN)
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
//              assert(!values(i).isNaN)
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
//              assert(!values(i).isNaN)
            }
            i += 1
          }
      }
  }
}

/**
 * Mixin trait for implementing a MIRA step
 */
trait MarginScaled extends GradientStep {
  val C: Double = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = {
    val sqNorm = gradient.twoNormSquared
    if (sqNorm == 0.0) 0.0 else math.max(0.0, math.min(C, -value/gradient.twoNormSquared))
  }
}

/**
 * Mixin trait for a step size which looks like 1/sqrt(T)
 */
trait InvSqrtTStepSize extends GradientStep {
  val baseRate = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate / math.sqrt(it + 1)
}

/**
 * Mixin trait for a step size which looks like 1/T
 */
trait InvTStepSize extends GradientStep {
  val baseRate = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate / (it + 1)
}

/**
 * Mixin trait for a constant step size
 */
trait ConstantStepSize extends GradientStep {
  val baseRate = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate
}

/**
 * Mixin trait for a step size which is normalized by the length of the gradient and looks like 1/sqrt(T)
 */
trait InvSqrtTLengthStepSize extends GradientStep {
  val baseRate = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate / (math.sqrt(it + 1) * gradient.twoNorm)
}

/**
 * Mixin trait for a step size which is normalized by the length of the gradient and looks like 1/T
 */
trait InvTLengthStepSize extends GradientStep {
  val baseRate = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate / ((it + 1) * gradient.twoNorm)
}

/**
 * Mixin trait for a step size which is normalized by the length of the gradient and is constant
 */
trait ConstantLengthStepSize extends GradientStep {
  val baseRate  = 1.0
  override def lRate(weights: WeightsSet, gradient: WeightsMap, value: Double): Double = baseRate / gradient.twoNorm
}

/**
 * The AdaGrad algorithm.
 *
 * Should be used with averaging for optimal performance.
 * @param rate The base learning rate. It's worth tuning it.
 * @param delta A decay factor. Not worth tuning.
 */
class AdaGrad(override val rate: Double = 1.0, override val delta: Double = 0.1) extends AdaptiveLearningRate

/**
 * A simple gradient descent algorithm with constant learning rate.
 * @param baseRate The learning rate
 */
class ConstantLearningRate(override val baseRate: Double = 1.0) extends ConstantStepSize

/**
 * A simple gradient descent algorithm with constant norm-independent learning rate.
 * @param baseRate The learning rate
 */
class ConstantLengthLearningRate(override val baseRate: Double = 1.0) extends ConstantLengthStepSize

/**
 * The MIRA algorithm
 * @param C The regularization constant. Doesn't really need tuning.
 */
class MIRA(override val C: Double = 1.0) extends MarginScaled

/**
 * The combination of AdaGrad with MIRA
 * @param rate See AdaGrad, but here it matters much less.
 * @param delta See AdaGrad, but again here it matters much less.
 * @param C See MIRA.
 */
class AdaMira(override val rate: Double, override val delta: Double = 0.1, override val C: Double = 1.0) extends AdaptiveLearningRate with MarginScaled

/**
 * Convenience name for the perceptron.
 */
class Perceptron extends ConstantLearningRate

/**
 * Convenience name for the averaged perceptron.
 */
class AveragedPerceptron extends ConstantLearningRate with ParameterAveraging
