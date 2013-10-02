package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._
import cc.factorie.model.{WeightsMap, WeightsSet}

/** Base trait for optimizers that update weights according to a gradient. */
trait GradientOptimizer {
  // TODO Why are we passing in weightsSet each time?  Couldn't this be dangerous?  -akm
  // I think passing it in here is probably a good compromise for optimizers that require no setup/teardown on weights or can do it automatically -luke
  /**
   * Updates the weights according to the gradient.
   * @param weights The weights
   * @param gradient The gradient
   * @param value The value
   */
  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit
  /**
   * Whether the optimizer has converged yet.
   */
  def isConverged: Boolean
  /**
   * Reset the optimizers internal state (such as Hessian approximation, etc.)
   */
  def reset(): Unit
  /**
   * Some optimizers swap out weights with special purpose tensors for e.g. efficient scoring while learning.
   * @param weights The weights
   */
  def initializeWeights(weights: WeightsSet): Unit
  /**
   * Once learning is done, the weights should be copied back into normal tensors.
   * @param weights The weights
   */
  def finalizeWeights(weights: WeightsSet): Unit
}

/** Include L2 regularization (Gaussian with given scalar as the spherical covariance) in the gradient and value. */
trait L2Regularization extends GradientOptimizer {
  var variance: Double = 10.0
  abstract override def step(weights: WeightsSet, gradient: WeightsMap, value: Double) {
    gradient += (weights, -1 / variance)
    super.step(weights, gradient, value - 0.5 / variance * (weights dot weights))
  }
}