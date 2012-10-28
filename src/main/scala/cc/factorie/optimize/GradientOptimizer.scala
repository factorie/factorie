package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._

/** Repeatedly call "step" until "isConverged" is true. */
trait GradientOptimizer {
  //def step(gradient:Tensor, value:Double, margin:Double): Unit
  // TODO Why are we passing in weights each time?  Couldn't this be dangerous?  -akm
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit
  def isConverged: Boolean
  def reset(): Unit
}

/** Include L2 regularization (Gaussian with given scalar as the diagonal covariance) in the gradient and value. */
trait L2Regularization extends GradientOptimizer {
  var variance: Double = 10.0
  abstract override def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double) {
    gradient += (weights, -1 / variance)
    // TODO: should this be -0.5 / variance * (weights dot weights)? -luke
    super.step(weights, gradient, value - 1 / variance * (weights dot weights), margin)
  }
}