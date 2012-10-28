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
  var variance: Double = 0.1
  abstract override def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double) {
    gradient += (weights, -variance)
    super.step(weights, gradient, value - variance * (weights dot weights), margin)
  }
}