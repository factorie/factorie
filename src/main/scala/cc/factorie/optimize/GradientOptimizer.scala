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

