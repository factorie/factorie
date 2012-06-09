package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._

class AROW(var learningMargin:Double = 1.0) extends GradientOptimizer {
  def reset(): Unit = throw new Error("Not yet implemented")
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    throw new Error("Not yet implemented.")
  }
  def isConverged = false // TODO What to put here?
}
