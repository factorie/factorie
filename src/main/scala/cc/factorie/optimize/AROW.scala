package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._

class AROW(val weights:Tensor, var learningMargin:Double = 1.0) extends GradientOptimizer {
  def this(model:TemplateModel) = this(model.weightsTensor, 1.0)
  def this(model:TemplateModel, rate:Double) = this(model.weightsTensor, rate)
  
  def step(gradient:Tensor, value:Double, margin:Double): Unit = {
    throw new Error("Not yet implemented.")
  }
  def isConverged = false // TODO What to put here?
  
}
