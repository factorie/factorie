package cc.factorie
import scalala.Scalala._
import scalala.tensor.Vector

trait WeightUpdates {
  type TemplatesToUpdate <: DotTemplate
  /** Call this method to use the current gradient to change the weight parameters. */
  def updateWeights : Unit
  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit
}


