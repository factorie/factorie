package cc.factorie
import scalala.Scalala._
import scalala.tensor.Vector

trait WeightUpdates {
  type TemplatesToUpdate <: DotTemplate
  /** Call this method to use the current gradient to change the weight parameters. */
  var updateCount : Int = 0
  def updateWeights : Unit = updateCount += 1

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit
}


