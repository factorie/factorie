package cc.factorie
import scalala.Scalala._
import scalala.tensor.Vector

// TODO Consider renaming GradientWeightUpdates

/** For parameter estimation methods that use a gradient to update weight parameters. 
    @author Andrew McCallum */
trait WeightUpdates {
  type TemplatesToUpdate <: DotTemplate
  /** The number of times 'updateWeights' has been called. */
  var updateCount : Int = 0
  /** Call this method to use the current gradient to change the weight parameters.  When you override it, you must call super.updateWeights. */
  def updateWeights : Unit = updateCount += 1
  /** Abstract method to be provided elsewhere. */
  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit
}


