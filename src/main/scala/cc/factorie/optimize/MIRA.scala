package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._

class MIRA(var learningMargin:Double) extends GradientOptimizer {
  def this() = this(1.0)
  //def useObjectiveChangeAsMargin: Boolean = true
  val boxConstraint: Double = 1.0 //Math.POS_INF_DOUBLE

  def reset(): Unit = throw new Error("Not yet implemented")

  /** The "margin" is the difference between the best objective score and objective score of the model's choice. */
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    //if (useObjectiveChangeAsMargin) learningMargin = changeProposal.objectiveScore.abs else 1
    val learningRate = math.min(kktMultiplier(gradient, margin), boxConstraint)
    weights.+=(gradient, learningRate)
  }
  def isConverged = false // TODO What to put here?
  
  protected val epsilon: Double = 0.000000001
  protected def kktMultiplier(gradient:Tensor, margin:Double): Double = {
    val l2sqrd = gradient dot gradient
    val error =  learningMargin - margin //TODO: this computation should really be done in SampleRankExample
    var lambda = 0.0
    if (l2sqrd > 0.0 + epsilon || l2sqrd < 0.0 - epsilon)
      lambda = error / l2sqrd
    if (lambda < 0.0) lambda = 0.0
    lambda
  }
}
