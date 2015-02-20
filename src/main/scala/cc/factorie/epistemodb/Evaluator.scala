package cc.factorie.epistemodb

/**
 * Created by beroth on 2/18/15.
 */
object Evaluator {

  def meanAveragePrecision(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]]): Double = {
    throw new UnsupportedOperationException
  }

  def averagePrecision(predictionAndLabel: Seq[(Double, Boolean)]): Double = {
    throw new UnsupportedOperationException
  }

  // convenience method
  def averagePrecision(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]]): Double = {
    throw new UnsupportedOperationException
  }

  /**
   * Micro average: compute overall precision and recall across all relations. Compute f1 score from that.
   * This is the approach used in TAC slot-filling and cold start.
   */
  def microAvgF1Score(predictionAndLabel: Seq[(Double, Boolean)], threshold: Double): Double = {
    throw new UnsupportedOperationException
  }

  def microAvgF1Score(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]],
                      classToThreshold: Option[Map[Int,Double]] = None, defaultThreshold: Double): Double = {
    throw new UnsupportedOperationException
  }

  /**
   * Macro average: compute precision and recall for each test column separately, average those numbers and compute f1.
   */
  def macroAvgF1Score(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]],
                      classToThreshold: Option[Map[Int,Double]] = None, defaultThreshold: Double = 0.0): Double = {
    throw new UnsupportedOperationException
  }

  def tuneThresholdsMicroAvgF1Score(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]]): Map[Int,Double] = {
    throw new UnsupportedOperationException
  }

  def tuneThresholdsMacroAvgF1Score(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]]): Map[Int,Double] = {
    throw new UnsupportedOperationException
  }
}
