package cc.factorie.app.uschema

/**
 * Created by beroth on 2/18/15.
 */
object Evaluator {

  def meanAveragePrecision(classToPredictionAndLabel: Map[Int, Seq[(Double, Boolean)]]): Double = {
    classToPredictionAndLabel.values.map(averagePrecision(_)).sum / classToPredictionAndLabel.size.toDouble
  }

  def averagePrecision(predictionAndLabel: Seq[(Double, Boolean)]): Double = {
    val judgements = predictionAndLabel.sortBy(-_._1).map(_._2)
    // This foldleft aggregates (#true, #false, averagePrecision).
    // #true and #false are simply counted up.
    // averagePrecision (ap) is updated for true items:
    // ap_new = (#true_new - 1) * ap_old / #true_new + 1 / (#true_new + #false_new)
    //        = (#true_old) * ap_old / (#true_old + 1) + 1 / (#true_old + #false_old + 1)
    val mapStats = judgements.foldLeft((0,0,0.0))((stat,j) => {if (j==true) {
      (stat._1 + 1, stat._2, stat._1 * stat._3 / (stat._1 + 1) + 1.0/(stat._1 + stat._2 + 1))
    } else {
      (stat._1, stat._2 + 1, stat._3)
    }})
    mapStats._3
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
