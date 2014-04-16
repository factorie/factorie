package cc.factorie.util

/** For evaluating precision, recall and F1 using raw true positive,... false negative counts.
    @author Andrew McCallum */
class F1Evaluation {
  var precisionNumerator = 0.0
  var precisionDenominator = 0.0
  var recallNumerator = 0.0
  var recallDenominator = 0.0
  
  def appendTP(count:Double): Unit = {
    precisionNumerator += count
    recallNumerator += count
    precisionDenominator += count
    recallDenominator += count
  }
  def appendFP(count:Double): Unit = {
    precisionDenominator += count
  }
  def appendTN(count:Double): Unit = { }
  def appendFN(count:Double): Unit = {
    recallDenominator += count
  }
  
  def precision: Double = if (precisionDenominator == 0.0) 1.0 else precisionNumerator / precisionDenominator
  def recall: Double = if (recallDenominator == 0.0) 1.0 else recallNumerator / recallDenominator
  def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)

  def append(pred:Boolean, truth:Boolean): Unit = {
    if (pred && truth) appendTP(1.0)
    else if (!pred && !truth) appendTN(1.0)
    else if (pred && !truth) appendFP(1.0)
    else appendFN(1.0)
  }
  def microAppend(other: F1Evaluation): Unit = { 
    precisionNumerator += other.precisionNumerator
    precisionDenominator += other.precisionDenominator
    recallNumerator += other.recallNumerator
    recallDenominator += other.recallDenominator
  }
  def macroAppend(other: F1Evaluation): Unit = {
    precisionNumerator += other.precision
    precisionDenominator += 1.0
    recallNumerator += other.recall
    recallDenominator += 1.0
  }
  
  def toString(prefix: String): String = 
    "%s %6.3f %6.3f %6.3f".format(prefix, precision * 100.0, recall * 100.0, f1 * 100.0)
}
