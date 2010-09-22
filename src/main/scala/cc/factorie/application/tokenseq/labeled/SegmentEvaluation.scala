/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.application.tokenseq.labeled
import cc.factorie._
import scala.util.matching.Regex
import scala.collection.mutable.{HashSet,HashMap}

/** Evalute in terms of correct entire segments.  
    The field start and end boundaries must be perfect to count as correct.  No partial credit. 
    For example, this is the standard for results on CoNLL 2003. */
class PerSegmentEvaluation(val labelName:String, val labelValueStart: Regex, val labelValueContinue: Regex) {
  //println("PerSegmentEvaluation "); println(labelName); println(labelValueStart); println(labelValueContinue); println
  //if (labelValueContinue == null) labelValueContinue = labelValueStart // Waiting for Scala 2.8 default parameters
  var trueCount, predictedCount, correctCount = 0 // per segment
  var predictedStart, trueStart = false
  def ++=(tokenseqs:Seq[Seq[{def label:LabelVariable[String]}]]): Unit = tokenseqs.foreach(ts => this.+=(ts.map(_.label)))
  //def +=(tokenseq:Seq[{def label:LabelVariable[String]}]): Unit = +=(tokenseq.map(_.label))
  /** Add the given sequence of labels to the statistics for this evalution.
      Even though you may be tempted to put all Label instances across all sentences in a single Seq[] and pass them in here, 
      note that you risk getting slightly incorrect results at document boundaries: when one document ends 
      in a mention and the next document begins with the same mention type, 
      they will be counted as only one mention, when they should have been counted as two. */
  def +=(labels: Seq[LabelVariable[String]]): Unit = {
    //println("PerSegmentEvaluation += "+labels.size)
    for (position <- 0 until labels.length) {
      val label = labels(position)
      val labelHasPrev = position > 0
      val labelPrev: LabelVariable[String] = if (labelHasPrev) labels(position - 1) else null
      //print("\n"+label.token.word+"="+label.trueValue+"/"+label.value+" ")
      predictedStart = false; trueStart = false
      // Find out if we are at the beginning of a segment.  
      // This complicated conditional is necessary to make the start pattern "(B|I)-" work for both BIO and IOB formats.
      // We are at a start if either (a) only labelValueStart matches, or (b) labelValueContinue matches and the previous label doesn't match
      // The (b) case makes it work for IOB notation, in which "B-*" is only used at the boundary between two like-categoried mentions.
      if ((labelValueStart.findAllIn(label.value).hasNext && !labelValueContinue.findAllIn(label.value).hasNext)
          || (labelValueContinue.findAllIn(label.value).hasNext 
              && (!labelHasPrev || (!labelValueStart.findAllIn(labelPrev.value).hasNext && !labelValueContinue.findAllIn(labelPrev.value).hasNext)))) {
                predictedCount += 1
                predictedStart = true
                //print("ps ")
              }
      if ((labelValueStart.findAllIn(label.trueValue).hasNext && !labelValueContinue.findAllIn(label.trueValue).hasNext)
          || (labelValueContinue.findAllIn(label.trueValue).hasNext 
              && ((!labelHasPrev) || (!labelValueStart.findAllIn(labelPrev.trueValue).hasNext && !labelValueContinue.findAllIn(labelPrev.trueValue).hasNext)))) {
                trueCount += 1
                trueStart = true
                //print("ts ")
              }
      // Truth and prediction both agree that a segment is starting here, let's see if they end in the same place
      if (predictedStart && trueStart) {
        //print(" pts ")
        //print("%s=%s ".format(label.token.word, label.value))
        var predictedContinue, trueContinue = false
        var j = position + 1
        var stopSearchForSegmentEnd = false
        while (j < labels.length && !stopSearchForSegmentEnd) {
          val label2 = labels(j)
          predictedContinue = labelValueContinue.findAllIn(label2.value).hasNext
          trueContinue = labelValueContinue.findAllIn(label2.trueValue.toString).hasNext
          //print("j="+j+predictedContinue+trueContinue)
          //if (predictedContinue) print("pc ")
          //if (trueContinue) print("tc ")
          if (!predictedContinue || !trueContinue) {
            if (predictedContinue == trueContinue) {
              correctCount += 1 // Both sequences ended at the same position: correct
              //print("%s=%s/%s correct".format(label2.token.word, label2.trueValue.toString, label2.value))
            } //else print("%s=%s %s=%s/%s @%d wrong".format(if (label2.hasPrev) label2.prev.token.word else "(null)", if (label2.hasPrev) label2.prev.value else "(null)", label2.token.word, label2.trueValue, label2.value, j-position))
            stopSearchForSegmentEnd = true
          } //else print("%s=%s ".format(label2.token.word, label2.value))
          j += 1
        }
        // Handle special case for the end of the sequence
        // srr: fixed this to check not only whether trueContinue==predictedContinue, but if both are continuing.
        if (j == labels.length && predictedContinue && trueContinue) trueCount += 1
      }
    }
  }
  def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
  def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
  def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
  def alarmCount = predictedCount - correctCount
  def missCount = trueCount - correctCount
  def tp = correctCount
  def fn = missCount
  def fp = alarmCount
  override def toString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelName, f1, precision, recall, tp, fp, fn, trueCount, predictedCount) 
}
  
// Some utilities for automatically filling in values 
object SegmentEvaluation {
  // Assume that the first two characters of each label are the "B-" or "I-" prefix.  Skip the label "O" because it is less than 3 chars long
  def labelStringsToBase(labelVals:Seq[String]): Seq[String] = {
    val result = new HashSet[String]
    labelVals.foreach(s => if (s.length > 2) result += s.substring(2))
    result.toSeq
  }
}
   
// for defaultStartPrefix = "(B|I)-" // Although just "B-" would be enough for BIO, "(B|I)-" is needed for IOB
class SegmentEvaluation[L<:LabelVariable[String]](baseLabelStrings: Seq[String], startPrefix:String = "(B|I)-", continuePrefix:String = "I-") {
  def this()(implicit m:Manifest[L]) = this(SegmentEvaluation.labelStringsToBase(Domain[L](m).toSeq))
  def this(labels:Seq[L])(implicit m:Manifest[L]) = { this(); this.+=(labels) }
  private val evals = new HashMap[String,PerSegmentEvaluation]
  private var labelCount = 0
  private var labelCorrectCount = 0
  evals ++= baseLabelStrings.map(s => (s, new PerSegmentEvaluation(s, (startPrefix+s).r, (continuePrefix+s).r)))
  /** Return the LabelEvaluation specific to labelString. */
  def apply(labelString:String) = evals(labelString)
  def +=(labels: Seq[L]): Unit = {
    evals.values.foreach(eval => eval += labels)
    labelCount += labels.length
    labels.foreach(label => if (label.valueIsTruth) labelCorrectCount += 1)
  }
  // This is a per-label measure
  def tokenAccuracy = labelCorrectCount.toDouble / labelCount
  // The rest are per-segment
  def correctCount = evals.values.foldLeft(0)(_+_.correctCount)
  def predictedCount = evals.values.foldLeft(0)(_+_.predictedCount)
  def trueCount = evals.values.foldLeft(0)(_+_.trueCount)
  def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
  def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
  def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
  def alarmCount = predictedCount - correctCount
  def missCount = trueCount - correctCount
  def tp = correctCount
  def fn = missCount
  def fp = alarmCount
  def summaryString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d) acc=%-5f (%d/%d)\n".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp, tokenAccuracy, labelCorrectCount, labelCount)
  override def toString = {
    val sb = new StringBuffer
    //sb.append("ACCURACY "+tokenAccuracy+" ("+labelCorrectCount+"/"+labelCount+")")
    //sb.append("\n")
    sb.append(summaryString)
    evals.values.foreach(e => { sb.append(e.toString); sb.append("\n") })
    sb.toString
  } 
}
