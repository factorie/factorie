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

package cc.factorie.app.tokenseq.labeled
import cc.factorie._

/** Stores the results of evaluating per-label accuracy and other measures.
    Note, this is not per-field accuracy. */
class PerLabelEvaluation[L<:LabelVariable[String]](val labelValue: String)(implicit m:Manifest[L]) {
  var fp = 0
  var fn = 0
  var tp = 0
  private val targetIndex = Domain[L](m).index(labelValue)

  def ++=(tokenseqs:Seq[Seq[{def label:LabelVariable[String]}]]) = tokenseqs.foreach(ts => this += ts.map(_.label))
  //def +=(tokens:Seq[{def label:LabelVariable[String]}]): Unit = +=(tokens.map(_.label))
  def +=(labels: Seq[LabelVariable[String]]): Unit = {
    for (l <- labels) {
      val trueIndex = l.trueIntValue
      val predIndex = l.intValue
      if (targetIndex == trueIndex) {
        if (trueIndex == predIndex)
          tp += 1
        else
          fp += 1
      } else if (targetIndex == predIndex) {
        if (trueIndex == predIndex)
          tp += 1
        else
          fn += 1
      }
    }
  }
  def accuracy: Double = throw new Error
  def precision: Double = if (tp + fp == 0.0) 0.0 else tp.toDouble / (tp + fp)
  def recall: Double = if (tp + fn == 0.0) 0.0 else tp.toDouble / (tp + fn)
  def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
  def correctCount = tp
  def missCount = fn
  def alarmCount = fp
  override def toString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelValue, f1, precision, recall, tp, fp, fn, tp+fn, tp+fp) 
}

class LabelEvaluation[L<:LabelVariable[String] with AbstractVarInSeq[L]](val backgroundLabelValue:String)(implicit m:Manifest[L]) {
  import scala.collection.mutable.HashMap
  def this(labels:Seq[L])(implicit m:Manifest[L]) = { this("O"); this += labels }
  def this(lab:String, labels:Seq[L])(implicit m:Manifest[L]) = { this(lab); this += labels }
  //def this(labels:Seq[LabeledTokenSeq]) = { this("O"); this.+=(labels.flatMap(_.labels)) }
  var fp = 0
  var fn = 0
  var tp = 0
  //println("Evaluation Labels: "+Domain[Label].toList)
  private val labelEval: HashMap[String,PerLabelEvaluation[L]] = { 
    val h = new HashMap[String,PerLabelEvaluation[L]];
    h ++= Domain[L](m).map(labelString => (labelString, new PerLabelEvaluation[L](labelString)))
    h
  }
  /** Return the LabelEvaluation specific to labelString. */
  def apply(labelString:String) = labelEval(labelString)
  def +=(labels: Seq[L]): Unit = {
    labelEval.values.foreach(eval => { 
      eval += labels
      if (eval.labelValue != backgroundLabelValue) {
        fp += eval.fp
        fn += eval.fn
        tp += eval.tp
      }
    })
  }
  def accuracy: Double = tp.toDouble / ( tp + fp)
  def precision: Double = if (tp + fp == 0.0) 0.0 else tp.toDouble / (tp + fp)
  def recall: Double = if (tp + fn == 0.0) 0.0 else tp.toDouble / (tp + fn)
  def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
  def summaryString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp)
  override def toString = {
    val sb = new StringBuffer
    sb.append("ACCURACY "+accuracy)
    sb.append("\n")
    sb.append(summaryString)
    sb.append("\n")
    labelEval.values.foreach(e => { sb.append(e.toString); sb.append("\n") })
    sb.toString
  } 
}

