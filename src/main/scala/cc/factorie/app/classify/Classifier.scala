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

package cc.factorie.app.classify
import cc.factorie._
import cc.factorie.generative.Proportions
import cc.factorie.er._
import scala.collection.mutable.{HashMap,ArrayBuffer}

/** Infrastructure for independent classification of feature vectors with String-valued features. 
    @author Andrew McCallum
    @since 0.8
 */

/** Performs independent prediction of (iid) Labels (all of whom must share the same domain).
    Has abstract methods for "model" and "labelDomain". */
trait Classifier[L<:DiscreteVariable] {
  def model:Model
  def labelDomain: DiscreteDomain
  def classify(label:L): Classification[L] = {
    require(label.domain eq labelDomain)
    val p = label.proportions(model) // Here the model's distribution over labels is calculated
    val result = new Classification(label, model, p)
    // In addition to recording the result in a Classification instance, also set the label to the most likely value
    label.set(result.bestLabelIndex)(null)
    result
  }
  def classify(labels:Iterable[L]): Seq[Classification[L]] = labels.toSeq.map(label => classify(label))
}

/** An "instance list" for iid classification, except it actually holds labels, each of which is associated with a feature vector. */
class LabelList[L<:DiscreteVar](val labelToFeatures:L=>DiscreteVectorVar)(implicit lm:Manifest[L]) extends ArrayBuffer[L] {
  def this(labels:Iterable[L], l2f:L=>DiscreteVectorVar)(implicit lm:Manifest[L]) = { this(l2f); this ++= labels }
  val instanceWeights: HashMap[L,Double] = null
  def labelDomain = head.domain // TODO Perhaps we should verify that all labels in the list have the same domain
  def instanceDomain = labelToFeatures(head).domain // TODO Likewise
  def featureDomain = instanceDomain.dimensionDomain
}

// TODO Consider including labelToFeatures here also?
/** The result of applying a Classifier to a Label. */
class Classification[L<:DiscreteVar](val label:L, val model:Model, val proportions:Proportions) {
  val bestLabelIndex = proportions.maxPrIndex
  def bestLabelValue = label.domain.getValue(bestLabelIndex)
  def bestCategoryValue: String = bestLabelValue match {
    case cv:CategoricalValue[String] => cv.category
    case dv:DiscreteValue => dv.intValue.toString
  }
}

/** A collection of Classification results, along with methods for calculating several evaluation measures.
    You can subclass Trial to add new evaluation measures. */
class Trial[L<:LabelVariable[String]](val classifier:Classifier[L]) extends LabelEvaluation(classifier.labelDomain.asInstanceOf[CategoricalDomain[String]]) with IndexedSeq[Classification[L]] {
  private val classifications = new ArrayBuffer[Classification[L]]
  def length = classifications.length
  def apply(i:Int) = classifications(i)
  def +=(label:L): Unit = { classifications.+=(classifier.classify(label)); super.+=(label) }
  def ++=(labels:Iterable[L]): this.type = { labels.foreach(+=(_)); this }
  override def toString: String = evalString
}

/** An object that can train a Classifier given a LabelList. */
trait ClassifierTrainer {
  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L]
}
