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
import scala.collection.mutable.{HashMap,ArrayBuffer}

/** Infrastructure for independent classification of feature vectors with String-valued features. 
    @author Andrew McCallum
    @since 0.8
 */

//case class Instance[L<:DiscreteVariable,F<:DiscreteTensorVar](label:L, features:F)
// No, this isn't in the FACTORIE philosophy; relations between variables can be captured with functions instead
// Note that this function could create a new DiscreteTensorVar on the fly.

/** Performs independent prediction of (iid) Labels (all of whom must share the same domain).
    Has abstract methods for "model" and "labelDomain". */
trait Classifier[L<:DiscreteVariable] {
  def model:Model
  def labelDomain: DiscreteDomain
  def classify(label:L): Classification[L] = {
    require(label.domain eq labelDomain)
    Classify(label, model)
  }
  def classify(labels:Iterable[L]): Seq[Classification[L]] = labels.toSeq.map(label => classify(label))
}

// TODO Consider putting a TUI here
object Classify {
  def apply[L<:DiscreteVariable](label:L, model:Model): Classification[L] = {
    val p = label.proportions(model) // Here the model's distribution over labels is calculated
    val result = new Classification(label, model, p)
    // In addition to recording the result in a Classification instance, also set the label to the most likely value
    label.set(result.bestLabelIndex)(null)
    result
  }
}

/** An "instance list" for iid classification, except it actually holds labels, 
    each of which is associated with a feature vector through the provided function labelToFeatures. */
class LabelList[L<:DiscreteVar,+F<:DiscreteTensorVar](val labelToFeatures:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) extends ArrayBuffer[L] {
  def this(labels:Iterable[L], l2f:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) = { this(l2f); this ++= labels }
  val labelManifest = lm
  //private val featureManifest = fm
  def labelClass = lm.erasure
  val featureClass = fm.erasure
  //val instanceWeights: HashMap[L,Double] = null // TODO Implement this! -akm
  def labelDomain = head.domain // TODO Perhaps we should verify that all labels in the list have the same domain?
  def instanceDomain = labelToFeatures(head).domain // TODO Likewise
  def featureDomain = instanceDomain.dimensionDomain
}

// TODO Consider including labelToFeatures here also?  
// TODO Should we store the Classifier instead of the Model?  But what if it doesn't come from a Classifier?
/** The result of applying a Classifier to a Label. */
class Classification[L<:DiscreteVar](val label:L, val model:Model, val proportions:Proportions) {
  val bestLabelIndex = proportions.maxIndex
  def bestLabelValue = label.domain.apply(bestLabelIndex)
  def bestCategoryValue: String = bestLabelValue match {
    case cv:CategoricalValue[_] => cv.category.toString
    case dv:DiscreteValue => dv.intValue.toString
  }
}

/** A collection of Classification results, along with methods for calculating several evaluation measures.
    You can subclass Trial to add new evaluation measures. */
// TODO Make this work for arbitrary category type, not just String; needs existential type argument.
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
  def train[L<:LabelVariable[_],F<:DiscreteTensorVar](il:LabelList[L,F])(implicit lm:Manifest[L], fm:Manifest[F]): Classifier[L]
}
