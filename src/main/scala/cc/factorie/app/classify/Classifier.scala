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

/** Infrastructure for independent classification of feature vectors with String-valued features. 
    @author Andrew McCallum
@since 0.8
  */

/** Performs independent prediction of (iid) Labels (all of which must share the same domain).
    Has abstract method "labelDomain". */
trait Classifier[L <: MutableDiscreteVar[_]] {
  def labelDomain: DiscreteDomain // This is necessary for LabelEvaluation
  /** Return a record summarizing the outcome of applying this classifier to the given label.  Afterwards the label will have the same value it had before this call. */
  def classification(label: L): Classification[L]
  def classifications(labels: Iterable[L]): Seq[Classification[L]] = labels.toSeq.map(label => classification(label))
  /** Set the label to classifier-predicted value and return a Classification object summarizing the outcome. */
  def classify(label: L): Classification[L] = {
    val c = classification(label)
    c.globalize(null)
    c
  }
  def classify(labels: Iterable[L]): Seq[Classification[L]] = {
    val c = classifications(labels)
    c.foreach(_.globalize(null))
    c
  }
}

/** A classifier that uses a Model to score alternative label values. */
class ModelBasedClassifier[L <: MutableDiscreteVar[_]](val model: Model with Weights, val labelDomain: DiscreteDomain) extends Classifier[L] {
  def classification(label: L): Classification[L] = {
    require(label.domain eq labelDomain)
    new Classification(label, this, label.proportions(model))
  }
}

/** An object that can train a Classifier given a LabelList. */
trait ClassifierTrainer {
  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteDimensionTensorVar](il: LabelList[L, F]): Classifier[L]
}

/** An object that can gather  */
trait ClassifierEvaluator[L <: MutableDiscreteVar[_]] {
  def += (c: Classification[L]): Unit
  def toString: String
}