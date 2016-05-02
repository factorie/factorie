/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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
import cc.factorie.app.classify.backend.{MulticlassClassification, MulticlassClassifier}
import cc.factorie.variable.{CategoricalDomain, LabeledDiscreteEvaluation, LabeledMutableDiscreteVar}

import scala.collection.mutable.ArrayBuffer


case class LabeledClassification[L](label: L, classification: MulticlassClassification)

/** A collection of Classification results, along with methods for calculating several evaluation measures.
    You can subclass Trial to add new evaluation measures. */
class Trial[L<:LabeledMutableDiscreteVar,F](val classifier: MulticlassClassifier[F], labelDomain: CategoricalDomain[String], l2f: L => F)
  extends LabeledDiscreteEvaluation(labelDomain) with IndexedSeq[LabeledClassification[L]] {
  private val classifications = new ArrayBuffer[LabeledClassification[L]]
  def length = classifications.length
  def apply(i:Int) = classifications(i)
  def +=(label:L): Unit = {
    val c = LabeledClassification(label, classifier.classification(l2f(label)))
    classifications += c
    super.+=(label, c.classification.bestLabelIndex)
  }
  def ++=(labels:Iterable[L]): this.type = { labels.foreach(+=(_)); this }
  def +=(c:LabeledClassification[L]): Unit = {
    classifications += c
    super.+=(c.label, c.classification.bestLabelIndex)
  }
  override def toString(): String = "OVERALL: " + overallEvalString + "\n" +  evalString
}
