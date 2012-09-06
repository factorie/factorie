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

/** The result of applying a Classifier to a Label. */
class Classification[L<:MutableDiscreteVar[_]](theLabel:L, val classifier:Classifier[L], theProportions:Proportions1) extends DiscreteMarginal1(theLabel, theProportions) {
  final def label = _1
  val bestLabelIndex = proportions.maxIndex
  def bestLabelValue = label.domain.apply(bestLabelIndex)
  def bestCategoryValue: String = bestLabelValue match {
    case cv:CategoricalValue[_] => cv.category.toString
    case dv:DiscreteValue => dv.intValue.toString
  }
  // TODO  Is "set" the best name for this method?  Perhaps "globalize" like Assignment?
  //@deprecated("Use globalize instead") def set(): this.type = { label.set(bestLabelIndex)(null); this }
//  // TypedAssignment implementation
//  //def variables = List(_1)
//  def apply[B<:L](v:B): B#Value = if (v eq _1) bestLabelValue.asInstanceOf[B#Value] else null.asInstanceOf[B#Value]
//  def get[B<:L](v:B): Option[B#Value] = if (v eq _1) Some(bestLabelValue.asInstanceOf[B#Value]) else None
//  def contains(v:L): Boolean = if (v eq _1) true else false
//  override def globalize(implicit d:DiffList): Unit = label.set(bestLabelIndex)(null)
//
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
  def +=(c:Classification[L]): Unit = {
    if (c.classifier == classifier) classifications += c
    else throw new Error("Classification.classifier does not match.")
  }
  override def toString: String = evalString
}
