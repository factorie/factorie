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

/** An "instance list" for iid classification, except it actually holds labels, 
    each of which is associated with a feature vector through the provided function labelToFeatures. */
class LabelList[L<:DiscreteVar,F<:DiscreteTensorVar](val labelToFeatures:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) extends ArrayBuffer[L] {
  def this(labels:Iterable[L], l2f:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) = { this(l2f); this ++= labels }
  val labelManifest = lm
  val featureManifest = fm
  def labelClass = lm.erasure
  def featureClass = fm.erasure
  /** The weight with which a LabelVariable should be considered in training. */
  lazy val instanceWeight = new HashMap[L,Double] { override def default(l:L) = 1.0 }
  override def remove(index:Int): L = {
    instanceWeight.remove(apply(index))
    super.remove(index)
  }
  override def remove(index:Int, count:Int): Unit = {
    this.slice(index, index+count).foreach(l => instanceWeight.remove(l))
    super.remove(index, count)
  }
  def labelDomain = head.domain // TODO Perhaps we should verify that all labels in the list have the same domain?
  def instanceDomain = labelToFeatures(head).domain // TODO Likewise
  def featureDomain = instanceDomain.dimensionDomain
}

//case class Instance[L<:DiscreteVariable,F<:DiscreteTensorVar](label:L, features:F)
// No, this isn't in the FACTORIE philosophy; relations between variables can be captured with functions instead
// Note that this function could create a new DiscreteTensorVar on the fly.

