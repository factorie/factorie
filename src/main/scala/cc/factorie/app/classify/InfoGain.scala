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
import cc.factorie.util.TopN

/** Calculate the information gain between all features of Instances and the Instances' labels.
    @author Andrew McCallum
    @since 0.10
 */
class InfoGain[L<:DiscreteVar](labels:Iterable[L], f:L=>DiscreteVectorVar) extends IndexedSeq[Double] {
  def this(labels:LabelList[L]) = this(labels, labels.labelToFeatures)
  def apply(i:Int) = infogains(i)
  def length = infogains.length
  val domain: DiscreteDomain = f(labels.head).domain.dimensionDomain
  private val infogains = new Array[Double](domain.size)
  var baseEntropy: Double = 0.0
  init(labels)
  
  // TODO Currently only works for CategoricalDomain, not DiscreteDomain
  def top(n:Int): TopN[String] = new TopN[String](n, this, domain.asInstanceOf[CategoricalDomain[String]].categoryValues)
  
  protected def init(labels:Iterable[L]) {
    val numInstances = labels.size
    val instanceDomain = f(labels.head).domain
    val numFeatures = instanceDomain.dimensionDomain.size
    val labelDomain = labels.head.domain
    val numLabels = labelDomain.size
    val featureTargetProportions = Array.fill(numFeatures)(new DenseProportions1(numLabels))
    val featureCount = new Array[Double](numFeatures)
    val targetProportions = new DenseProportions1(numLabels)
    var targetCountSum = 0.0
    for (label <- labels) {
      val instance = f(label)
      assert(instance.domain == instanceDomain)
      assert(label.domain == labelDomain)
      val labelIndex = label.intValue
      targetProportions.+=(labelIndex, 1.0)
      //println("InfoGain "+instance.activeDomain.toSeq)
      for (featureIndex <- instance.activeDomain) {
        featureTargetProportions(featureIndex).+=(labelIndex, 1.0)
        featureCount(featureIndex) += 1
      }
    }
    baseEntropy = targetProportions.entropy
    forIndex(numFeatures)(featureIndex => {
      val entropyWithFeature = if (featureTargetProportions(featureIndex).massTotal > 0) featureTargetProportions(featureIndex).entropy else 0.0
      val normWithoutFeature = numInstances - featureCount(featureIndex)
      val entropyWithoutFeature = 
        if (normWithoutFeature > 0)
          maths.entropy((0 until numLabels).map(li => (targetProportions.mass(li) - featureTargetProportions(featureIndex).mass(li))/normWithoutFeature))
        else 0.0
      infogains(featureIndex) = baseEntropy - featureCount(featureIndex)/numInstances * entropyWithFeature - (numInstances-featureCount(featureIndex))/numInstances * entropyWithoutFeature
    })
  }

}
