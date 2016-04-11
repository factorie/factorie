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
import cc.factorie.util.TopN
import cc.factorie.variable._

/** Calculate the information gain between all features of Instances and the Instances' labels.
    @author Andrew McCallum
    @since 0.10
 */
class InfoGain[L<:DiscreteVar,F<:VectorVar](labels:Iterable[L], f:L=>F) extends cc.factorie.util.DenseDoubleSeq {
  def apply(i:Int): Double = infogains(i)
  def length = infogains.length
  val domain: DiscreteDomain = f(labels.head).domain.dimensionDomain
  private val infogains = cc.factorie.util.DoubleSeq(domain.size)
  var baseEntropy: Double = 0.0
  init(labels)
  
  // TODO Currently only works for CategoricalDomain, not DiscreteDomain
  override def top(n:Int): TopN[String] = new TopN[String](n, this, domain.asInstanceOf[CategoricalDomain[String]].categories)
  
  protected def init(labels:Iterable[L]) {
    val numInstances = labels.size
    val instanceDomain = f(labels.head).domain
    val numFeatures = instanceDomain.dimensionDomain.size
    val labelDomain = labels.head.domain
    val numLabels = labelDomain.size
    val featureTargetProportions = Array.fill(numFeatures)(new DenseProportions1(numLabels))
    val featureCount = new Array[Double](numFeatures)
    val targetProportions = new DenseProportions1(numLabels)
    for (label <- labels) {
      val instance: VectorVar = f(label)
      assert(instance.domain == instanceDomain)
      assert(label.domain == labelDomain)
      val labelIndex = label.intValue
      targetProportions.masses.+=(labelIndex, 1.0)
      //println("InfoGain "+instance.activeDomain.toSeq)
      //for (featureIndex <- instance.activeDomain.asSeq)
      //println("InfoGain "+instance.tensor.asInstanceOf[cc.factorie.la.GrowableSparseBinaryTensor1].toIntArray.toSeq)
      assert(instance.value.activeDomain.toSeq.distinct.length == instance.value.activeDomain.toSeq.length, instance.value.activeDomain.toSeq.toString())
      instance.value.activeDomain.foreach(featureIndex => {
        featureTargetProportions(featureIndex).masses.+=(labelIndex, 1.0)
        featureCount(featureIndex) += 1
      })
    }
    baseEntropy = targetProportions.entropy
    for (featureIndex <- 0 until numFeatures) {
      //println("InfoGain feature="+instanceDomain.dimensionDomain.asInstanceOf[CategoricalDomain[String]].category(featureIndex))
      //println("InfoGain targetProportions="+targetProportions.masses)
      //println("InfoGain featureTargetProp="+featureTargetProportions(featureIndex).masses)
      val entropyWithFeature = if (featureTargetProportions(featureIndex).masses.massTotal > 0) featureTargetProportions(featureIndex).entropy else 0.0
      val normWithoutFeature = numInstances - featureCount(featureIndex)
      val entropyWithoutFeature = 
        if (normWithoutFeature > 0) {
          val noTargetProportions = new DenseMasses1(numLabels)
          noTargetProportions += targetProportions.masses
          noTargetProportions -= featureTargetProportions(featureIndex).masses
          noTargetProportions.normalize()
          noTargetProportions.entropy
          //maths.entropy((0 until numLabels).map(li => (targetProportions.mass(li) - featureTargetProportions(featureIndex).mass(li))/normWithoutFeature))
        } else 0.0
      infogains(featureIndex) = baseEntropy - featureCount(featureIndex)/numInstances * entropyWithFeature - (numInstances-featureCount(featureIndex))/numInstances * entropyWithoutFeature
    }
  }

}
