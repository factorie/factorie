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

/** Calculate the information gain between all features of Instances and the Instances' labels.
    @author Andrew McCallum
    @since 0.10
 */
class InfoGain(instances:Iterable[InstanceVariable]) extends IndexedSeq[Double] {
  def apply(i:Int) = infogains(i)
  def length = infogains.length
  val categoricalDomain: CategoricalDomain[String] = instances.head.domain.dimensionDomain
  private val infogains = new Array[Double](categoricalDomain.size)
  var baseEntropy: Double = 0.0
  init(instances)
  
  def top(n:Int): Seq[(String,Double)] = {
    val entries = this.toArray.zipWithIndex.sortBy({case (infogain, index) => -infogain}).take(n) //.toList
    entries.map({case (infogain,index)=>new Tuple2(categoricalDomain.getCategory(index).toString, infogain)})
  }
  
  protected def init(instances:Iterable[InstanceVariable]) {
    val numInstances = instances.size
    val instanceDomain = instances.head.domain
    val numFeatures = instanceDomain.dimensionDomain.size
    val labelDomain = instances.head.label.domain
    val numLabels = labelDomain.size
    //val targetFeatureCount = Array.ofDim[Double](numTargets, numFeatures)
    val featureTargetProportions = Array.fill(numFeatures)(new cc.factorie.generative.DenseCountsProportions(numLabels))
    val featureCount = new Array[Double](numFeatures)
    val targetProportions = new cc.factorie.generative.DenseCountsProportions(numLabels)
    var targetCountSum = 0.0
    // double flv; // feature location value
    // int fli; // feature location index
    // double count;
    // Populate targetFeatureCount, et al
    for (instance <- instances) {
      assert(instance.domain == instanceDomain)
      assert(instance.label.domain == labelDomain)
      val labelIndex = instance.label.intValue
      targetProportions.increment(labelIndex, 1.0)(null)
      for (featureIndex <- instance.activeDomain) {
        featureTargetProportions(featureIndex).increment(labelIndex, 1.0)(null)
        //targetFeatureCount(labelIndex)(featureIndex) += 1
        featureCount(featureIndex) += 1
      }
    }
    baseEntropy = targetProportions.entropy
    forIndex(numFeatures)(featureIndex => {
      val entropyWithFeature = if (featureTargetProportions(featureIndex).countsTotal > 0) featureTargetProportions(featureIndex).entropy else 0.0
      val normWithoutFeature = numInstances - featureCount(featureIndex)
      val entropyWithoutFeature = 
        if (normWithoutFeature > 0)
          maths.entropy((0 until numLabels).map(li => (targetProportions.counts(li) - featureTargetProportions(featureIndex).counts(li))/normWithoutFeature))
        else 0.0
      infogains(featureIndex) = baseEntropy - featureCount(featureIndex)/numInstances * entropyWithFeature - (numInstances-featureCount(featureIndex))/numInstances * entropyWithoutFeature
    })
  }

}
