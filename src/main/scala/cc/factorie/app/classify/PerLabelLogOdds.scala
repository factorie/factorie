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

/** Calculate the weighted log-odds ratio:
    p(w|c) * log(p(w|c)/p(w|!c)) for each word w and label c.
    Ranks highly those words that substantially contribute to positively predicting label c.  
    @author Andrew McCallum
    @since 0.10
 */
class PerLabelLogOdds[L<:DiscreteVar,F<:VectorVar](labels:Iterable[L], labelToFeatures:L=>F) {
  val instanceDomain: VectorDomain = labelToFeatures(labels.head).domain
  val featureDomain: DiscreteDomain = instanceDomain.dimensionDomain
  val labelDomain: DiscreteDomain = labels.head.domain
  private val logodds = Array.ofDim[Double](labelDomain.size, featureDomain.size)
  var labelEntropies = new Array[Double](labelDomain.size)
  init(labels)
  
  def top(labelIndex:Int, n:Int): TopN[String] = new TopN[String](n, logodds(labelIndex), featureDomain.asInstanceOf[CategoricalDomain[String]].categories)
  def top(labelValue:DiscreteValue, n:Int): TopN[String] = {
    require(labelValue.dim1 == labelDomain.size) // require(labelValue.domain == labelDomain)
    top(labelValue.intValue, n)
  }
  
  protected def init(labels:Iterable[L]) {
    val numInstances = labels.size
    val numFeatures = featureDomain.size
    val numLabels = labelDomain.size
    
    val labelFeatureCounts = Array.ofDim[Double](numLabels, numFeatures)
    val featureCounts = new Array[Int](numFeatures)
    val labelCounts = new Array[Int](numLabels)
    
    for (label <- labels) {
      val instance = labelToFeatures(label)
      assert(instance.domain == instanceDomain)
      assert(label.domain == labelDomain)
      val labelIndex = label.intValue
      labelCounts(labelIndex) += 1
      //for (featureIndex <- instance.activeDomain) 
      instance.value.foreachActiveElement((featureIndex,featureValue) => {
        labelFeatureCounts(labelIndex)(featureIndex) += 1
        featureCounts(featureIndex) += 1
      })
    }
    
    // Calculate per-class weighted-log-odds of each feature, and store it in "igs"
    for (fi <- 0 until numFeatures) {
      for (li <- 0 until numLabels) {
        if (featureCounts(fi) == 0) {
          logodds(li)(fi) = 0.0
        } else {
          if (labelFeatureCounts(li)(fi) == 0)
            logodds(li)(fi) = 0.0
          else {
            // Use +1 "Laplace" smoothing
            val pr_w_c = (labelFeatureCounts(li)(fi) + 1.0) / (labelCounts(li) + numFeatures) 
            val pr_w_nc = (featureCounts(fi) - labelFeatureCounts(li)(fi) + 1.0) / (numInstances - labelCounts(li) + numFeatures)
            logodds(li)(fi) = pr_w_c * math.log(pr_w_c / pr_w_nc)
          }
        }
      }
    }
    
  }

}