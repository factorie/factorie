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
import cc.factorie._
import cc.factorie.util.TopN
import cc.factorie.variable._

/** Calculate the information gain between the binary variable "in class" / "not in class" 
    and the binary variable "has feature" / "does not have feature"
    for every (label,feature) combination. 
    @author Andrew McCallum
    @since 0.10
 */
class PerLabelInfoGain[L<:DiscreteVar,F<:VectorVar](labels:Iterable[L], labelToFeatures:L=>F) {
  val instanceDomain: VectorDomain = labelToFeatures(labels.head).domain
  val featureDomain: DiscreteDomain = instanceDomain.dimensionDomain
  val labelDomain: DiscreteDomain = labels.head.domain
  private val infogains = Array.ofDim[Double](labelDomain.size, featureDomain.size)
  var labelEntropies = new Array[Double](labelDomain.size)
  init(labels)
  
  def top(labelIndex:Int, n:Int): TopN[String] = new TopN[String](n, infogains(labelIndex), featureDomain.asInstanceOf[CategoricalDomain[String]].categories)
  def top(labelValue:DiscreteValue, n:Int): TopN[String] = {
    require(labelValue.dim1 == labelDomain.size) //require(labelValue.domain == labelDomain)
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
      // for (featureIndex <- instance.activeDomain) 
      instance.value.foreachActiveElement((featureIndex,featureValue) => {
        labelFeatureCounts(labelIndex)(featureIndex) += 1
        featureCounts(featureIndex) += 1
      })
    }
    
    for (labelIndex <- 0 until numLabels) {
      val pc = (labelCounts(labelIndex):Double)/numInstances
      val pnc = (numInstances-labelCounts(labelIndex):Double)/numInstances
      labelEntropies(labelIndex) = entropy (pc, pnc)
    }
    //if (true) for (labelIndex <- 0 until numLabels) println(labelDomain.asInstanceOf[CategoricalDomain[String]].category(labelIndex)+" "+labelEntropies(labelIndex))

    // Calculate per-class infogain of each feature, and store it in "igs"
    for (fi <- 0 until numFeatures) {
      val pf = (featureCounts(fi):Double)/numInstances
      val pnf = (numInstances-featureCounts(fi):Double)/numInstances
      assert (pf >= 0)
      assert (pnf >= 0)
      //assert (sum == featureCounts[fi]);
      for (li <- 0 until numLabels) {
        if (featureCounts(fi) == 0) {
          infogains(li)(fi) = 0
        } else {
          var pc, pnc: Double = 0.0
          // Calculate the {ci,!ci}-entropy given that the feature does occur
          pc = (labelFeatureCounts(li)(fi):Double) / featureCounts(fi)
          pnc = (featureCounts(fi)-labelFeatureCounts(li)(fi):Double) / featureCounts(fi)
          val ef = entropy(pc, pnc)
          // Calculate the {ci,!ci}-entropy given that the feature does not occur
          pc = (labelCounts(li)-labelFeatureCounts(li)(fi):Double) / (numInstances-featureCounts(fi))
          pnc = ((numInstances-featureCounts(fi):Double)-(labelCounts(li)-labelFeatureCounts(li)(fi))) / (numInstances-featureCounts(fi))
          val enf = entropy(pc, pnc)
          infogains(li)(fi) = labelEntropies(li) - (pf*ef + pnf*enf)
          //if (fi < 100)  println("pf="+pf+" ef="+ef+" pnf="+pnf+" enf="+enf+" e="+labelEntropies(li)+" cig="+labelFeatureCounts(li)(fi)+" c="+featureDomain.asInstanceOf[CategoricalDomain[String]].category(fi))
        }
      }
    }
    
  }

  private def entropy (pc:Double, pnc:Double): Double = {
    if (pc != pc && pnc != pnc)
      0.0
    else {
      assert (math.abs((pc+pnc)-1) < 0.0001, "pc="+pc+" pnc="+pnc)
      if (pc == 0 || pnc == 0)
        0.0
      else {
        val ret = -pc * math.log(pc) / maths.log2 - pnc * math.log(pnc) / maths.log2
        assert (ret >= 0, "pc="+pc+" pnc="+pnc)
        ret
      }
    }
  }

}