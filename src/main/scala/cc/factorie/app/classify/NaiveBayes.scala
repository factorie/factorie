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

class NaiveBayesTrainer extends ClassifierTrainer {
  var biasSmoothingMass = 1.0
  var evidenceSmoothingMass = 1.0
  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteTensorVar](il: LabelList[L, F]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures, il.labelDomain, il.instanceDomain)(il.labelManifest, il.featureManifest)
    val labelDomain = il.labelDomain
    val featureDomain = il.featureDomain
    val numLabels = labelDomain.size
    val numFeatures = featureDomain.size
    val bias = new DenseProportions1(numLabels)
    val evid = Seq.tabulate(numLabels)(i => new DenseProportions1(numFeatures))
    // Note: this doesn't actually build the graphical model, it just gathers smoothed counts, never creating factors
    // Incorporate smoothing, with simple +m smoothing
    for (li <- 0 until numLabels) bias.masses += (li, biasSmoothingMass)
    for (li <- 0 until numLabels; fi <- 0 until numFeatures) evid(li).masses += (fi, evidenceSmoothingMass)
    // Incorporate evidence
    for (label <- il) {
      val targetIndex = label.intValue
      bias.masses += (targetIndex, 1.0)
      val features = il.labelToFeatures(label)
      val activeElements = features.tensor.activeElements
      while (activeElements.hasNext) {
        val (featureIndex, featureValue) = activeElements.next()
        evid(targetIndex).masses += (featureIndex, featureValue)
      }
    }
    // Put results into the model templates
    forIndex(numLabels)(i => cmodel.biasTemplate.weights(i) = math.log(bias(i)))
    for (li <- 0 until numLabels; fi <- 0 until numFeatures)
      cmodel.evidenceTemplate.weights(li * numFeatures + fi) = math.log(evid(li).apply(fi))
    new ModelBasedClassifier[L](cmodel, il.head.domain)
  }
}
