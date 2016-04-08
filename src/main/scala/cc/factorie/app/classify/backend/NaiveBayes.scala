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

package cc.factorie.app.classify.backend

import cc.factorie.la.Tensor1
import cc.factorie.variable.DenseProportions1

class NaiveBayes(var evidenceSmoothingMass: Double = 1.0) extends MulticlassClassifierTrainer[LinearMulticlassClassifier]  {
  def baseTrain(classifier: LinearMulticlassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (LinearMulticlassClassifier) => Unit) {
    val numLabels = classifier.labelSize
    val numFeatures = classifier.featureSize
    // assuming the existence of a bias feature
    val evid = Seq.tabulate(numLabels)(i => new DenseProportions1(numFeatures))
    // Note: this doesn't actually build the graphical model, it just gathers smoothed counts, never creating factors
    // Incorporate smoothing, with simple +m smoothing
    for (li <- 0 until numLabels; fi <- 0 until numFeatures) evid(li).masses += (fi, evidenceSmoothingMass)
    // Incorporate evidence
    for (i <- labels.indices; label = labels(i); feature = features(i); w = weights(i)) {
      feature.foreachActiveElement((featureIndex, featureValue) => {
        evid(label).masses += (featureIndex, w*featureValue)
      })
    }
    // Put results into the model templates
    val evWeightsValue = classifier.weights.value
    for (li <- 0 until numLabels; fi <- 0 until numFeatures)
      evWeightsValue(fi, li) = math.log(evid(li).apply(fi))
  }
  def newModel(featureSize: Int, labelSize: Int) = new LinearMulticlassClassifier(labelSize, featureSize)
}
