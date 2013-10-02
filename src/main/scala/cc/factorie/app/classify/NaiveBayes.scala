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
import cc.factorie.la.Tensor1
import cc.factorie.variable.DenseProportions1

class NaiveBayes(var evidenceSmoothingMass: Double = 1.0) extends MultiClassTrainerBase[LinearMultiClassClassifier]  {
  def baseTrain(classifier: LinearMultiClassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (LinearMultiClassClassifier) => Unit) {
    val numLabels = classifier.labelSize
    val numFeatures = classifier.featureSize
    // assuming the existence of a bias feature
    val evid = Seq.tabulate(numLabels)(i => new DenseProportions1(numFeatures))
    // Note: this doesn't actually build the graphical model, it just gathers smoothed counts, never creating factors
    // Incorporate smoothing, with simple +m smoothing
    for (li <- 0 until numLabels; fi <- 0 until numFeatures) evid(li).masses += (fi, evidenceSmoothingMass)
    // Incorporate evidence
    for (i <- 0 until labels.length; label = labels(i); feature = features(i); w = weights(i)) {
      feature.foreachActiveElement((featureIndex, featureValue) => {
        evid(label).masses += (featureIndex, w*featureValue)
      })
    }
    // Put results into the model templates
    val evWeightsValue = classifier.weights.value
    for (li <- 0 until numLabels; fi <- 0 until numFeatures)
      evWeightsValue(li * numFeatures + fi) = math.log(evid(li).apply(fi))
  }

  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (LinearMultiClassClassifier) => Unit) = {
    val classifier = new LinearMultiClassClassifier(labelSize, featureSize)
    baseTrain(classifier, labels, features, weights, evaluate)
    classifier
  }
}
