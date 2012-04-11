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
import cc.factorie.er._
import cc.factorie.la.Vector

class MaxEntSampleRankTrainer extends ClassifierTrainer {
  var iterations = 10
  var learningRateDecay = 0.9
  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures)
    val learner = new VariableSettingsSampler[L](cmodel, HammingLossObjective) with SampleRank with GradientAscentUpdates {
      override def pickProposal(proposals:Seq[Proposal]): Proposal = proposals.head // which proposal is picked is irrelevant, so make it quick
    }
    learner.learningRate = 1.0
    for (i <- 0 until iterations) {
      learner.processAll(il)
      learner.learningRate *= learningRateDecay
    }
    new Classifier[L] { val model = cmodel; val labelDomain = il.head.domain }
  }
}

// => Train
class MaxEntLikelihoodTrainer(val l2: Double = 10.0, val warmStart: Vector = null, modelFile: String = null) extends ClassifierTrainer {
  def trainer(): LogLinearMaximumLikelihood
  // => apply()
  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures)
    if (warmStart != null) cmodel.evidenceTemplate.setWeights(warmStart)
    val trainer = trainer() // new LogLinearMaximumLikelihood(cmodel, modelFile = modelFile)
    trainer.gaussianPriorVariance = l2
    // Do the training by BFGS
    trainer.processAll(il.map(List(_)))
    new Classifier[L] { val model = cmodel; val labelDomain = il.head.domain; val weights = cmodel.evidenceTemplate.weights }
  }
}

class LibLinearTrainer(val l2: Double = 10.0) extends ClassifierTrainer { // TODO: actually use this l2
  import de.bwaldvogel.liblinear

  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L] = {
    val p = new liblinear.Problem

    p.bias = -1.0 // no bias
    //val biasIdx = il.featureDomain.size
    
    p.l = il.size
    p.n = il.featureDomain.size // +1 for bias

    println("liblinear: filling feature vectors")
    def makeLibLinearFeatureVector(label: L): Array[liblinear.FeatureNode] = {
      val featureVector = label.vector
      val fnArr = Array.fill[liblinear.FeatureNode](featureVector.activeDomainSize)(null) // +1 for bias, if used
      var j = 0
      for ((idx, value) <- featureVector.activeElements) {
        fnArr(j) = new liblinear.FeatureNode(idx + 1, value) // 1-indexing, gahh!
        j += 1
      }
      //fnArr(j) = new liblinear.FeatureNode(biasIdx + 1, p.bias)
      fnArr
    }
    
    p.x = Array.fill[Array[liblinear.FeatureNode]](il.size)(null)
    for ((l,i) <- il.zipWithIndex)
      p.x(i) = makeLibLinearFeatureVector(l)

    println("liblinear: filling target value vector")
    p.y = Array.fill[Int](p.l)(-1)
    for ((l, i) <- il.zipWithIndex)
      p.y(i) = l.targetIntValue

    println("liblinear: starting training")
    val m = liblinear.Linear.train(p, new liblinear.Parameter(liblinear.SolverType.L2R_LR, 1, .1))
    println("liblinear: finished training")

    println("liblinear: done")
    new Classifier[L] {
      val labelDomain = il.labelDomain
      val model = new TemplateModel
      override def classify(label:L): Classification[L] = {
        require(label.domain eq labelDomain)
        val probs = Array.fill[Double](labelDomain.size)(0.0)
        liblinear.Linear.predictProbability(m, makeLibLinearFeatureVector(label), probs)
        val proportions = Array.fill[Double](labelDomain.size)(0.0)
        label.settings.foreach(_ => proportions(label.intValue) = probs(label.intValue))

        val result = new Classification(label, model, new generative.DenseProportions(proportions))
        label.set(result.bestLabelIndex)(null)
        result
      }
    }
  }

}
