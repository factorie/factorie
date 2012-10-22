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
import cc.factorie.optimize._
import la.{Tensor1, Tensor}
import scala.collection.mutable.{HashMap, ArrayBuffer}

class MaxEntSampleRankTrainer extends ClassifierTrainer {
  var iterations = 10
  var learningRateDecay = 0.9
  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteTensorVar](il: LabelList[L, F]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures, il.labelDomain, il.instanceDomain)(il.labelManifest, il.featureManifest)
    val sampler = new GibbsSampler(cmodel, HammingLossObjective) {
      override def pickProposal(proposals: Seq[Proposal]): Proposal = proposals.head // which proposal is picked is irrelevant, so make it quick
    }
    val learner = new SampleRank(sampler, new MIRA)
    learner.processAll(il, iterations)
    new ModelBasedClassifier[L](cmodel, il.head.domain)
  }
}

class MaxEntLikelihoodTrainer(val l2: Double = 10.0, val warmStart: Tensor = null) extends ClassifierTrainer {
  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteTensorVar](il: LabelList[L, F]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures, il.labelDomain, il.instanceDomain)(il.labelManifest, il.featureManifest)
    val pieces = il.map(l => new GLMPiece(
      il.labelToFeatures(l).tensor.asInstanceOf[Tensor1],
      l.intValue,
      LossFunctions.logMultiClassLoss,
      weight = il.instanceWeight(l)))
    if (warmStart != null) cmodel.evidenceTemplate.weights := warmStart
    // Do the training by BFGS
    val lbfgs = new L2RegularizedLBFGS(l2 = 1 / l2)
    val strategy = new BatchTrainer(lbfgs, new ModelWithWeightsImpl(cmodel))
    while (!strategy.isConverged)
      strategy.process(pieces)
    new ModelBasedClassifier[L](cmodel, il.head.domain) {val weights = cmodel.evidenceTemplate.weights}
  }
}

// The default trainer
class MaxEntTrainer extends MaxEntLikelihoodTrainer()
