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
import cc.factorie.generative.Proportions
import cc.factorie.er._
import scala.collection.mutable.{HashMap,ArrayBuffer}

class MaxEntSampleRankTrainer extends ClassifierTrainer {
  var iterations = 10
  var learningRateDecay = 0.9
  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures)
    val learner = new VariableSettingsSampler[L](cmodel, HammingLossObjective) with SampleRank with GradientAscentUpdates
    learner.learningRate = 1.0
    for (i <- 0 until iterations) {
      learner.processAll(il)
      learner.learningRate *= learningRateDecay
    }
    new Classifier[L] { val model = cmodel; val labelDomain = il.head.domain }
  }
}

class MaxEntLikelihoodTrainer extends ClassifierTrainer {
  def train[L<:LabelVariable[_]](il:LabelList[L])(implicit lm:Manifest[L]): Classifier[L] = {
    val cmodel = new LogLinearModel(il.labelToFeatures)
    val trainer = new LogLinearMaximumLikelihood(cmodel)
    // Do the training by BFGS
    trainer.processAll(il.map(List(_)))
    new Classifier[L] { val model = cmodel; val labelDomain = il.head.domain }
  }
}
