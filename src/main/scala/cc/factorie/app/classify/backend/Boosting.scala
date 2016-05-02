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

import cc.factorie.la.{DenseTensor1, Tensor1}
import cc.factorie.maths.ArrayOps
import cc.factorie.model.Template2
import cc.factorie.util.StoreFetchCubbie
import cc.factorie.variable.{LabeledMutableDiscreteVar, TensorVar}

import scala.reflect.ClassTag

class BoostedBinaryClassifier(val weakClassifiers: Seq[(BinaryClassifier[Tensor1], Double)]) extends BinaryClassifier[Tensor1] {
  def predict(features: Tensor1) = weakClassifiers.foldLeft(0.0)((acc, t) => acc + t._1.predict(features) * t._2)
}

// TODO this multiclass doesn't work for >2 classes right now - also add gradient boosting -luke
class BoostedMulticlassClassifier(var weakClassifiers: Seq[(MulticlassClassifier[Tensor1], Double)], val numLabels: Int) extends MulticlassClassifier[Tensor1] {
  def predict(features: Tensor1) =
    weakClassifiers.foldLeft(new DenseTensor1(numLabels))((acc, t) => {acc += (t._1.predict(features), t._2); acc})
  def asTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: ClassTag[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2[T](l2f, this)
}

class BoostingMulticlassTrainer(numWeakLearners: Int = 100, argTrainWeakLearner: MulticlassClassifierTrainer[MulticlassClassifier[Tensor1]] = null)(implicit random: scala.util.Random)
  extends MulticlassClassifierTrainer[BoostedMulticlassClassifier] {
  val trainWeakLearner = if (argTrainWeakLearner ne null) argTrainWeakLearner else new DecisionTreeMulticlassTrainer(treeTrainer = new DecisionStumpTrainer)

  def baseTrain(classifier: BoostedMulticlassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (BoostedMulticlassClassifier) => Unit) {
    classifier.weakClassifiers = AdaBoostTrainer.train(
      features, labels, classifier.numLabels, numIterations = numWeakLearners, trainWeakLearner.simpleTrain(classifier.numLabels, features.head.length, labels, features, _, c => {}))
     evaluate(classifier)
  }
  def newModel(featureSize: Int, labelSize: Int) = new BoostedMulticlassClassifier(null, labelSize)
}

// TODO add more ways of serializing sub-cubbies so that we can serialize different types of weak learners -luke
class BoostedTreeCubbie extends StoreFetchCubbie[BoostedMulticlassClassifier] {
  val numLabels = IntSlot("numLabels")
  val weakLearners = CubbieListSlot[DecisionTreeCubbie]("weakLearners", () => new DecisionTreeCubbie)
  val weights = DoubleListSlot("weights")
  numLabels := 0; weakLearners := Seq(); weights := Seq()
  def store(t: BoostedMulticlassClassifier): Unit = {
    weakLearners := t.weakClassifiers.map(_._1).map(wc => {val c = new DecisionTreeCubbie; c.store(wc.asInstanceOf[DecisionTreeMulticlassClassifier]); c})
    weights := t.weakClassifiers.map(_._2)
    numLabels := t.numLabels
  }
  def fetch(): BoostedMulticlassClassifier = {
    val weighted = weakLearners.value.map(_.fetch()).zip(weights.value)
    new BoostedMulticlassClassifier(weighted, numLabels.value)
  }
}

object AdaBoostTrainer {
  type WeakClassifier = MulticlassClassifier[Tensor1]
  def train(features: Seq[Tensor1], labels: Seq[Int], numLabels: Int, numIterations: Int, trainWeakClassifier: Seq[Double] => WeakClassifier): Seq[(WeakClassifier, Double)] = {
    val K = numLabels
    val numInstances = labels.length
    val instanceWeights = Array.fill(numInstances)(1.0 / numInstances)
    var converged = false
    var weightedClassifiers = List(): List[(WeakClassifier, Double)]
    var i = 0
    while (!converged) {
      val currentClassifier = trainWeakClassifier(instanceWeights)
      val classifications = features.map(v => currentClassifier.classification(v)).toArray
      val isFail = (0 until numInstances).map(i => classifications(i).bestLabelIndex != labels(i))
      val amountOfFail = (0 until numInstances).filter(isFail).foldLeft(0.0)((acc, el) => acc + instanceWeights(el))
      // FIXME: why doesn't this work multiclass? The log(K - 1) term should do this (see "Multi-class AdaBoost" by Zhu et al.) -luke
      val classifierWeight = math.log((1 - amountOfFail) / amountOfFail) + math.log(K - 1)
      for (i <- 0 until numInstances) instanceWeights(i) *= math.exp(if (isFail(i)) classifierWeight else 0)
      val dSum = ArrayOps.oneNorm(instanceWeights)
      for (i <- 0 until numInstances) instanceWeights(i) /= dSum
      weightedClassifiers = (currentClassifier, classifierWeight) :: weightedClassifiers
      converged = i > numIterations || amountOfFail == 0.0
      i += 1
    }
    if (i == 1) List((weightedClassifiers(0)._1, 1.0)) else weightedClassifiers
  }
}

