package cc.factorie.app.classify

import cc.factorie.la.{SingletonBinaryTensor1, DenseTensor2, DenseTensor1, Tensor1}
import cc.factorie._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.maths.ArrayOps
import cc.factorie.util.StoreFetchCubbie
import cc.factorie.variable.{TensorVar, LabeledMutableDiscreteVar}
import cc.factorie.model.Template2

class BoostedBinaryClassifier(val weakClassifiers: Seq[(BaseBinaryClassifier[Tensor1], Double)]) extends BaseBinaryClassifier[Tensor1] {
  def score(features: Tensor1) = weakClassifiers.foldLeft(0.0)((acc, t) => acc + t._1.score(features) * t._2)
}

// TODO this multiclass doesn't work for >2 classes right now - also add gradient boosting -luke
class BoostedMultiClassClassifier(var weakClassifiers: Seq[(MultiClassClassifier[Tensor1], Double)], val numLabels: Int) extends MultiClassClassifier[Tensor1] {
  def score(features: Tensor1) =
    weakClassifiers.foldLeft(new DenseTensor1(numLabels))((acc, t) => {acc += (t._1.score(features), t._2); acc})
  def asTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: Manifest[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2[T](l2f, this)
}

class BoostingMultiClassTrainer(numWeakLearners: Int = 100, argTrainWeakLearner: MultiClassTrainerBase[MultiClassClassifier[Tensor1]] = null)(implicit random: scala.util.Random)
  extends MultiClassTrainerBase[BoostedMultiClassClassifier] {
  val trainWeakLearner = if (argTrainWeakLearner ne null) argTrainWeakLearner else new DecisionTreeMultiClassTrainer(treeTrainer = new DecisionStumpTrainer)

  def baseTrain(classifier: BoostedMultiClassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (BoostedMultiClassClassifier) => Unit) {
    classifier.weakClassifiers = AdaBoostTrainer.train(
      features, labels, classifier.numLabels, numIterations = numWeakLearners, trainWeakLearner.simpleTrain(classifier.numLabels, features.head.length, labels, features, _, c => {}))
     evaluate(classifier)
  }

  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: BoostedMultiClassClassifier => Unit): BoostedMultiClassClassifier = {
    val weightedWeakLearners = AdaBoostTrainer.train(
      features, labels, labelSize, numIterations = numWeakLearners, trainWeakLearner.simpleTrain(labelSize, featureSize, labels, features, _, c => {}))
    val classifier = new BoostedMultiClassClassifier(weightedWeakLearners, labelSize)
    evaluate(classifier)
    classifier
  }
}

// TODO add more ways of serializing sub-cubbies so that we can serialize different types of weak learners -luke
class BoostedTreeCubbie extends StoreFetchCubbie[BoostedMultiClassClassifier] {
  val numLabels = IntSlot("numLabels")
  val weakLearners = CubbieListSlot[DecisionTreeCubbie]("weakLearners", () => new DecisionTreeCubbie)
  val weights = DoubleListSlot("weights")
  numLabels := 0; weakLearners := Seq(); weights := Seq()
  def store(t: BoostedMultiClassClassifier): Unit = {
    weakLearners := t.weakClassifiers.map(_._1).map(wc => {val c = new DecisionTreeCubbie; c.store(wc.asInstanceOf[DecisionTreeMultiClassClassifier]); c})
    weights := t.weakClassifiers.map(_._2)
    numLabels := t.numLabels
  }
  def fetch(): BoostedMultiClassClassifier = {
    val weighted = weakLearners.value.map(_.fetch()).zip(weights.value)
    new BoostedMultiClassClassifier(weighted, numLabels.value)
  }
}

object AdaBoostTrainer {
  type WeakClassifier = MultiClassClassifier[Tensor1]
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

