package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la.{DenseTensor2, Tensor1, DenseTensor1}

/**
 * User: apassos
 * Date: 6/12/13
 * Time: 3:54 PM
 */


trait BaseClassification[Pred] {
  def score: Pred
  def proportions: Proportions
}

trait BaseClassifier[Pred, Features] {
  def score(features: Features): Pred
  def classification(features: Features): BaseClassification[Pred]
}

class BinaryClassification(val score: Double) extends BaseClassification[Double] {
  lazy val proportions = {
    val t = new DenseTensor1(2)
    t(1) = LinearObjectives.logisticLinkFunction(score)
    t(0) = 1.0-t(1)
    new DenseTensorProportions1(t)
  }
  lazy val bestValue = score > 0
}

class MultiClassClassification(val score: Tensor1) extends BaseClassification[Tensor1] {
  lazy val proportions = new DenseTensorProportions1(score.expNormalized.asInstanceOf[Tensor1])
  lazy val bestLabelIndex = proportions.maxIndex
}

trait BaseBinaryClassifier[Features] extends BaseClassifier[Double, Features] {
  def classification(features: Features) = new BinaryClassification(score(features))
}

trait MultiClassClassifier[Features] extends BaseClassifier[Tensor1, Features] {
  def classification(features: Features) = new MultiClassClassification(score(features))
}

class LinearBinaryClassifier(val featureSize: Int) extends BaseBinaryClassifier[Tensor1] with Parameters {
  val weights = Weights(new DenseTensor1(featureSize))
  def score(features: Tensor1) = weights.value.dot(features)
}

class LinearMultiClassClassifier(val labelSize: Int, val featureSize: Int) extends MultiClassClassifier[Tensor1] with Parameters {
  self =>
  val weights = Weights(new DenseTensor2(labelSize, featureSize))
  def score(features: Tensor1) = weights.value * features
  def asTemplate[T <: LabeledMutableDiscreteVar[_]](l2f: T => TensorVar)(implicit ml: Manifest[T]) = new DotTemplateWithStatistics2[T,TensorVar] {
    def unroll1(v: T) = Factor(v, l2f(v))
    def unroll2(v: TensorVar) = Nil
    val weights = self.weights
  }
}

/**
 * Helper class to train a multiclass linear classifier.
 * @param optimizer The optimizer to use
 * @param useParallelTrainer Whether to use a parallel trainer
 * @param useOnlineTrainer Whether to use an online trainer
 * @param objective The objective function to use.
 * @param maxIterations The maximum number of passes to do on the training data.
 */
class MultiClassTrainer(val optimizer: GradientOptimizer,
                        val useParallelTrainer: Boolean,
                        val useOnlineTrainer: Boolean,
                        val objective: LinearObjectives.MultiClass,
                        val maxIterations: Int) {
  /**
   * The basic training method.
   * @param labelSize How many labels exist
   * @param featureSize How many features exist
   * @param labels A list of labels, integers between 0 and labelSize
   * @param features A list of feature vectors, one per label
   * @param weights A list of example weights, one per label
   * @param evaluate A function to be called after each training iteration
   * @return A trained classifier
   */
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: LinearMultiClassClassifier => Unit) = {
    val classifier = new LinearMultiClassClassifier(labelSize, featureSize)
    val examples = (0 until labels.length).map(i => new LinearMultiClassExample(classifier.weights, features(i), labels(i), objective, weight=weights(i)))
    Trainer.train(parameters=classifier.parameters, examples=examples, maxIterations=maxIterations, evaluate = () => evaluate(classifier), optimizer=optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=useOnlineTrainer)
    classifier
  }

  /**
   * Train from labels expressed as factorie variables
   * @param labels The labels
   * @param features The feature vector variables, one per label
   * @param weights The weights, one per label
   * @param testLabels Test labels, to evaluate on
   * @param testFeatures Test feature vectors, one per label
   * @return A trained classifier
   */
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[DiscreteTensorVar], weights: Seq[Double], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): LinearMultiClassClassifier = {
    val evaluate = (c: LinearMultiClassClassifier) => println(f"Test accuracy: ${testFeatures.map(i => c.classification(i.value.asInstanceOf[Tensor1]).bestLabelIndex).zip(testLabels).count(i => i._1 == i._2.targetIntValue).toDouble/testLabels.length}%1.4f")
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value.asInstanceOf[Tensor1]), weights, evaluate)
  }

  /**
   * Trains without example weights while evaluating on a test set.
   * @param labels The labels
   * @param features The feature vector variables, one per label
   * @param testLabels The test labels
   * @param testFeatures The test feature vector variables, one per test label
   * @return A trained classifier
   */
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[DiscreteTensorVar], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): LinearMultiClassClassifier =
    train(labels, features, labels.map(i => 1.0), testLabels, testFeatures)

  /**
   * Trains with example weights and a custom evaluation function
   * @param labels The labels
   * @param features The feature vector variables, one per label
   * @param weights The example weights, one per label
   * @param evaluate The evaluation function
   * @return A trained classifier
   */
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[DiscreteTensorVar], weights: Seq[Double], evaluate: LinearMultiClassClassifier => Unit): LinearMultiClassClassifier =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), weights, evaluate)

  /**
   * Train a classifier
   * @param labels The labels
   * @param features The feature vector variables, one per label
   * @param evaluate How to evaluate after each iteration of training
   * @return A trained classifier
   */
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[DiscreteTensorVar], evaluate: LinearMultiClassClassifier => Unit): LinearMultiClassClassifier =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), evaluate)

  /**
   * Simplest training function: no weights, just label and feature variables
   * @param labels The labels
   * @param features The feature variables, one per label
   * @return A trained classifier
   */
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[DiscreteTensorVar]): LinearMultiClassClassifier =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), c => ())
}

/**
 * A specialization MultiClassTrainer for online learning.
 * @param useParallel Whether to train in parallel
 * @param optimizer The optimizer to use
 * @param objective The objective function to use.
 * @param maxIterations The maximum number of passes to do on the training data.
 */
class OnlineMultiClassTrainer(useParallel:Boolean = false,
                              optimizer: GradientOptimizer = new AdaGrad with ParameterAveraging,
                              objective: LinearObjectives.MultiClass = LinearObjectives.sparseLogMultiClass,
                              maxIterations: Int = 3)
  extends MultiClassTrainer(optimizer, useParallel, useOnlineTrainer = true, objective, maxIterations) {}

/**
 * A specialization of MultiClassTrainer for batch learning.
 * @param useParallel Whether to train in parallel
 * @param optimizer The optimizer to use
 * @param objective The objective function to use.
 * @param maxIterations The maximum number of passes to do on the training data.
 */
class BatchMultiClassTrainer(useParallel:Boolean = true,
                             optimizer: GradientOptimizer = new LBFGS with L2Regularization,
                             objective: LinearObjectives.MultiClass = LinearObjectives.sparseLogMultiClass,
                             maxIterations: Int = 200)
  extends MultiClassTrainer(optimizer, useParallel, useOnlineTrainer = false, objective, maxIterations) {}

