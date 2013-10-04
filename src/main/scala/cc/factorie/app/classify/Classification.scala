package cc.factorie.app.classify

import cc.factorie._
import cc.factorie.la.{Tensor2, DenseTensor2, Tensor1, DenseTensor1}
import cc.factorie.util.{TensorCubbie, Cubbie}
import cc.factorie.variable._
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, Template2}
import cc.factorie.optimize._

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
  lazy val bestLabelIndex = score.maxIndex
}

trait BaseBinaryClassifier[Features] extends BaseClassifier[Double, Features] {
  def classification(features: Features) = new BinaryClassification(score(features))
}

class ClassifierTemplate[Features,Value<:DiscreteValue,T<: LabeledMutableDiscreteVar, F<:Var { type Value = Features }](classifier: BaseClassifier[Tensor1,Features], l2f: T => F)(implicit ml: Manifest[T], implicit val mf: Manifest[F]) extends Template2[T,F] {
  def unroll1(v: T) = Factor(v, l2f(v))
  def unroll2(v: F) = Nil
  def score(v1: T#Value, v2: Features): Double = classifier.classification(v2).score(v1.asInstanceOf[DiscreteValue].intValue)
}

trait MultiClassClassifier[Features] extends BaseClassifier[Tensor1, Features] {
  def classification(features: Features) = new MultiClassClassification(score(features))
  def asTemplate[Value<:DiscreteValue,T <: LabeledMutableDiscreteVar,F<: Var { type Value = Features }](l2f: T => F)(implicit ml: Manifest[T], mf: Manifest[F]) = new ClassifierTemplate[Features,Value,T,F](this, l2f)
}

trait MultiClassTrainerBase[C <: MultiClassClassifier[Tensor1]] {
  def baseTrain(classifier: C, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: C => Unit)
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: C => Unit): C

  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): C= {
    val evaluate = (c: C) => println(f"Test accuracy: ${testFeatures.map(i => c.classification(i.value.asInstanceOf[Tensor1]).bestLabelIndex)
                                                                                         .zip(testLabels).count(i => i._1 == i._2.targetIntValue).toDouble/testLabels.length}%1.4f")
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), weights, evaluate)
  }
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): C =
    train(labels, features, labels.map(i => 1.0), testLabels, testFeatures)
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double]): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), weights, c => ())
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar]): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), c => ())
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], evaluate: C => Unit): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), weights, evaluate)
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], evaluate: C => Unit): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), evaluate)
  def train[Label<:LabeledDiscreteVar](labels: Seq[Label], l2f: Label => VectorVar, testLabels: Seq[Label], l2w: Label => Double = (l: Label) => 1.0): C =
    train(labels, labels.map(l2f), labels.map(l2w), testLabels, testLabels.map(l2f))
  def train[Label<:LabeledDiscreteVar](labels: Seq[Label], l2f: Label => VectorVar, l2w: Label => Double): C =
    train(labels, labels.map(l2f), labels.map(l2w))


  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]) {
    val evaluate = (c: C) => println(f"Test accuracy: ${testFeatures.map(i => c.classification(i.value.asInstanceOf[Tensor1]).bestLabelIndex)
                                                                                         .zip(testLabels).count(i => i._1 == i._2.targetIntValue).toDouble/testLabels.length}%1.4f")
    baseTrain(classifier, labels.map(_.targetIntValue), features.map(_.value), weights, evaluate)
  }
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): Unit =
    train(classifier, labels, features, labels.map(i => 1.0), testLabels, testFeatures)
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double]): Unit =
    baseTrain(classifier, labels.map(_.targetIntValue), features.map(_.value), weights, c => ())
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar]): Unit =
    baseTrain(classifier, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), c => ())
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], evaluate: C => Unit): Unit =
    baseTrain(classifier, labels.map(_.targetIntValue), features.map(_.value), weights, evaluate)
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], evaluate: C => Unit): Unit =
    baseTrain(classifier, labels.map(_.targetIntValue), features.map(_.value), labels.map(i => 1.0), evaluate)
  def train[Label<:LabeledDiscreteVar](classifier: C, labels: Seq[Label], l2f: Label => VectorVar, testLabels: Seq[Label], l2w: Label => Double = (l: Label) => 1.0): Unit =
    train(classifier, labels, labels.map(l2f), labels.map(l2w), testLabels, testLabels.map(l2f))
  def train[Label<:LabeledDiscreteVar](classifier: C, labels: Seq[Label], l2f: Label => VectorVar, l2w: Label => Double): Unit =
    train(classifier, labels, labels.map(l2f), labels.map(l2w))
}

class ClassifierTemplate2[T <: DiscreteVar](l2f: T => TensorVar, classifier: MultiClassClassifier[Tensor1])(implicit ml: Manifest[T], mf: Manifest[TensorVar]) extends Template2[T, TensorVar] {
  def unroll1(v: T) = Factor(v, l2f(v))
  def unroll2(v: TensorVar) = Nil
  def score(v1: T#Value, v2: TensorVar#Value): Double = classifier.score(v2.asInstanceOf[Tensor1])(v1.asInstanceOf[DiscreteValue].intValue)
}

class LinearBinaryClassifier(val featureSize: Int) extends BaseBinaryClassifier[Tensor1] with Parameters {
  val weights = Weights(new DenseTensor1(featureSize))
  def score(features: Tensor1) = weights.value.dot(features)
}

class LinearMultiClassClassifier(val labelSize: Int, val featureSize: Int) extends MultiClassClassifier[Tensor1] with Parameters {
  self =>
  val weights = Weights(new DenseTensor2(featureSize, labelSize))
  def score(features: Tensor1) = weights.value.leftMultiply(features)
  def asDotTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: Manifest[T]) = new DotTemplateWithStatistics2[T,TensorVar] {
    def unroll1(v: T) = Factor(v, l2f(v))
    def unroll2(v: TensorVar) = Nil
    val weights = self.weights
  }
}

class LinearMultiClassClassifierCubbie extends Cubbie {
  val labelSize = IntSlot("labelSize")
  val featureSize = IntSlot("featureSize")
  val parameters = CubbieSlot("parameters", () => null.asInstanceOf[TensorCubbie[Tensor2]])
  store(new LinearMultiClassClassifier(1, 1))

  def store(model: LinearMultiClassClassifier) {
    labelSize := model.labelSize
    featureSize := model.featureSize
    parameters := new TensorCubbie[Tensor2]
    parameters.value.store(model.weights.value)
  }
  def fetch: LinearMultiClassClassifier = {
    val model = new LinearMultiClassClassifier(labelSize.value, featureSize.value)
    model.weights.set(parameters.value.fetch())
    model
  }
}

class LinearMultiClassTrainer(val optimizer: GradientOptimizer,
                        val useParallelTrainer: Boolean,
                        val useOnlineTrainer: Boolean,
                        val objective: LinearObjectives.MultiClass,
                        val maxIterations: Int,
                        val miniBatch: Int,
                        val nThreads: Int)(implicit random: scala.util.Random) extends MultiClassTrainerBase[LinearMultiClassClassifier] {
  def baseTrain(classifier: LinearMultiClassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: LinearMultiClassClassifier => Unit) {
    val examples = (0 until labels.length).map(i => new LinearMultiClassExample(classifier.weights, features(i), labels(i), objective, weight=weights(i)))
    Trainer.train(parameters=classifier.parameters, examples=examples, maxIterations=maxIterations, evaluate = () => evaluate(classifier), optimizer=optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=useOnlineTrainer, miniBatch=miniBatch, nThreads=nThreads)
  }
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: LinearMultiClassClassifier => Unit) = {
    val classifier = new LinearMultiClassClassifier(labelSize, featureSize)
    baseTrain(classifier, labels, features, weights, evaluate)
    classifier
  }
}

class SVMMultiClassTrainer(parallel: Boolean=false)(implicit random: scala.util.Random) extends LinearMultiClassTrainer(optimizer=null, useParallelTrainer=parallel, useOnlineTrainer=false, objective=null, miniBatch= -1, maxIterations= -1, nThreads= -1) {
  override def baseTrain(classifier: LinearMultiClassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: LinearMultiClassClassifier => Unit) {
    val ll = labels.toArray
    val ff = features.toArray
    val numLabels = classifier.weights.value.dim1
    val weightTensor = {
      if (parallel) (0 until numLabels).par.map { label => (new LinearL2SVM).train(ff, ll, label) }
      else (0 until numLabels).map { label => (new LinearL2SVM).train(ff, ll, label) }
    }
    val weightsValue = classifier.weights.value
    for (f <- 0 until weightsValue.dim2; (l,t) <- (0 until numLabels).zip(weightTensor)) {
      weightsValue(f,l) = t(f)
    }
    evaluate(classifier)
  }
}

class OnlineLinearMultiClassTrainer(useParallel:Boolean = false,
                              optimizer: GradientOptimizer = new AdaGrad with ParameterAveraging,
                              objective: LinearObjectives.MultiClass = LinearObjectives.sparseLogMultiClass,
                              maxIterations: Int = 3,
                              miniBatch: Int = -1,
                              nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends LinearMultiClassTrainer(optimizer, useParallel, useOnlineTrainer = true, objective, maxIterations, miniBatch, nThreads) {}

class BatchLinearMultiClassTrainer(useParallel:Boolean = true,
                             optimizer: GradientOptimizer = new LBFGS with L2Regularization,
                             objective: LinearObjectives.MultiClass = LinearObjectives.sparseLogMultiClass,
                             maxIterations: Int = 200,
                             nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends LinearMultiClassTrainer(optimizer, useParallel, useOnlineTrainer = false, objective, maxIterations, -1, nThreads) {}

