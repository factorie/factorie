package cc.factorie.app.classify.backend

import cc.factorie._
import cc.factorie.la._
import cc.factorie.util.{Threading, TensorCubbie, Cubbie}
import cc.factorie.variable._
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, Template2}
import cc.factorie.optimize._
import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1
import cc.factorie.la.Tensor2
import cc.factorie.la.DenseTensor2

// "Classifier"s take "values" of variables as input rather than the variables themselves.
// This is in contrast to "Classifier"s, which take variables as input.

/** A record of the result of applying a Classifier.
    @author Alexandre Passos */
trait Classification[Pred] {
  def score: Pred
  def proportions: Proportions1
}

trait ClassificationModel[Pred, Features] {
  def score(features: Features): Pred
}

trait LinearModel[Pred, Features] extends ClassificationModel[Pred, Features] {
  def accumulateStats(accumulator: WeightsMapAccumulator, features: Features, gradient: Pred): Unit
}

trait Classifier[Pred, Features] extends ClassificationModel[Pred, Features] {
  def classification(features: Features): Classification[Pred]
}

class BinaryClassification(val score: Double) extends Classification[Double] {
  lazy val proportions = {
      val t = new DenseTensor1(2)
      t(1) = LinearObjectives.logisticLinkFunction(score)
      t(0) = 1.0-t(1)
      new DenseTensorProportions1(t)
  }
  lazy val bestBoolean: Boolean = score > 0
}

class MulticlassClassification(val score: Tensor1) extends Classification[Tensor1] {
  lazy val proportions = new DenseTensorProportions1(score.expNormalized.asInstanceOf[Tensor1])
  lazy val bestLabelIndex = score.maxIndex
}

trait BinaryClassifier[Features] extends Classifier[Double, Features] {
  def classification(features: Features) = new BinaryClassification(score(features))
}

class ClassifierTemplate[Features,Value<:DiscreteValue,T<: LabeledMutableDiscreteVar, F<:Var { type Value = Features }](classifier: Classifier[Tensor1,Features], l2f: T => F)(implicit ml: Manifest[T], implicit val mf: Manifest[F]) extends Template2[T,F] {
  def unroll1(v: T) = Factor(v, l2f(v))
  def unroll2(v: F) = Nil
  def score(v1: T#Value, v2: Features): Double = classifier.classification(v2).score(v1.asInstanceOf[DiscreteValue].intValue)
}

trait MulticlassClassifier[Features] extends Classifier[Tensor1, Features] {
  def classification(features: Features) = new MulticlassClassification(score(features))
  def asTemplate[Value<:DiscreteValue, T <: LabeledMutableDiscreteVar, F<: Var { type Value = Features }](l2f: T => F)(implicit ml: Manifest[T], mf: Manifest[F]) = new ClassifierTemplate[Features,Value,T,F](MulticlassClassifier.this, l2f)
}

trait BaseLinearTrainer[Features,Prediction,Label, C<:ClassificationModel[Prediction,Features]] {
  def newModel(featureSize: Int, labelSize: Int): C
  /** Estimate the parameters of a classifier that has already been created. */
  def baseTrain(classifier: C, labels: Seq[Label], features: Seq[Features], weights: Seq[Double], evaluate: C => Unit)
  /** Create a Classifier and estimate its parameters. */
  final def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Label], features: Seq[Features], weights: Seq[Double], evaluate: C => Unit): C = {
    val model = newModel(featureSize, labelSize)
    baseTrain(model, labels, features, weights, evaluate)
    model
  }
}

trait OptimizingBaseLinearTrainer[Features,Prediction,Label, C<:LinearModel[Prediction,Features] with Parameters] extends BaseLinearTrainer[Features,Prediction,Label,C] {
  def objective: LinearObjective[Prediction,Label]
  def maxIterations: Int
  def optimizer: GradientOptimizer
  def useParallelTrainer: Boolean
  def useOnlineTrainer: Boolean
  def miniBatch: Int
  def nThreads: Int
  implicit def random: scala.util.Random
  final def baseTrain(classifier: C, labels: Seq[Label], features: Seq[Features], weights: Seq[Double], evaluate: C => Unit) {
    val examples = (0 until labels.length).map(i => new LinearExample(classifier, features(i), labels(i), objective, weight=weights(i)))
    Trainer.train(parameters=classifier.parameters, examples=examples, maxIterations=maxIterations, evaluate = () => evaluate(classifier), optimizer=optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=useOnlineTrainer, miniBatch=miniBatch, nThreads=nThreads)
  }
}

/** Base trait for training multi-class classifiers, but it requires the input to be a Tensor1 feature vector. */
trait MulticlassClassifierTrainer[C <: MulticlassClassifier[Tensor1]] extends BaseLinearTrainer[Tensor1,Tensor1,Int,C] {
  // Various methods for creating and training a classifier
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): C= {
    val evaluate = (c: C) => println(f"Test accuracy: ${testFeatures.map(i => c.classification(i.value.asInstanceOf[Tensor1]).bestLabelIndex)
                                                                                         .zip(testLabels).count(i => i._1 == i._2.target.intValue).toDouble/testLabels.length}%1.4f")
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.target.intValue), features.map(_.value), weights, evaluate)
  }
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): C =
    train(labels, features, labels.map(i => 1.0), testLabels, testFeatures)
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double]): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.target.intValue), features.map(_.value), weights, c => ())
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar]): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.target.intValue), features.map(_.value), labels.map(i => 1.0), c => ())
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], evaluate: C => Unit): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.target.intValue), features.map(_.value), weights, evaluate)
  def train(labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], evaluate: C => Unit): C =
    simpleTrain(labels.head.domain.size, features.head.domain.dimensionSize, labels.map(_.target.intValue), features.map(_.value), labels.map(i => 1.0), evaluate)
  def train[Label<:LabeledDiscreteVar](labels: Seq[Label], l2f: Label => VectorVar, testLabels: Seq[Label], l2w: Label => Double = (l: Label) => 1.0): C =
    train(labels, labels.map(l2f), labels.map(l2w), testLabels, testLabels.map(l2f))
  def train[Label<:LabeledDiscreteVar](labels: Seq[Label], l2f: Label => VectorVar, l2w: Label => Double): C =
    train(labels, labels.map(l2f), labels.map(l2w))

  // Various methods for training an already-created classifier
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]) {
    val evaluate = (c: C) => println(f"Test accuracy: ${testFeatures.map(i => c.classification(i.value.asInstanceOf[Tensor1]).bestLabelIndex)
                                                                                         .zip(testLabels).count(i => i._1 == i._2.target.intValue).toDouble/testLabels.length}%1.4f")
    baseTrain(classifier, labels.map(_.target.intValue), features.map(_.value), weights, evaluate)
  }
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], testLabels: Seq[LabeledDiscreteVar], testFeatures: Seq[TensorVar]): Unit =
    train(classifier, labels, features, labels.map(i => 1.0), testLabels, testFeatures)
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double]): Unit =
    baseTrain(classifier, labels.map(_.target.intValue), features.map(_.value), weights, c => ())
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar]): Unit =
    baseTrain(classifier, labels.map(_.target.intValue), features.map(_.value), labels.map(i => 1.0), c => ())
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], weights: Seq[Double], evaluate: C => Unit): Unit =
    baseTrain(classifier, labels.map(_.target.intValue), features.map(_.value), weights, evaluate)
  def train(classifier: C, labels: Seq[LabeledDiscreteVar], features: Seq[VectorVar], evaluate: C => Unit): Unit =
    baseTrain(classifier, labels.map(_.target.intValue), features.map(_.value), labels.map(i => 1.0), evaluate)
  def train[Label<:LabeledDiscreteVar](classifier: C, labels: Seq[Label], l2f: Label => VectorVar, testLabels: Seq[Label], l2w: Label => Double = (l: Label) => 1.0): Unit =
    train(classifier, labels, labels.map(l2f), labels.map(l2w), testLabels, testLabels.map(l2f))
  def train[Label<:LabeledDiscreteVar](classifier: C, labels: Seq[Label], l2f: Label => VectorVar, l2w: Label => Double): Unit =
    train(classifier, labels, labels.map(l2f), labels.map(l2w))
}

class ClassifierTemplate2[T <: DiscreteVar](l2f: T => TensorVar, classifier: MulticlassClassifier[Tensor1])(implicit ml: Manifest[T], mf: Manifest[TensorVar]) extends Template2[T, TensorVar] {
  def unroll1(v: T) = Factor(v, l2f(v))
  def unroll2(v: TensorVar) = Nil
  def score(v1: T#Value, v2: TensorVar#Value): Double = classifier.score(v2.asInstanceOf[Tensor1])(v1.asInstanceOf[DiscreteValue].intValue)
}

class LinearBinaryClassifier(val featureSize: Int) extends BinaryClassifier[Tensor1] with Parameters with LinearModel[Double, Tensor1] {
  val weights = Weights(new DenseTensor1(featureSize))
  def score(features: Tensor1) = weights.value.dot(features)
  def accumulateStats(accumulator: WeightsMapAccumulator, features: Tensor1, gradient: Double) = accumulator.accumulate(weights, features, gradient)
}

class LinearMulticlassClassifier(val labelSize: Int, val featureSize: Int) extends MulticlassClassifier[Tensor1] with Parameters with LinearModel[Tensor1,Tensor1] {
  val weights = Weights(new DenseTensor2(featureSize, labelSize))
  def score(features: Tensor1): Tensor1 = weights.value.leftMultiply(features)
  def accumulateStats(accumulator: WeightsMapAccumulator, features: Tensor1, gradient: Tensor1) = accumulator.accumulate(weights, features outer gradient)
  def asDotTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: Manifest[T]) = new DotTemplateWithStatistics2[T,TensorVar] {
    def unroll1(v: T) = Factor(v, l2f(v))
    def unroll2(v: TensorVar) = Nil
    val weights = LinearMulticlassClassifier.this.weights
  }
}

class LinearMulticlassClassifierCubbie extends Cubbie {
  val labelSize = IntSlot("labelSize")
  val featureSize = IntSlot("featureSize")
  val parameters = CubbieSlot("parameters", () => new TensorCubbie[Tensor2])
  store(new LinearMulticlassClassifier(1, 1))

  def store(model: LinearMulticlassClassifier) {
    labelSize := model.labelSize
    featureSize := model.featureSize
    parameters := new TensorCubbie[Tensor2]
    parameters.value.store(model.weights.value)
  }
  def fetch: LinearMulticlassClassifier = {
    val model = new LinearMulticlassClassifier(labelSize.value, featureSize.value)
    model.weights.set(parameters.value.fetch())
    model
  }
}

class LinearMulticlassTrainer(val optimizer: GradientOptimizer,
                        val useParallelTrainer: Boolean,
                        val useOnlineTrainer: Boolean,
                        val objective: LinearObjectives.Multiclass,
                        val maxIterations: Int,
                        val miniBatch: Int,
                        val nThreads: Int)(implicit val random: scala.util.Random) extends MulticlassClassifierTrainer[LinearMulticlassClassifier] with OptimizingBaseLinearTrainer[Tensor1, Tensor1, Int, LinearMulticlassClassifier]{
  def newModel(featureSize: Int, labelSize: Int) = new LinearMulticlassClassifier(labelSize, featureSize)
}

class SVMMulticlassTrainer(nThreads: Int = 1)(implicit val random: scala.util.Random) extends MulticlassClassifierTrainer[LinearMulticlassClassifier] {
  def newModel(featureSize: Int, labelSize: Int) = new LinearMulticlassClassifier(labelSize, featureSize)
  def baseTrain(classifier: LinearMulticlassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: LinearMulticlassClassifier => Unit) {
    val ll = labels.toArray
    val ff = features.toArray
    val numLabels = classifier.weights.value.dim2
    val weightTensor = Threading.parMap(0 until numLabels, nThreads) { label => (new LinearL2SVM).train(ff, ll, label) }
    val weightsValue = classifier.weights.value
    for (f <- 0 until weightsValue.dim1; (l,t) <- (0 until numLabels).zip(weightTensor)) {
      weightsValue(f,l) = t(f)
    }
    evaluate(classifier)
  }
}

class OnlineLinearMulticlassTrainer(useParallel:Boolean = false,
                              optimizer: GradientOptimizer = new AdaGrad with ParameterAveraging,
                              objective: LinearObjectives.Multiclass = LinearObjectives.sparseLogMulticlass,
                              maxIterations: Int = 3,
                              miniBatch: Int = -1,
                              nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends LinearMulticlassTrainer(optimizer, useParallel, useOnlineTrainer = true, objective, maxIterations, miniBatch, nThreads) {}

class BatchLinearMulticlassTrainer(useParallel:Boolean = true,
                             optimizer: GradientOptimizer = new LBFGS with L2Regularization,
                             objective: LinearObjectives.Multiclass = LinearObjectives.sparseLogMulticlass,
                             maxIterations: Int = 200,
                             nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends LinearMulticlassTrainer(optimizer, useParallel, useOnlineTrainer = false, objective, maxIterations, -1, nThreads) {}

