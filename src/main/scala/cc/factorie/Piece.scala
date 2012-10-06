package cc.factorie

import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.util._
import app.classify
import cc.factorie.la._
import classify.{ModelBasedClassifier, LogLinearModel}
import scala.collection.mutable._
import collection.parallel.mutable.ParSeq
import collection.GenSeq
import java.io.File
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: Alexandre Passos, Luke Vilnis
 * Date: 10/5/12
 * Time: 11:13 AM
 * To change this template use File | Settings | File Templates.
 */
//trait PieceState { def merge(other: PieceState): PieceState }

// Pieces are thread safe
trait Piece {
  def accumulateValueAndGradient(model: Model, gradient: TensorAccumulator, value: DoubleAccumulator): Unit
  def accumulateGradient(model: Model, gradient: TensorAccumulator): Unit = accumulateValueAndGradient(model, gradient, NoopDoubleAccumulator)
  def accumulateValue(model: Model, value: DoubleAccumulator): Unit = accumulateValueAndGradient(model, NoopTensorAccumulator, value)
  //def state: PieceState
  //def updateState(state: PieceState)
}

trait PiecewiseLearner {
  def model: Model
  def process(pieces: GenSeq[Piece]): Unit
}

class BatchPiecewiseLearner(val optimizer: GradientOptimizer, val model: Model) extends PiecewiseLearner {
  val gradient = model.weightsTensor.copy
  val gradientAccumulator = new LocalTensorAccumulator(gradient.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  def process(pieces: GenSeq[Piece]): Unit = {
    gradient.zero()
    valueAccumulator.value = 0.0
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit
    pieces /*.par*/ .foreach(piece => piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator))
    println("Gradient: " + gradient + "\nLoss: " + valueAccumulator.value)
    optimizer.step(model.weightsTensor, gradient, valueAccumulator.value, 0)
  }
}

class SGDPiecewiseLearner(val optimizer: GradientOptimizer, val model: Model) extends PiecewiseLearner {
  val gradient = new ThreadLocal[Tensor] {override def initialValue = model.weightsTensor.copy}
  val gradientAccumulator = new ThreadLocal[LocalTensorAccumulator] { override def initialValue = new LocalTensorAccumulator(gradient.get.asInstanceOf[WeightsTensor]) }
  val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] {override def initialValue = new LocalDoubleAccumulator(0.0)}

  override def process(pieces: GenSeq[Piece]): Unit = {
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit
    pieces.foreach(piece => {
      gradient.get.zero()
      valueAccumulator.get.value = 0.0
      piece.accumulateValueAndGradient(model, gradientAccumulator.get, valueAccumulator.get)
      optimizer.step(model.weightsTensor, gradient.get, valueAccumulator.get.value, 0)
    })
  }
}

class HogwildPiecewiseLearner(optimizer: GradientOptimizer, model: Model) extends SGDPiecewiseLearner(optimizer, model) {
  override def process(pieces: GenSeq[Piece]) = super.process(pieces.par)
}

// Example usage

object LossFunctions {
  type LossFunction = (Double, Double) => (Double, Double)
  val hingeLoss: LossFunction = (prediction, label) => {
    val loss = -math.max(0, 1 - prediction * label)
    (loss, math.signum(loss) * label)
  }
  def sigmoid(x: Double): Double = 1.0 / (1 + math.exp(-x))
  val logLoss: LossFunction = (prediction, label) => {
    val loss = -math.log(1 + math.exp(-prediction * label))
    (loss, math.signum(label) * sigmoid(-prediction * label))
  }
  type MultiClassLossFunction = (Tensor1, Tensor1) => (Double, Tensor1)
  val logMultiClassLoss: MultiClassLossFunction = (prediction, label) => {
    //println("Prediction: " + prediction)
    val normed = prediction.expNormalized
    val loss = math.log(normed dot label)
    val gradient = (label - normed).asInstanceOf[Tensor1]
    (loss, gradient)
  }
}

class MultiClassGLMPiece(featureVector: Tensor1, label: Tensor1, lossAndGradient: LossFunctions.MultiClassLossFunction) extends Piece {
  //def updateState(state: PieceState): Unit = { }
  def state = null
  def accumulateValueAndGradient(model: Model, gradient: TensorAccumulator, value: DoubleAccumulator) {
    // println("featureVector size: %d weights size: %d" format (featureVector.size, model.weights.size))
    val (loss, sgrad) = lossAndGradient(model.weightsTensor.asInstanceOf[Tensor2] matrixVector featureVector, label)
    value.accumulate(loss)
    //println("Stochastic gradient: " + sgrad)
    val numFeatures = featureVector.size
    sgrad.foreachActiveElement((idx, v) => {
      val offset = numFeatures * idx
      featureVector.activeDomain.foreach(x => gradient.accumulate(offset + x, v))
    })
  }
}

// This family exists only to  allow us to map a single tensor into a WeightsTensor
object DummyFamily extends DotFamily {
  type FamilyType = this.type

  type NeighborType1 = Variable
  type FactorType = Factor

  def weights = null
}

 class GLMPiece(featureVector: Tensor, label: Double, lossAndGradient: LossFunctions.LossFunction) extends Piece {
  def state = null
  def accumulateValueAndGradient(model: Model, gradient: TensorAccumulator, value: DoubleAccumulator) {
    val (loss, sgrad) = lossAndGradient(featureVector dot  model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily), label)
    value.accumulate(loss)
    featureVector.activeDomain.foreach(x => gradient.accumulate(DummyFamily, x, sgrad))
  }
}

class BPMaxLikelihoodPiece[A <: cc.factorie.DiscreteValue](labels: Seq[LabeledMutableDiscreteVarWithTarget[A]]) extends Piece{
  def state = null

  labels.foreach(_.setToTarget(null))

  def accumulateValueAndGradient(model: Model, gradient: TensorAccumulator, value: DoubleAccumulator) {
    val fg = BP.inferTreewiseSum(labels.toSet, model)
    // The log loss is - score + log Z
    value.accumulate(- model.score(labels) + fg.bpFactors.head.calculateLogZ)

    fg.bpFactors.foreach(f => {
      val factor = f.factor.asInstanceOf[DotFamily#Factor]
      gradient.accumulate(factor.family, factor.currentStatistics * -1)
      gradient.accumulate(factor.family, f.calculateMarginal)
    })
  }
}

object Test {
  class ModelWithWeightsImpl(model: Model) extends Model {
    def addFactors(v:Variable, result:Set[Factor]): Unit = throw new Error
    def copy = sys.error("unimpl")
    //def setWeights(t: Tensor) { model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights := t }
    val weights = new WeightsTensor()
    weights(DummyFamily) = model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights
    override def weightsTensor = weights
  }

  object DocumentDomain extends CategoricalTensorDomain[String]
  class Document(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = DocumentDomain
    var label = new Label(file.getParentFile.getName, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryFeatureVectorVariable
    "\\w+".r.findAllIn(Source.fromFile(file, "ISO-8859-1").mkString).foreach(regexMatch => this += regexMatch.toString)
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(name: String, val document: Document) extends LabeledCategoricalVariable(name) {
    def domain = LabelDomain
  }
  def main(args: Array[String]): Unit = {
    // Read data and create Variables
    var docLabels = new classify.LabelList[Label, Document](_.document)
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        docLabels += new Document(file).label
      }
    }

    // Make a test/train split
    val (testSet, trainSet) = docLabels.shuffle.split(0.5)
    val trainLabels = new classify.LabelList[Label, Document](trainSet, _.document)
    val testLabels = new classify.LabelList[Label, Document](testSet, _.document)

    val loss = LossFunctions.logMultiClassLoss
    // needs to be binary
    val model = new LogLinearModel[Label, Document](_.document, LabelDomain, DocumentDomain)
    val modelWithWeights = new ModelWithWeightsImpl(model)
    //   val forOuter = new la.SingletonBinaryTensor1(2, 0)
    val pieces = docLabels.map(l => {
      val labelTensor = new la.DenseTensor1(l.domain.size)
      labelTensor += l.target.tensor
      new MultiClassGLMPiece(l.document.value.asInstanceOf[Tensor1], labelTensor, loss)
    })

    val strategy = new SGDPiecewiseLearner(new StepwiseGradientAscent(rate = .01), modelWithWeights)

    var totalTime = 0L
    for (_ <- 1 to 100) {
      val t0 = System.currentTimeMillis()
      strategy.process(pieces)
      totalTime += System.currentTimeMillis() - t0

      val classifier = new ModelBasedClassifier[Label](model, LabelDomain)

      val testTrial = new classify.Trial[Label](classifier)
      testTrial ++= testLabels

      val trainTrial = new classify.Trial[Label](classifier)
      trainTrial ++= trainLabels

      println("Train accuracy = " + trainTrial.accuracy)
      println("Test  accuracy = " + testTrial.accuracy)
      println("Total time to train: " + totalTime / 1000.0)
    }
  }
}