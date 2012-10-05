package cc.factorie.optimize
import cc.factorie._
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

abstract class TensorAccumulator {
  def add(t: Tensor) : Unit
  def add(index: Int, value: Double): Unit
}

class ActualTensorAccumulator(val innerTensor: Tensor) extends TensorAccumulator {
  val l = new Object
  def add(t: Tensor) {
    innerTensor += t
  }

  def add(index: Int, value: Double) {
    innerTensor(index) += value
  }
}


abstract class PieceState {
  def merge(other: PieceState): PieceState
}

abstract class ModelWithWeights {
  def weights: Tensor
  def setWeights(t: Tensor): Unit
  def copy: ModelWithWeights
}

// Pieces are thread safe
abstract class Piece {
  def process(model: ModelWithWeights, gradient: TensorAccumulator,  value: TensorAccumulator,  computeGradient: Boolean, computeValue: Boolean): Unit

  def state: PieceState

  def updateState(state: PieceState)
}

abstract class OptimizationStrategy(val pieces: GenSeq[Piece], val model: ModelWithWeights) {
  def iterate(): Unit
}

class DumbOptimizerStrategy(pieces: GenSeq[Piece], optimizer: GradientOptimizer, model: ModelWithWeights) extends OptimizationStrategy(pieces, model) {
  val gradient = model.weights.copy
  val gradientAccumulator = new ActualTensorAccumulator(gradient)

  val value = new DenseTensor1(1)
  val valueAccumulator = new ActualTensorAccumulator(value)
  def iterate() {
    gradient.zero()
    value.zero()
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit
    pieces/*par*/.foreach(piece => piece.process(model, gradientAccumulator, valueAccumulator, true, true))
    optimizer.step(model.weights, gradient, value(0), 0)
  }
}

class SGDStrategy(pieces: GenSeq[Piece], optimizer: GradientOptimizer, model: ModelWithWeights) extends DumbOptimizerStrategy(pieces, optimizer, model) {
  override def iterate() {
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit
    pieces.foreach(piece => {
      gradient.zero()
      value.zero()
      piece.process(model, gradientAccumulator, valueAccumulator, true, true)
      optimizer.step(model.weights, gradient, value(0), 0)
    })
  }
}

class HogwildStrategy(pieces: GenSeq[Piece], optimizer: GradientOptimizer, model: ModelWithWeights) extends SGDStrategy(pieces.par, optimizer, model) {}

object LossFunctions {
  type LossFunction = (Double, Double) => (Double, Double)
  val hingeLoss: LossFunction = (prediction, label) => {
    val loss = math.max(0, 1 - prediction * label)
    (loss, math.signum(loss) * label)
  }
  def sigmoid(x: Double): Double = 1.0 / (1 + math.exp(-x))
  val logLoss: LossFunction = (prediction, label) => {
    val loss = math.log(1 + math.exp(-prediction * label))
    (loss, math.signum(label)*sigmoid(-prediction * label))
  }
}

class GLMPiece(featureVector: Tensor, label: Double, lossAndGradient: LossFunctions.LossFunction) extends Piece {
  def updateState(state: PieceState): Unit = { }
  def state = null
  def process(model: ModelWithWeights, gradient: TensorAccumulator, value: TensorAccumulator, computeGradient: Boolean, computeValue: Boolean) {
    // println("featureVector size: %d weights size: %d" format (featureVector.size, model.weights.size))
    val (loss, sgrad) = lossAndGradient(featureVector dot  model.weights , label)
    if (computeValue) value.add(0, -loss)
    if (computeGradient) featureVector.activeDomain.foreach(x => gradient.add(x, sgrad))
  }
}

object Test {
  class ModelWithWeightsImpl(model: Model) extends ModelWithWeights {
    def copy = sys.error("unimpl")
    def setWeights(t: Tensor) { model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights := t }
    def weights = model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights
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

    val loss = LossFunctions.logLoss
    // needs to be binary
    val model = new LogLinearModel[Label, Document](_.document, LabelDomain, DocumentDomain)
    val modelWithWeights = new ModelWithWeightsImpl(model)
    val forOuter = new la.SingletonBinaryTensor1(2, 0)
    val pieces = docLabels.map(l => new GLMPiece((l.document.value outer forOuter), (1 - l.target.value.intValue) * 2 - 1, loss))

    val strategy = new HogwildStrategy(pieces, new StepwiseGradientAscent(rate=0.01), modelWithWeights)

    var totalTime = 0L
    for (_ <- 1 to 100) {
      val t0 = System.currentTimeMillis()
      strategy.iterate()
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