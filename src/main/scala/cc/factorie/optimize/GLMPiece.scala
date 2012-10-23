package cc.factorie.optimize
import cc.factorie._
import app.classify
import cc.factorie.util._
import cc.factorie.la._
import classify.{ModelBasedClassifier, LogLinearModel}
import java.io.File
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/17/12
 * Time: 7:32 PM
 * To change this template use File | Settings | File Templates.
 */


object LossFunctions {
  type MultiClassLossFunction = (Tensor1, Int) => (Double, Tensor1)
  val logMultiClassLoss: MultiClassLossFunction = (prediction, label) => {
    val normed = prediction.expNormalized
    val loss = math.log(normed(label))
    normed *= -1
    normed(label) += 1.0
    val gradient = normed.asInstanceOf[Tensor1]
    (loss, gradient)
  }
  val hingeMultiClassLoss: MultiClassLossFunction = (prediction, label) => {
    // TODO: this seems wrong - shouldnt it have loss for every weight vector it has a margin violation with?
    // same with the gradient, should it be 0 for categories without margin violations, 1 for those with, -1 for correct category?
    val loss = -math.max(0, 1 - prediction(label))
    val predictedLabel = prediction.maxIndex
    val gradient =
      if (label == predictedLabel)
        new UniformTensor1(prediction.size, 0.0)
      else {
        val g = new DenseTensor1(prediction.size, 0.0)
        g(label) += 1.0
        g(predictedLabel) += -1.0
        g
      }
    (loss, gradient)
  }
}

class GLMPiece(featureVector: Tensor1, label: Int, lossAndGradient: LossFunctions.MultiClassLossFunction, var weight: Double = 1.0) extends Piece[Variable] {
  //def updateState(state: PieceState): Unit = { }
  def state = null
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    // println("featureVector size: %d weights size: %d" format (featureVector.size, model.weights.size))
    val weightsMatrix = model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily).asInstanceOf[Tensor2]
    val prediction = weightsMatrix matrixVector featureVector
    //    println("Prediction: " + prediction)
    val (loss, sgrad) = lossAndGradient(prediction, label)
    if (value != null) value.accumulate(loss)
    if (weight != 0.0) sgrad *= weight
    //    println("Stochastic gradient: " + sgrad)
    if (gradient != null) gradient.accumulateOuter(DummyFamily, sgrad, featureVector)
  }
}

// This family exists only to  allow us to map a single tensor into a WeightsTensor
object DummyFamily extends DotFamily {
  type FamilyType = this.type

  type NeighborType1 = Variable
  type FactorType = Factor

  def weights = null
}

class ModelWithWeightsImpl(model: Model[Variable]) extends Model[Variable] {
  def factors(v: Variable): Iterable[Factor] = throw new Error
  def copy = sys.error("unimpl")
  //def setWeights(t: Tensor) { model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights := t }
  val weights = new WeightsTensor()
  weights(DummyFamily) = model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights
  override def weightsTensor = weights
}

object GlmTest {
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
    val pieces = trainLabels.map(l => new GLMPiece(l.document.value.asInstanceOf[Tensor1], l.target.intValue, loss))

    //    val strategy = new HogwildTrainer(new SparseL2RegularizedGradientAscent(rate = .01), modelWithWeights)
    //        val strategy = new BatchTrainer(new L2RegularizedConjugateGradient, modelWithWeights)
    //        val strategy = new SGDTrainer(new ConfidenceWeighting(modelWithWeights), modelWithWeights)
    //    val strategy = new SGDThenBatchTrainer(new L2RegularizedLBFGS, modelWithWeights)
    val lbfgs = new L2RegularizedLBFGS(l2 = 0.1)
    lbfgs.tolerance = 0.05
    val strategy = new SGDThenBatchTrainer(lbfgs, modelWithWeights, learningRate = .01)
    //    val strategy = new BatchTrainer(new SparseL2RegularizedGradientAscent(rate = 10.0 / trainLabels.size), modelWithWeights)

    var totalTime = 0L
    var i = 0
    var perfectAccuracy = false
    while (i < 100 && !strategy.isConverged && !perfectAccuracy) {
      val t0 = System.currentTimeMillis()
      strategy.processAll(pieces)

      //      val classifier = new classify.MaxEntTrainer().train(trainLabels)

      totalTime += System.currentTimeMillis() - t0

      val classifier = new ModelBasedClassifier[Label](model.evidenceTemplate, LabelDomain)

      val testTrial = new classify.Trial[Label](classifier)
      testTrial ++= testLabels

      val trainTrial = new classify.Trial[Label](classifier)
      trainTrial ++= trainLabels

      //      println("Weights = " + model.evidenceTemplate.weights)
      println("Train accuracy = " + trainTrial.accuracy)
      println("Test  accuracy = " + testTrial.accuracy)
      println("Total time to train: " + totalTime / 1000.0)
      i += 1
      perfectAccuracy = (trainTrial.accuracy == 1.0 && testTrial.accuracy == 1.0)
    }

    if (strategy.isConverged || perfectAccuracy) println("Converged in " + totalTime / 1000.0)
  }
}