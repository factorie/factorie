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


object ObjectiveFunctions {
  type MultiClassObjectiveFunction = (Tensor1, Int) => (Double, Tensor1)
  val logMultiClassObjective: MultiClassObjectiveFunction = (prediction, label) => {
    val normed = prediction.expNormalized
    val loss = math.log(normed(label))
    normed *= -1
    normed(label) += 1.0
    val gradient = normed.asInstanceOf[Tensor1]
    (loss, gradient)
  }
  // This implements Structured SVM loss for the multiclass problem (create a margin between the 2 best-ranked labels)
  val hingeMultiClassObjective: MultiClassObjectiveFunction = (prediction, label) => {
    var loss = 0.0; var i = 0; val len = prediction.length
    while (i < len) {
      if (i == label) loss += -math.max(0, 1 - prediction(label))
      else loss += -math.max(0, prediction(i) - 1)
      i += 1
    }
    val (maxLabel1, maxLabel2) = prediction.maxIndex2
    val gradient =
      if (label == maxLabel1 && prediction(maxLabel1) > 1 + prediction(maxLabel2))
        new UniformTensor1(prediction.size, 0.0)
      else if (label == maxLabel1) {
        val g = new DenseTensor1(prediction.size, 0.0)
        g(label) += 1.0
        g(maxLabel2) += -1.0
        g
      } else {
        val g = new DenseTensor1(prediction.size, 0.0)
        g(label) += 1.0
        g(maxLabel1) += -1.0
        g
      }
    (loss, gradient)
  }
  val hingeSqMultiClassObjective: MultiClassObjectiveFunction = (prediction, label) => {
    var loss = 0.0; var i = 0; val len = prediction.length
    while (i < len) {
      if (i == label) loss += -math.pow(math.max(0, 1 - prediction(label)), 2)
      else loss += -math.pow(math.max(0, prediction(i) - 1), 2)
      i += 1
    }
    val (maxLabel1, maxLabel2) = prediction.maxIndex2
    val gradient =
      if (label == maxLabel1 && prediction(maxLabel1) > 1 + prediction(maxLabel2))
        new UniformTensor1(prediction.size, 0.0)
      else if (label == maxLabel1) {
        val g = new DenseTensor1(prediction.size, 0.0)
        g(label) += 2 * (prediction(maxLabel2) + 1 - prediction(label))
        g(maxLabel2) += -2 * (prediction(maxLabel2) + 1 - prediction(label))
        g
      } else {
        val g = new DenseTensor1(prediction.size, 0.0)
        g(label) += 2 * (prediction(maxLabel1) + 1 - prediction(label))
        g(maxLabel1) += -2 * (prediction(maxLabel1) + 1 - prediction(label))
        g
      }
//    println(prediction)
//    println(gradient)
    (loss, gradient)
  }
  // This one has a hot index in the gradient for every margin violation, not just the biggest one
  // However this messes with projected gradient stuff by making the norm too big
  val hingeMultiClassObjectiveOneVsAll: MultiClassObjectiveFunction = (prediction, label) => {
    var loss = 0.0; var i = 0; val len = prediction.length
    while (i < len) {
      if (i == label) loss += -math.max(0, 1 - prediction(label))
      else loss += -math.max(0, prediction(i) - 1)
      i += 1
    }
    val gradient = new DenseTensor1(prediction.size, 0.0)
    i = 0; var isPerfectlyClassified = true
    while (i < len) {
      if (i == label && prediction(i) < 1.0) { gradient(i) = 1.0; isPerfectlyClassified = false }
      else if (i != label && prediction(i) > 1.0) { gradient(i) = -1.0; isPerfectlyClassified = false }
//      else if (i == predictedLabel && predictedLabel != label && prediction(i) > 1.0) gradient(i) = -1.0
      i += 1
    }
    (loss, if (isPerfectlyClassified) new UniformTensor1(len, 0.0) else gradient)
  }
}

class GLMExample(featureVector: Tensor1, label: Int, lossAndGradient: ObjectiveFunctions.MultiClassObjectiveFunction, var weight: Double = 1.0) extends Example[LogLinearModel[_,_]] {
  //def updateState(state: ExampleState): Unit = { }
  def state = null
  def accumulateExampleInto(model: LogLinearModel[_,_], gradient:WeightsTensorAccumulator, value:DoubleAccumulator, margin:DoubleAccumulator) {
    // println("featureVector size: %d weights size: %d" format (featureVector.size, model.weights.size))
    val weightsMatrix = model.evidenceTemplate.weights
    val prediction = weightsMatrix * featureVector
    //    println("Prediction: " + prediction)
    val (loss, sgrad) = lossAndGradient(prediction, label)
    if (value != null) value.accumulate(loss)
    if (weight != 1.0) sgrad *= weight
    //    println("Stochastic gradient: " + sgrad)
    // TODO: find out why using the Outer1Tensor2 here is so much slower than accumulateOuter? Inlining??
    //    if (gradient != null) gradient.accumulate(model.evidenceTemplate, new la.Outer1Tensor2(sgrad, featureVector))
    if (gradient != null) gradient.accumulateOuter(model.evidenceTemplate, sgrad, featureVector)
  }
}

// This family exists only to  allow us to map a single tensor into a WeightsTensor
//object DummyFamily extends DotFamily {
//  type FamilyType = this.type
//  type NeighborType1 = Variable
//  type FactorType = Factor
//  def weights = null
//}
//class ModelWithWeightsImpl(model: Model[Variable]) extends Model[Variable] {
//  def factors(v: Variable): Iterable[Factor] = throw new Error
//  def copy = sys.error("unimpl")
//  //def setWeights(t: Tensor) { model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights := t }
//  val weights = new WeightsTensor()
//  weights(DummyFamily) = model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weights
//  override def weightsTensor = weights
//}

object GlmTest {
  object DocumentDomain extends CategoricalDimensionTensorDomain[String]
  class Document(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = DocumentDomain
    var label = new Label(file.getParentFile.getName, this)
    val text = Source.fromFile(file, "ISO-8859-1").mkString
    val text2 = Some(text.indexOf("\n\n"))
        .filter(-1 !=).orElse(Some(text.indexOf("\r\n\r\n")))
        .filter(-1 !=).map(text.substring(_)).getOrElse(text)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryFeatureVectorVariable
    "\\w+".r.findAllIn(text2).foreach(regexMatch => this += regexMatch.toString)
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

    val loss = ObjectiveFunctions.hingeMultiClassObjective
    // needs to be binary
    val model = new LogLinearModel[Label, Document](_.document, LabelDomain, DocumentDomain)
    //val modelWithWeights = new ModelWithWeightsImpl(model)

    //   val forOuter = new la.SingletonBinaryTensor1(2, 0)
    val pieces = trainLabels.map(l => new GLMExample(l.document.value.asInstanceOf[Tensor1], l.target.intValue, loss))

    //    val strategy = new HogwildTrainer(new SparseL2RegularizedGradientAscent(rate = .01), modelWithWeights)
//            val strategy = new BatchTrainer(model)
    val strategy = new InlineSGDTrainer(model, optimizer = new AdagradAccumulatorMaximizer(model))

//        val strategy = new SGDThenBatchTrainer(new L2RegularizedLBFGS, modelWithWeights)
//    val lbfgs = new L2RegularizedLBFGS(l2 = 0.1)
//    lbfgs.tolerance = 0.05
//    val strategy = new SGDThenBatchTrainer(lbfgs, model, learningRate = .01)
    //    val strategy = new BatchTrainer(new SparseL2RegularizedGradientAscent(rate = 10.0 / trainLabels.size), modelWithWeights)

    var totalTime = 0L
    var i = 0
    var perfectAccuracy = false
    while (i < 100 && !strategy.isConverged && !perfectAccuracy) {
      val t0 = System.currentTimeMillis()
      strategy.processExamples(pieces)
      println(model.evidenceTemplate.weights)
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