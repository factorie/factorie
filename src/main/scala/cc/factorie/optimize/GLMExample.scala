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
  type RegressionObjectiveFunction = (Double, Double) => (Double, Double)
  val squaredLossRegressionObjective: RegressionObjectiveFunction =  (prediction, label) => {
    (0.5*(prediction - label)*(prediction - label),(prediction - label))
  }
  type RegressionLinkFunction = (Double) => Double
  val squaredLossLinkFunction: RegressionLinkFunction = prediction => prediction

  type BinaryObjectiveFunction = (Double, Double) => (Double, Double)
  val logBinaryObjective: BinaryObjectiveFunction = (prediction, label) => {
      val probCorrect = 1.0/(1 + math.exp(-label*prediction))
      (math.log(probCorrect),(1- probCorrect)*label)
  }
  val hingeBinaryObjective: BinaryObjectiveFunction = (prediction, label) => {
    if (prediction * label < 1.0)
      (prediction * label - 1.0, label)
    else
      (0.0, 0.0)
  }

  type BinaryLinkFunction = (Double) => (Double)
  val logisticLinkFunction:  BinaryLinkFunction = prediction => 1.0/(1 + math.exp(-prediction))

  type MultiClassObjectiveFunction = (Tensor1, Int) => (Double, Tensor1)

  val logMultiClassObjective: MultiClassObjectiveFunction = (prediction, label) => {
    val normed = prediction.expNormalized.asInstanceOf[Tensor1]
    val loss = math.log(normed(label))
    normed *= -1
    normed(label) += 1.0
    (loss, normed)
  }

    // This implements Structured SVM loss for the multiclass problem (create a margin between the 2 best-ranked labels)
  val hingeMultiClassObjective: MultiClassObjectiveFunction = (prediction, label) => {
    val lossAugmented = { val c = prediction.copy; c -= (label, 1.0); c }
    val maxLabel = lossAugmented.maxIndex
    if (maxLabel == label)
      (0.0, new UniformTensor1(prediction.size, 0.0))
    else {
      val grad = new DenseTensor1(prediction.size, 0.0)
      grad(label) += 1.0
      grad(maxLabel) -= 1.0
      val value = prediction(label) - prediction(maxLabel)
      (value, grad)
    }
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

class LinearMultiClassExample(weights: TensorSetKey2, featureVector: Tensor1, label: Int, lossAndGradient: ObjectiveFunctions.MultiClassObjectiveFunction, weight: Double = 1.0)
  extends Example[WeightsDef] {
  def accumulateExampleInto(model: WeightsDef, gradient:TensorSetAccumulator, value:DoubleAccumulator) {
    val prediction = weights.value * featureVector
    val (obj, sgrad) = lossAndGradient(prediction, label)
    if (value != null) value.accumulate(obj)
    if (gradient != null) gradient.accumulate(weights, sgrad outer featureVector, weight)
  }
}

class LinearBinaryExample(weights: TensorSetKey1, featureVector: Tensor1, label: Int, lossAndGradient: ObjectiveFunctions.BinaryObjectiveFunction, weight: Double = 1.0)
  extends Example[WeightsDef] {
  def accumulateExampleInto(model: WeightsDef, gradient:TensorSetAccumulator, value:DoubleAccumulator) {
    val score = weights.value dot featureVector
    val (obj, sgrad) = lossAndGradient(score, label)
    if (value != null) value.accumulate(obj)
    if (gradient != null) gradient.accumulate(weights, featureVector, weight * sgrad)
  }
}

//
//class GLMExample(featureVector: Tensor1, targetLabel: Int, lossAndGradient: ObjectiveFunctions.MultiClassObjectiveFunction, var weight: Double = 1.0) extends Example[LogLinearModel[_,_]] {
//  //def updateState(state: ExampleState): Unit = { }
//  def state = null
//  def accumulateExampleInto(model: LogLinearModel[_,_], gradient:TensorSetAccumulator, value:DoubleAccumulator) {
//    // println("featureVector size: %d weightsSet size: %d" format (featureVector.size, model.weightsSet.size))
//    val weightsMatrix = model.evidenceTemplate.weights.value
//    val prediction = weightsMatrix * featureVector
//    //    println("Prediction: " + prediction)
//    val (loss, sgrad) = lossAndGradient(prediction, targetLabel)
//    if (value != null) value.accumulate(loss)
//    if (weight != 1.0) sgrad *= weight
//    //    println("Stochastic gradient: " + sgrad)
//    // TODO: find out why using the Outer1Tensor2 here is so much slower than accumulateOuter? Inlining??
//    //    if (gradient != null) gradient.accumulate(model.evidenceTemplate, new la.Outer1Tensor2(sgrad, featureVector))
//    if (gradient != null) gradient.accumulate(model.evidenceTemplate.weights, sgrad outer featureVector)
//  }
//}

// This family exists only to  allow us to map a single tensor into a WeightsTensor
//object DummyFamily extends DotFamily {
//  type FamilyType = this.type
//  type NeighborType1 = Variable
//  type FactorType = Factor
//  def weightsSet = null
//}
//class ModelWithWeightsImpl(model: Model[Variable]) extends Model[Variable] {
//  def factors(v: Variable): Iterable[Factor] = throw new Error
//  def copy = sys.error("unimpl")
//  //def setWeights(t: Tensor) { model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weightsSet := t }
//  val weightsSet = new WeightsTensor()
//  weightsSet(DummyFamily) = model.asInstanceOf[LogLinearModel[_, _]].evidenceTemplate.weightsSet
//  override def weights = weightsSet
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
    val pieces = trainLabels.map(l => new LinearMultiClassExample(model.evidenceTemplate.weights, l.document.value.asInstanceOf[Tensor1], l.target.intValue, loss))

    //    val strategy = new HogwildTrainer(new SparseL2RegularizedGradientAscent(rate = .01), modelWithWeights)
//            val strategy = new BatchTrainer(model)
    val strategy = new OnlineTrainer(model, optimizer = new AdaGrad)

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

      val classifier = new ModelBasedClassifier[Label, model.evidenceTemplate.type](model.evidenceTemplate, LabelDomain)

      val testTrial = new classify.Trial[Label](classifier)
      testTrial ++= testLabels

      val trainTrial = new classify.Trial[Label](classifier)
      trainTrial ++= trainLabels

      //      println("WeightsDef = " + model.evidenceTemplate.weightsSet)
      println("Train accuracy = " + trainTrial.accuracy)
      println("Test  accuracy = " + testTrial.accuracy)
      println("Total time to train: " + totalTime / 1000.0)
      i += 1
      perfectAccuracy = (trainTrial.accuracy == 1.0 && testTrial.accuracy == 1.0)
    }

    if (strategy.isConverged || perfectAccuracy) println("Converged in " + totalTime / 1000.0)
  }
}