package cc.factorie.optimize
import cc.factorie._
import app.classify
import cc.factorie.util._
import cc.factorie.la._
import classify.{ModelBasedClassifier, LogLinearModel}
import java.io.File
import io.Source

trait LinearObjective[Pred, Label] {
  def valueAndGradient(prediction: Pred, label: Label): (Double, Pred)
}

trait UnivariateLinearObjective[Label] extends LinearObjective[Double, Label]
trait MultivariateLinearObjective[Label] extends LinearObjective[Tensor1, Label]

object LinearObjectives {

  type MultiClass = MultivariateLinearObjective[Int]
  type Binary = UnivariateLinearObjective[Int]

  class HingeMultiClass extends MultivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val lossAugmented = { val c = prediction.copy; c -= (label, 1.0); c }
      val maxLabel = lossAugmented.maxIndex
      if (maxLabel == label)
        (0.0, new SparseIndexedTensor1(prediction.size))
      else {
        val grad = new DenseTensor1(prediction.size, 0.0)
        grad(label) += 1.0
        grad(maxLabel) -= 1.0
        val value = prediction(label) - prediction(maxLabel)
        (value, grad)
      }
    }
  }

  class HingeSqMultiClass extends MultivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val lossAugmented = { val c = prediction.copy; c -= (label, 1.0); c }
      val maxLabel = lossAugmented.maxIndex
      if (maxLabel == label)
        (0.0, new SparseIndexedTensor1(prediction.size))
      else {
        val violation = prediction(label) - prediction(maxLabel)
        val grad = new DenseTensor1(prediction.size, 0.0)
        grad(label) += 2 * -violation
        grad(maxLabel) += 2 * violation
        (-violation * violation, grad)
      }
    }
  }

  class LogMultiClass extends MultivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val normed = prediction.expNormalized.asInstanceOf[Tensor1]
      val loss = math.log(normed(label))
      normed *= -1
      normed(label) += 1.0
      (loss, normed)
    }
  }

  class SparseLogMultiClass extends MultivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val normed = prediction.expNormalized.asInstanceOf[Tensor1]
      val loss = math.log(normed(label))
      normed *= -1
      normed(label) += 1.0
      val sparse = new SparseIndexedTensor1(normed.dim1)
      var i = 0
      while (i < normed.dim1) {
        if (math.abs(normed(i)) > 0.01)
          sparse += (i,normed(i))
        i += 1
      }
      (loss, sparse)
    }
  }

  class SquaredMultivariate extends MultivariateLinearObjective[Tensor1] {
    def valueAndGradient(prediction: Tensor1, label: Tensor1): (Double, Tensor1) = {
      for (i <- prediction.activeDomain)
        prediction(i) -= label(i)
      val value = -(prediction dot prediction)
      prediction *= -2
      (value, prediction)
    }
  }

  class EpsilonInsensitiveSqMultivariate(epsilon: Double) extends MultivariateLinearObjective[Tensor1] {
    def valueAndGradient(prediction: Tensor1, label: Tensor1): (Double, Tensor1) = {
      var objective = 0.0
      for (i <- prediction.activeDomain) {
        prediction(i) -= label(i)
        val o = math.max(0, math.abs(prediction(i)) - epsilon)
        objective -= o*o
        prediction(i) = -2*prediction(i)
      }
      (objective, prediction)
    }
  }

  class LogBinary extends UnivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) = {
      val probCorrect = 1.0 / (1 + math.exp(-label * prediction))
      (math.log(probCorrect), (1 - probCorrect) * label)
    }
  }

  class HingeBinary extends UnivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) =
      if (prediction * label < 1.0)
        (prediction * label - 1.0, label)
      else
        (0.0, 0.0)
  }

  class HingeScaledBinary(posSlackRescale: Double = 1.0, negSlackRescale: Double = 1.0) extends UnivariateLinearObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) = {
      val slackRescale = if (label == 1.0) negSlackRescale else posSlackRescale
      if (prediction * label < 1.0)
        (prediction * label * slackRescale - 1.0, label * slackRescale)
      else
        (0.0, 0.0)
    }
  }

  class SquaredUnivariate extends UnivariateLinearObjective[Double] {
    def valueAndGradient(prediction: Double, label: Double): (Double, Double) =
      (0.5 * (prediction - label) * (prediction - label), prediction - label)
  }

  val squaredMultivariate = new SquaredMultivariate
  val hingeMultiClass = new HingeMultiClass
  val hingeSqMultiClass = new HingeSqMultiClass
  val logMultiClass = new LogMultiClass
  val sparseLogMultiClass = new SparseLogMultiClass
  def epsilonInsensitiveSqMultivariate(epsilon: Double) = new EpsilonInsensitiveSqMultivariate(epsilon)
  def hingeScaledBinary(posSlackRescale: Double = 1.0, negSlackRescale: Double = 1.0) = new HingeScaledBinary(posSlackRescale, negSlackRescale)

  val logBinary = new LogBinary
  val hingeBinary = new HingeBinary
  val squaredUnivariate = new SquaredUnivariate

  type UnivariateLinkFunction = Double => Double

  val squaredLossLinkFunction: UnivariateLinkFunction = prediction => prediction
  val logisticLinkFunction: UnivariateLinkFunction = prediction => 1.0 / (1 + math.exp(-prediction))
}

class LinearMultiClassExample(weights: Weights2, featureVector: Tensor1, label: Int, objective: LinearObjectives.MultiClass, weight: Double = 1.0)
  extends LinearMultivariateExample(weights, featureVector, label, objective, weight)

class LinearBinaryExample(weights: Weights1, featureVector: Tensor1, label: Int, objective: LinearObjectives.Binary, weight: Double = 1.0)
  extends LinearUnivariateExample(weights, featureVector, label, objective, weight)

class LinearMultivariateExample[Label](weights: Weights2, featureVector: Tensor1, label: Label, objective: MultivariateLinearObjective[Label], weight: Double = 1.0)
  extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator) {
    val prediction = weights.value * featureVector
    val (obj, sgrad) = objective.valueAndGradient(prediction, label)
    if (value != null) value.accumulate(obj)
    if (gradient != null && !sgrad.isInstanceOf[UniformTensor]) gradient.accumulate(weights, sgrad outer featureVector, weight)
  }
}

class LinearUnivariateExample[Label](weights: Weights1, featureVector: Tensor1, label: Label, objective: UnivariateLinearObjective[Label], weight: Double = 1.0)
  extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator) {
    val score = weights.value dot featureVector
    val (obj, sgrad) = objective.valueAndGradient(score, label)
    if (value != null) value.accumulate(obj)
    if (gradient != null) gradient.accumulate(weights, featureVector, weight * sgrad)
  }
}

object LinearObjectivesTest {
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

    val loss = LinearObjectives.hingeMultiClass
    // needs to be binary
    val model = new LogLinearModel[Label, Document](_.document, LabelDomain, DocumentDomain)
    //val modelWithWeights = new ModelWithWeightsImpl(model)

    //   val forOuter = new la.SingletonBinaryTensor1(2, 0)
    val pieces = trainLabels.map(l => new LinearMultivariateExample(model.evidenceTemplate.weights, l.document.value.asInstanceOf[Tensor1], l.target.intValue, loss))

    //    val strategy = new HogwildTrainer(new SparseL2RegularizedGradientAscent(rate = .01), modelWithWeights)
//            val strategy = new BatchTrainer(model)
    val strategy = new OnlineTrainer(model.parameters, optimizer = new AdaGrad)

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

      //      println("Parameters = " + model.evidenceTemplate.weightsSet)
      println("Train accuracy = " + trainTrial.accuracy)
      println("Test  accuracy = " + testTrial.accuracy)
      println("Total time to train: " + totalTime / 1000.0)
      i += 1
      perfectAccuracy = (trainTrial.accuracy == 1.0 && testTrial.accuracy == 1.0)
    }

    if (strategy.isConverged || perfectAccuracy) println("Converged in " + totalTime / 1000.0)
  }
}