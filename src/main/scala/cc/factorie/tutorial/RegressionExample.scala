package cc.factorie.tutorial

import java.io.File

import collection.mutable
import io.Source

import cc.factorie.app.regress.LinearRegressionTrainer
import cc.factorie.la.{DenseTensor1, Tensor1}
import cc.factorie.variable.{TensorVariable, BinaryFeatureVectorVariable, DiffList, CategoricalVectorDomain}

/**
 * An example of Linear Regression.  Tries to predict the hash value
 */
object RegressionExample {

  // input features
  object InputDomain extends CategoricalVectorDomain[String]
  class Input(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = InputDomain

    { // add all words in document to vector
      val text = Source.fromFile(file, "ISO-8859-1").mkString
      val words = """\w+""".r.findAllIn(text.trim)
      words.foreach{ word => this += word }
    }
  }

  class Output(val input: Input, val label: Double)(implicit d: DiffList = null) extends TensorVariable[Tensor1] {
    set(new DenseTensor1(1))
    value(0) = label
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Usage: scala cc.factorie.tutorial.RegressionExample folder1/ folder2/")

    /** Load documents **/
    var outputs = mutable.ArrayBuffer[Output]()
    for ((directory, i) <- args.zipWithIndex) {
      for (file <- new File(directory).listFiles; if file.isFile) {
//        println("Loading " + file.getName)
        outputs += new Output(new Input(file), (2 * i - 1) + math.random * 0.001)
      }
    }
    // println("Loaded " + outputs.length + " files")

    /** Run regression **/
    val regressor = LinearRegressionTrainer.train[Input, Output](outputs, {f => f.input}, 0.0)
    val predictions: Seq[Double] = regressor.regressions(outputs).map(_.dependantValue(0))
    val truth: Seq[Double] = outputs.map(_.value(0))
    val error = truth.zip(predictions).map{case (t, p) => (t - p) * (t-p) }.sum
    // println("Prediction error: " + error)
    // println("Predictions/Truth:" + predictions.zip(truth))
  }

}
