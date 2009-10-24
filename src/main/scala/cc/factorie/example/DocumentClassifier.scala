package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie.util.Implicits._

class DocumentClassifierModel extends Model {

  class Label(name:String, val document:Document) extends cc.factorie.Label(name) 
  class Document(labelString:String, features:Iterable[String]) extends BinaryVectorVariable[String] {
    var label : Label = new Label(labelString, this)
    this ++= features
  }

  /** Bias term just on labels */
  this += new TemplateWithExpStatistics1[Label] with DenseWeights

  /** Factor between label and observed document */
  this += new TemplateWithExpStatistics2[Label,Document] with DenseWeights {
    def unroll1 (label:Label) = Factor(label, label.document)
    def unroll2 (token:Document) = throw new Error("Document values shouldn't change")
  }

  val objective = new Model(new TrueLabelTemplate[Label])
}


object DocumentClassifierDemo {
  
  def main(args: Array[String]) : Unit = {
  	if (args.length < 2) 
  		throw new Error("You must specify at least two directories containing text files for classification")
    val model = new DocumentClassifierModel()

    val docClassifyWorld = new World {
      import model._
      var documents = new ArrayBuffer[Document]
      // Read data and create Variables
      val lexer = new Regex("[a-zA-Z]+")
      for (directory <- args) {
        val directoryFile = new File(directory)
        if (! directoryFile.exists) throw new IllegalArgumentException("Directory "+directory+" does not exist.")
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          Console.println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
          Console.flush
          documents += new Document(directory, lexer.findAllIn(file.contentsAsString).toList.map(_ toLowerCase))
        }
      }

      // Make a test/train split
      val (testSet, trainSet) = documents.shuffle.split(0.5)
      var trainVariables = trainSet.map(_ label)
      var testVariables = testSet.map(_ label)

      // Set label variables to random values
      documents.foreach(d => d.label.set(d.label.domain.randomValue)(null))
      Console.println ("Initial test accuracy = "+ model.objective.aveScore(testVariables))

      // Sample and Learn!
      val learner = new GibbsSampleRank[Label](model, model.objective) with PerceptronUpdates
      val sampler = new GibbsSampler(model)
      learner.learningRate = 1.0
      for (i <- 0 until 10) {
        learner.process (trainVariables, 1)
        learner.learningRate *= 0.8
        sampler.process (testVariables, 4)
        Console.println ("Test accuracy = "+ model.objective.aveScore(testVariables))
      }

      // Show the parameters
      //Console.println ("Printing parameters of factors "+modelTemplates.size)
      //modelTemplates.foreach (f => Console.println(f.weights.toList))
    }
    0;
  }
}



