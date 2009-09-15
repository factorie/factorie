// A Named Entity Recognizer (NER) with a linear-chain CRF model

package cc.factorie.example

import scala.collection.mutable.ArrayBuffer
import cc.factorie._
import cc.factorie.model.LinearChainModel

class ChainNerModel extends LinearChainModel {
	this += new LabelTemplate with PerceptronLearning
  this += new TransitionTokenTemplate with PerceptronLearning
  val objective = new Model(new TrueLabelTemplate[Label])
}

class ChainNerWorld[M<:ChainNerModel](val model:M) extends World {
  import model._
  import scala.io.Source
  def load(filename:String) : Seq[Sentence] = {
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(filename)
    var sentence = new Sentence
    for (line <- source.getLines) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence //; println("num words " + document.size + " num docs "+documents.size)
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val pos = fields(1)
        val label = fields(3).stripLineEnd
        sentence += new Token(word, List("W="+word, "POS="+pos), label)
      }
    }
    println("Loaded "+sentences.length+" sentences from file "+filename)
    sentences
  }
}

object ChainNerDemo {
  def main(args: Array[String]) : Unit = {
    val model = new ChainNerModel()
    val nerWorld = new ChainNerWorld(model) {
      import model._
      // Read in the data
      var dataDir = "/Users/mccallum/research/data/ie/ner2003/"
      var trainFilename = dataDir+"eng.train"
      var testFilename = dataDir+"eng.testa" 
      if (args.length == 2) {
        trainFilename = args(0)
        testFilename = args(1)
      }
      val trainTokens = load(trainFilename)
      val testTokens = load(testFilename)
      val allTokens : Seq[Token] = trainTokens.flatMap(x=>x) ++ testTokens.flatMap(x=>x)
      // Add more features
      val Capitalized = "^[A-Z].*".r
      val Numeric = "^[0-9]+$".r
      val Punctuation = "[,\\.;:?!()]+".r
      def addFeatures(t:Token) : Unit = {
        if (t.word.length > 3) t += t.word.substring(0,3)
        if (Capitalized.findFirstMatchIn(t.word) != None) t += "CAPITALIZED"
        if (Numeric.findFirstMatchIn(t.word) != None) t += "NUMERIC"
        if (Punctuation.findFirstMatchIn(t.word) != None) t += "PUNCTUATION"
      }
      allTokens.foreach(addFeatures(_))
      allTokens.foreach(t => {
        if (t.hasPrev) t ++= t.prev.values.filter(!_.contains('@')).map(_+"@-1")
        if (t.hasNext) t ++= t.next.values.filter(!_.contains('@')).map(_+"@+1")
      })
      // Get the variables to be inferred
      val trainLabels : Seq[Label] = trainTokens.flatMap(_.map(_.label))
      val testLabels : Seq[Label] = testTokens.flatMap(_.map(_.label))
      // Sample and Learn!
      (trainLabels ++ testLabels).foreach(_.setRandomly(Global.random, null))
      val sampler = new GibbsPerceptronLearner(model, model.objective)
      for (i <- 0 until 10) {
        sampler.sampleAndLearn (trainLabels, 1)
        Console.println ("Train accuracy = "+ model.aveScore(trainLabels))
        sampler.learningRate *= 0.9
        sampler.sample (testLabels, 2)
        Console.println ("Test  accuracy = "+ model.aveScore(testLabels))
      }

    }
    0
  }	
  
}
