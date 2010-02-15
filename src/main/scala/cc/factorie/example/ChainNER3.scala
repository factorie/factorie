/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import cc.factorie._
import java.io.File

object ChainNER3 {

  // The variable classes
  class Token(val word:String, features:Seq[String], labelString:String) extends BinaryVectorVariable[String] with VarInSeq[Token] {
    val label: Label = new Label(labelString, this)
    this ++= features
  }
  class Label(labelname: String, val token: Token) extends LabelVariable(labelname) {
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next = token.next.label
    def prev = token.prev.label
  }
  class Sentence extends VariableSeq[Token]
  
  // The model
  val model = new Model(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[Label], 
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[Label, Label] {
      def unroll1(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label) else Nil
      def unroll2(label: Label) = if (label.hasNext) Factor(label, label.token.next.label) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[Label, Token] {
      def unroll1(label: Label) = Factor(label, label.token)
      def unroll2(token: Token) = throw new Error("Token values shouldn't change")
    }
  )
  
  // The training objective
  val objective = new Model(new TrueLabelTemplate[Label])
  


  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER3 trainfile testfile")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))
    val allTokens : Seq[Token] = trainSentences.flatMap(x=>x) ++ testSentences.flatMap(x=>x)
    // Add features from next and previous tokens 
    allTokens.foreach(t => {
      if (t.hasPrev) t ++= t.prev.values.filter(!_.contains('@')).map(_+"@-1")
      if (t.hasNext) t ++= t.next.values.filter(!_.contains('@')).map(_+"@+1")
    })
    
    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.map(_.label))
    val testLabels = testSentences.flatMap(_.map(_.label))
    
    // Sample and Learn!
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val learner = new GibbsSampler(model, objective) with SampleRank with MIRAUpdates with ParameterAveraging { temperature = 0.01 }
    val predictor = new GibbsSampler(model) { temperature = 0.01 }
    for (i <- 1 to 6) {
      println("Iteration "+i) 
      learner.process(trainLabels, 1)
      trainLabels.take(50).foreach(printLabel _); println; println
      printDiagnostic(trainLabels.take(400))
      println ("Train accuracy = "+ objective.aveScore(trainLabels))
      println ("Test  accuracy = "+ objective.aveScore(testLabels))
    }
    learner.setWeightsToAverage
    predictor.temperature *= 0.1
    predictor.process(testLabels, 2)
    println ("Final Test  accuracy = "+ objective.aveScore(testLabels))
  }

  // Feature extraction
  def wordToFeatures(word:String, initialFeatures:String*) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    f += "W="+word
    f ++= initialFeatures
    if (word.length > 3) f += "PRE="+word.substring(0,3)
    if (Capitalized.findFirstMatchIn(word) != None) f += "CAPITALIZED"
    if (Numeric.findFirstMatchIn(word) != None) f += "NUMERIC"
    if (Punctuation.findFirstMatchIn(word) != None) f += "PUNCTUATION"
    f
  }
  val Capitalized = "^[A-Z].*".r
  val Numeric = "^[0-9]+$".r
  val Punctuation = "[-,\\.;:?!()]+".r

  
  def printLabel(label:Label) : Unit = {
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.trueValue, label.value, label.token.toString))
  }
 
  def printDiagnostic(labels:Seq[Label]) : Unit = {
    for (label <- labels; if (label.index != label.domain.index("O"))) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format((if (label.value != label.trueValue) label.trueValue else " "), label.value))
      print(label.token.word+" ")
      if (!label.hasNext || label.value != label.next.value) println()
    }
  }
 
  def load(filename:String) : Seq[Sentence] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    var wordCount = 0
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(new File(filename))
    var sentence = new Sentence
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val pos = fields(1)
        val label = fields(3).stripLineEnd
        sentence += new Token(word, wordToFeatures(word,pos), label)
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


