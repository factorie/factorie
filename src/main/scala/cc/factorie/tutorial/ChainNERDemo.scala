/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.tutorial
import cc.factorie._
import java.io.File
import cc.factorie.variable._
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, DotTemplateWithStatistics1, TemplateModel}
import cc.factorie.infer.{BPSummary, BP, IteratedConditionalModes, GibbsSampler}

/** A demonstration of training a linear-chain CRF for named entity recognition.
    Prints various diagnostics suitable to a demo.
    @author Andrew McCallum  */
object ChainNERDemo {

  // The variable classes
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(val word:String, features:Seq[String], labelString:String) extends BinaryFeatureVectorVariable[String] with ChainLink[Token,Sentence] {
    def domain = TokenDomain
    val label: Label = new Label(labelString, this)
    this ++= features
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelname: String, val token: Token) extends LabeledCategoricalVariable(labelname) {
    def domain = LabelDomain
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next = token.next.label
    def prev = token.prev.label
  }
  class Sentence extends Chain[Sentence,Token]
  
  // The model
  val model = new TemplateModel with Parameters {
    // Bias term on each individual label
    object bias extends DotTemplateWithStatistics1[Label] {
      val weights = Weights(new la.DenseTensor1(LabelDomain.size))
    }
    // Transition factors between two successive labels
    object transtion extends DotTemplateWithStatistics2[Label, Label] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
      def unroll1(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      def unroll2(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
    }
    // Factor between label and observed token
    object evidence extends DotTemplateWithStatistics2[Label, Token] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.dimensionSize))
      def unroll1(label: Label) = Factor(label, label.token)
      def unroll2(token: Token) = throw new Error("Token values shouldn't change")
    }
    this += evidence
    this += bias
    this += transtion
  }
  
  // The training objective
  val objective = new HammingTemplate[Label]

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length != 2) throw new Error("Usage: ChainNERDemo trainfile testfile")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.links.map(_.label)).take(50000) //.take(30000)
    val testLabels = testSentences.flatMap(_.links.map(_.label))//.take(2000)
    val allTokens: Seq[Token] = (trainLabels ++ testLabels).map(_.token)

    // Add features from next and previous tokens 
    // println("Adding offset features...")
    allTokens.foreach(t => {
      if (t.hasPrev) t ++= t.prev.activeCategories.filter(!_.contains('@')).map(_+"@-1")
      if (t.hasNext) t ++= t.next.activeCategories.filter(!_.contains('@')).map(_+"@+1")
    })

    println("Using "+TokenDomain.dimensionSize+" observable features.")
    
    // Print some significant features
    //println("Most predictive features:")
    //val pllo = new cc.factorie.app.classify.PerLabelLogOdds(trainSentences.flatMap(_.map(_.label)), (label:Label) => label.token)
    //for (label <- LabelDomain.values) println(label.category+": "+pllo.top(label, 20))
    
    // Sample and Learn!
    val startTime = System.currentTimeMillis
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val learner = new optimize.SampleRankTrainer(new GibbsSampler(model, objective) {temperature=0.1}, new cc.factorie.optimize.AdaGrad)
    val predictor = new IteratedConditionalModes(model, null)
    for (i <- 1 to 3) {
      // println("Iteration "+i)
      learner.processContexts(trainLabels)
      predictor.processAll(testLabels); predictor.processAll(trainLabels)
      trainLabels.take(20).foreach(printLabel _); println(); println()
      printDiagnostic(trainLabels.take(400))
      //trainLabels.take(20).foreach(label => println("%30s %s %s %f".format(label.token.word, label.targetCategory, label.categoryValue, objective.currentScore(label))))
      //println ("Tr50  accuracy = "+ objective.accuracy(trainLabels.take(20)))
      //println ("Train accuracy = "+ objective.accuracy(trainLabels))
      println ("Test  accuracy = "+ objective.accuracy(testLabels))
    }
    if (false) {
      // Use BP Viterbi for prediction
      for (sentence <- testSentences)
        BP.inferChainMax(sentence.asSeq.map(_.label), model).setToMaximize(null)
        //BP.inferChainSum(sentence.asSeq.map(_.label), model).setToMaximize(null) // max-marginal inference
      
      for (sentence <- trainSentences.take(10)) {
        println("---SumProduct---")
        printTokenMarginals(sentence.asSeq, BP.inferChainSum(sentence.asSeq.map(_.label), model))
        println("---MaxProduct---")
        // printTokenMarginals(sentence.asSeq, BP.inferChainMax(sentence.asSeq.map(_.label), model))
        println("---Gibbs Sampling---")
        predictor.processAll(testLabels, 2)
        sentence.asSeq.foreach(token => printLabel(token.label))
      }
    } else {
      // Use VariableSettingsSampler for prediction
      //predictor.temperature *= 0.1
      predictor.processAll(testLabels, 2)
    }
    println ("Final Test  accuracy = "+ objective.accuracy(testLabels))
    //println("norm " + model.weights.twoNorm)
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
    
    //for (sentence <- testSentences) BP.inferChainMax(sentence.asSeq.map(_.label), model); println ("MaxBP Test accuracy = "+ objective.accuracy(testLabels))
    //for (sentence <- testSentences) BP.inferChainSum(sentence.asSeq.map(_.label), model).setToMaximize(null); println ("SumBP Test accuracy = "+ objective.accuracy(testLabels))
    //predictor.processAll(testLabels, 2); println ("Gibbs Test accuracy = "+ objective.accuracy(testLabels))
  }

  def printTokenMarginals(tokens:Seq[Token], summary:BPSummary): Unit = {
    for (token <- tokens)
      println(token.word + " " + LabelDomain.categories.zip(summary.marginal(token.label).proportions.asSeq).sortBy(_._2).reverse.mkString(" "))
    println()
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
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.target.categoryValue, label.value.category, label.token.toString))
  }
 
  def printDiagnostic(labels:Seq[Label]) : Unit = {
    for (label <- labels; if label.intValue != label.domain.index("O")) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format(if (label.value != label.target.value) label.target.value.category.drop(2) else " ", label.value.category.drop(2)))
      print(label.token.word+" ")
      if (!label.hasNext || label.value != label.next.value) println()
    }
    println()
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
        sentence += new Token(word, wordToFeatures(word,"POS="+pos), label)
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


