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



package cc.factorie.example
import cc.factorie._
import java.io.File

object ChainNER4 {

  // The variable classes
  object TokenDomain extends CategoricalTensorDomain[String]
  class Token(val word:String, features:Seq[String], labelString:String) extends BinaryFeatureVectorVariable[String] with ChainLink[Token,Sentence] {
    def domain = TokenDomain
    val label: Label = new Label(labelString, this)
    this ++= features
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelname: String, val token: Token) extends LabelVariable(labelname) {
    def domain = LabelDomain
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next = token.next.label
    def prev = token.prev.label
  }
  class Sentence extends Chain[Sentence,Token]
  
  // The model
  val model = new TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[Label] { override def statisticsDomains = Tuple1(LabelDomain) },
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[Label, Label] {
      override def statisticsDomains = ((LabelDomain, LabelDomain))
      def unroll1(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      def unroll2(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[Label, Token] {
      override def statisticsDomains = ((LabelDomain, TokenDomain))
      def unroll1(label: Label) = Factor(label, label.token)
      def unroll2(token: Token) = throw new Error("Token values shouldn't change")
    }
  )
  
  // The training objective
  val objective = new TemplateModel(new HammingLossTemplate[Label])
  


  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER4 trainfile testfile")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.links.map(_.label)).take(30000)
    val testLabels = testSentences.flatMap(_.links.map(_.label)).take(2000)
    val allTokens: Seq[Token] = (trainLabels ++ testLabels).map(_.token)
/*
    // Add features from next and previous tokens 
    println("Adding offset features...")
    allTokens.foreach(t => {
      if (t.hasPrev) t ++= t.prev.activeCategories.filter(!_.contains('@')).map(_+"@-1")
      if (t.hasNext) t ++= t.next.activeCategories.filter(!_.contains('@')).map(_+"@+1")
    })
*/
    println("Using "+TokenDomain.dimensionSize+" observable features.")
    
    // Print some significant features
    //println("Most predictive features:")
    //val pllo = new cc.factorie.app.classify.PerLabelLogOdds(trainSentences.flatMap(_.map(_.label)), (label:Label) => label.token)
    //for (label <- LabelDomain.values) println(label.category+": "+pllo.top(label, 20))
    
    // Sample and Learn!
    val startTime = System.currentTimeMillis
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    //val learner = new VariableSettingsSampler[Label](model, objective) with SampleRank with ConfidenceWeightedUpdates { temperature = 0.01 }
    //val learner = new VariableSettingsSampler[Label](model, objective) with SampleRank with GradientAscentUpdates
    //val learner = new cc.factorie.bp.SampleRank2(model, new VariableSettingsSampler[Label](model, objective), new cc.factorie.optimize.StepwiseGradientAscent(model))
    //val learner = new cc.factorie.bp.SampleRank2(model, new VariableSettingsSampler[Label](model, objective), new cc.factorie.optimize.MIRA)
    val learner = new cc.factorie.bp.SampleRank2(new GibbsSampler(model, objective), new cc.factorie.optimize.AROW(model))
    //val learner = new cc.factorie.bp.SampleRank2(new GibbsSampler(model, objective), new cc.factorie.optimize.ConfidenceWeighting(model))
    //val learner = new cc.factorie.bp.SampleRank2(new GibbsSampler(model, objective), new cc.factorie.optimize.MIRA)
    val predictor = new VariableSettingsSampler[Label](model, null) { temperature = 0.01 }
    for (i <- 1 to 3) {
      println("Iteration "+i) 
      learner.processAll(trainLabels)
      trainLabels.take(50).foreach(printLabel _); println; println
      printDiagnostic(trainLabels.take(400))
      predictor.processAll(testLabels)
      println ("Train accuracy = "+ objective.aveScore(trainLabels))
      println ("Test  accuracy = "+ objective.aveScore(testLabels))
    }
    if (true) {
      // Use BP Viterbi for prediction
      for (sentence <- testSentences)
        BP.inferChainMax(sentence.asSeq.map(_.label), model)
        //BP.inferChainSum(sentence.asSeq.map(_.label), model).setToMaximize(null) // max-marginal inference
      
      for (sentence <- trainSentences.take(10)) {
        println("---SumProduct---")
        printTokenMarginals(sentence.asSeq, BP.inferChainSum(sentence.asSeq.map(_.label), model))
        println("---MaxProduct---")
        printTokenMarginals(sentence.asSeq, BP.inferChainMax(sentence.asSeq.map(_.label), model))
        println("---Gibbs Sampling---")
        predictor.processAll(testLabels, 2)
        sentence.asSeq.foreach(token => printLabel(token.label))
      }
    } else {
      // Use VariableSettingsSampler for prediction
      predictor.temperature *= 0.1
      predictor.processAll(testLabels, 2)
    }
    println ("Final Test  accuracy = "+ objective.aveScore(testLabels))
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }

  def printTokenMarginals(tokens:Seq[Token], summary:BPSummary): Unit = {
    //val summary = BP.inferChainSum(tokens.map(_.label), model)
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
    for (label <- labels; if (label.intValue != label.domain.index("O"))) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format((if (label.value != label.target.value) label.target.value.category else " "), label.value.category))
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
        sentence += new Token(word, wordToFeatures(word,"POS="+pos), label)
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


