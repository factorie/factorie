/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.io.Source
import cc.factorie._ 
import cc.factorie.er._
import cc.factorie.application.LabeledTokenSeqs
import cc.factorie.application.LabeledTokenSeqs.LabeledTokenSeq
import scala.collection.mutable.ArrayBuffer
import java.io.File

/** Demo of CoNLL NER with lots of features.  Aiming for near state-of-the-art accuracy, but not yet finished. */
object ChainNER2 {

  // Define the variable classes
  class Token(word:String, labelString:String) extends LabeledTokenSeqs.Token[Label,Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(labelString:String, token:Token) extends LabeledTokenSeqs.Label[Token,Label](labelString, token)

  // Define the model
  val model = new Model(
    Foreach[Label] { label => Score(label) } % "Prior",
    Foreach[Label] { label => Score(label, label.token) } % "LabelToken",
    Foreach[Label] { label => Score(label.prev, label) } % "LabelLabel"
    //Foreach[Label] { label => Score(label.prev, label, label.token.tags) },
    //Foreach[Label] { label => Score(label.prev, label, label.next) },
    //Foreach[Label] { label => Score(label.prev, label, label.next, label.token.tags) }
  )

  def main(args: Array[String]) : Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile")

    // Read training and testing data.  The function 'featureExtractor' function is defined below
    val trainSentences = 
      LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(new File(args(0))), (word,lab)=>new Token(word,lab), featureExtractor _, "-DOCSTART-".r)
    val testSentences = 
      LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(new File(args(1))), (word,lab)=>new Token(word,lab), featureExtractor _, "-DOCSTART-".r)

    // Change from CoNLL's IOB notation to to BIO notation
    (trainSentences ++ testSentences).foreach(s => { 
      s.foreach(t => {
        if (t.label.value(0) == 'I' && (!t.hasPrev || t.prev.label.value.substring(1) != t.label.value.substring(1))) {
          val newValue = "B"+t.label.value.substring(1) 
          t.label.value = newValue
          t.label.trueValue = newValue
        }
      })}) 
      
    // Make features of offset conjunctions
    (trainSentences ++ testSentences).foreach(s => s.addNeighboringFeatureConjunctions(List(0), List(0,0), List(-1), List(-1,0), List(1)))

    // Gather tokens into documents
    val documents = new ArrayBuffer[ArrayBuffer[Token]]; documents += new ArrayBuffer[Token]
    (trainSentences ++ testSentences).foreach(s => if (s.length == 0) documents += new ArrayBuffer[Token] else documents.last ++= s)
    // For documents that have a "-" within the first three words, the first word is a HEADER feature; apply it to all words in the document
    documents.foreach(d => if (d.take(3).map(_.word).contains("-")) { val f = "HEADER="+d(0).word.toLowerCase; d.foreach(t => t += f)})

    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    (trainSentences ++ testSentences).foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t += "SENTENCEUPPERCASE"))

    // Add features for character n-grams between sizes 2 and 5
    (trainSentences ++ testSentences).foreach(s => s.foreach(t => if (t.word.matches("[A-Za-z]+")) t ++= t.charNGrams(2,5).map(n => "NGRAM="+n)))

    // Add features from window of 4 words before and after
    (trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplify(t2.word).toLowerCase)))
    (trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplify(t2.word).toLowerCase)))

    // Put features of first mention on later mentions
    documents.foreach(d => {
      d.foreach(t => {
        if (t.isCapitalized && t.word.length > 1 && !t.values.exists(f => f.matches(".*FIRSTMENTION.*"))) {
          //println("Looking for later mentions of "+t.word)
          var t2 = t
          while (t2.hasNext) {
            t2 = t2.next
            if (t2.word == t.word) { /*println("Adding FIRSTMENTION to "+t2.word);*/ t2 ++= t.values.filter(_.contains("@")).map(f => "FIRSTMENTION="+f) }
          }
        }
      })
    })
    
    // Print data stats
    println("Training on "+trainSentences.size+" sentences, "+trainSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Testing  on "+testSentences.size+" sentences, "+testSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Domain size = "+Domain[Token].size)

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.labels)
    val testLabels = testSentences.flatMap(_.labels)

    // Train
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val predictor = new GibbsSampler1[Label](model) { temperature = 0.01 }
    val learner = new GibbsSampler1[Label](model) with SampleRank with ConfidenceWeightedUpdates {
      temperature = 0.01
      // Speed training by sometimes skipping inference of lowercase training words that are already correct
      override def preProcessHook(label:Label) = if (label.valueIsTruth && !label.token.isCapitalized && Global.random.nextDouble > 0.5) null else label
      // At the end of each iteration, print some diagnostics
      override def postIterationHook(): Boolean = {
        predictor.process(testLabels, 1)
        println("Train errors")
        printErrors(trainLabels, 200)
        println("Test errors")
        printErrors(testLabels, 200)
        println("Iteration "+iterationCount)
        println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](trainLabels))
        println("TEST\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](testLabels))
        true
      }
    }

    // Train for 10 iterations
    val startTime = System.currentTimeMillis
    learner.process(trainLabels, 10)
    println("Finished training in "+(System.currentTimeMillis-startTime)/60000.0+" minutes.")
    
    // Predict, also by sampling, visiting each variable 4 times.
    List(1.0, 0.1, 0.01, 0.001).foreach(temp => { predictor.temperature = temp; predictor.process(testLabels, 1) })
    println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](trainLabels))
    println("TEST\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](testLabels))

    model.save("chainner2.factorie")
  }

  
  /** Print diagnostics for error tokens, plus two true tokens before and after. */
  def printErrors(labels:Seq[Label], maxErrors:Int): Unit = {
    val contextSize = 2
    var count = 0
    var i = 0
    while (i < labels.length && count < maxErrors) {
      val label = labels(i)
      if (!label.valueIsTruth && label.hasPrev && label.hasNext && count < maxErrors) {
        var j = Math.max(i-contextSize, 0); var numTruthsAfter = -contextSize
        do {
          val l = labels(j)
          println("%s %-6s %-6s %-18s %s".format((if (l.valueIsTruth) " " else "*"), l.trueValue, l.value, l.token.word, l.token.toString))
          if (l.valueIsTruth) numTruthsAfter += 1 else { numTruthsAfter = 0; count += 1 }
          j += 1
        } while (numTruthsAfter < contextSize && j < labels.length && count < maxErrors) 
        println
        i = j - 1
      }
      i += 1
    }
  }
 
  // Simplified form of word for feature generation
  def simplify(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word.toLowerCase
  }
  
  // Collection of features created using raw features from data file 
  def featureExtractor(initialFeatures:Seq[String]) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    f += "SHAPE="+LabeledTokenSeqs.wordShape(word, 2)
    f += "W="+simplify(word)
    f += "POS="+initialFeatures(1)
    f += "PHRASE="+initialFeatures(2)
    if (Character.isUpperCase(word(0))) f += "CAPITALIZED"
    f
  }
  
}


