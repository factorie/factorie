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

package cc.factorie.app.nlp.ner.chain

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._

object NerFeaturesDomain extends CategoricalVectorDomain[String]
class NerFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
  def domain = NerFeaturesDomain
  override def skipNonCategories = true
}

object NerModel extends TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[NerLabel], 
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[NerLabel, NerLabel] {
      def unroll1(label: NerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[NerLabel], label) else Nil
      def unroll2(label: NerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[NerLabel]) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[NerLabel,NerFeatures] {
      def unroll1(label: NerLabel) = Factor(label, label.token.attr[NerFeatures])
      def unroll2(tf: NerFeatures) = Factor(tf.token.attr[NerLabel], tf)
    }
  )

object NerObjective extends TemplateModel(new ZeroOneLossTemplate[NerLabel])


object NER {
  
  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: NER trainfile testfile.")

    // Read in the data
    val trainDocuments = LoadConll2003.fromFilename(args(0))
    val testDocuments = LoadConll2003.fromFilename(args(1))

    // Add features for NER
    val Capitalized = "^[A-Z].*".r
    val Numeric = "^[0-9]+$".r
    val Punctuation = "[-,\\.;:?!()]+".r
    for (document <- (trainDocuments ++ testDocuments)) {
      for (token <- document) {
        val word = token.string
        val features = token.attr += new NerFeatures(token)
        features += "W="+word
        features += "SHAPE="+cc.factorie.app.strings.stringShape(word, 2)
        features += "SUFFIX3="+word.takeRight(3)
        features += "PREFIX3="+word.take(3)
        features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
        if (Capitalized.findFirstMatchIn(word) != None) features += "CAPITALIZED"
        if (Numeric.findFirstMatchIn(word) != None) features += "NUMERIC"
        if (Punctuation.findFirstMatchIn(word) != None) features += "PUNCTUATION"
      }
      for (sentence <- document.sentences)
        cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence, (t:Token)=>t.attr[NerFeatures], List(1), List(-1))
    }
    println("Example Token features")
    println(trainDocuments(3).tokens.take(10).map(_.attr[NerFeatures].toString).mkString("\n"))
    println("Num TokenFeatures = "+NerFeaturesDomain.dimensionDomain.size)
    
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainDocuments.flatten.map(_.attr[NerLabel]) //.take(10000)
    val testLabels = testDocuments.flatten.map(_.attr[NerLabel]) //.take(2000)
    
    def printEvaluation(iteration:String): Unit = {
      //println("Train Token accuracy = "+ NerObjective.aveScore(trainLabels))
      //println(" Test Token accuracy = "+ NerObjective.aveScore(testLabels))
      for (docs <- List(trainDocuments, testDocuments)) {
        if (docs.length > 300) println ("TRAIN") else println("TEST") // Fragile
        val tokenEvaluation = new LabelEvaluation(NerDomain)
        for (doc <- docs; token <- doc) tokenEvaluation += token.attr[NerLabel]
        println(tokenEvaluation)
        val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[NerLabel](NerDomain.categoryValues.filter(_.length > 2).map(_.substring(2)))
        for (doc <- docs; sentence <- doc.sentences) segmentEvaluation += sentence.map(_.attr[NerLabel])
        println("Segment evaluation")
        println(segmentEvaluation)
        println("Iteration "+iteration)
      }
    }

    // Train for 5 iterations
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    printEvaluation("Initialization")
    val learner = new VariableSettingsSampler[NerLabel](NerModel, NerObjective) with SampleRank with GradientAscentUpdates
    val predictor = new VariableSettingsSampler[NerLabel](NerModel)
    for (i <- 1 until 5) {
      learner.processAll(trainLabels)
      predictor.processAll(testLabels)
      printEvaluation(i.toString)
    }

    // Predict, also by sampling, visiting each variable 3 times.
    //predictor.processAll(testLabels, 3)
    for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    
    // Evaluate
    printEvaluation("FINAL")
  }

  
}
