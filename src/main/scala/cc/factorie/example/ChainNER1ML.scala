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

import java.io.File
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import collection.mutable.ArrayBuffer

object ChainNER1ML {
  object TokenFeaturesDomain extends CategoricalVectorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  val model = new TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[ChainNerLabel], 
    // Factor between label and observed token
    new TemplateWithDotStatistics2[ChainNerLabel,TokenFeatures] {
      def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
    },
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[ChainNerLabel, ChainNerLabel] {
      def unroll1(label: ChainNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[ChainNerLabel], label) else Nil
      def unroll2(label: ChainNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[ChainNerLabel]) else Nil
    }
  )

  
  def main(args:Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    val trainDocuments = LoadConll2003.fromFilename(args(0))
    val testDocuments = LoadConll2003.fromFilename(args(1))
    for (document <- (trainDocuments ++ testDocuments); token <- document) {
      val features = new TokenFeatures(token)
      features += "W="+token.string
      features += "SHAPE="+cc.factorie.app.strings.stringShape(token.string, 2)
      token.attr += features
    }
    val trainLabels = trainDocuments.flatten.map(_.attr[ChainNerLabel]) //.take(10000)
    val testLabels = testDocuments.flatten.map(_.attr[ChainNerLabel]) //.take(2000)
    
    // Get the variables to be inferred
    val trainLabelsSentences: Seq[Seq[NerLabel]] = trainDocuments.map(_.map(_.nerLabel))
    val  testLabelsSentences: Seq[Seq[NerLabel]] =  testDocuments.map(_.map(_.nerLabel))
    //val trainVariables = trainLabels
    //val testVariables = testLabels
    //val allTestVariables = testVariables.flatMap(l => l)
    //val allTokens: Seq[Token] = (trainSentences ++ testSentences).flatten
    // To enable Template.cachedStatistics
    for (doc <- (trainDocuments ++ testDocuments); token <- doc) token.attr[TokenFeatures].freeze

    // Train and test
    println("*** Starting training (#sentences=%d)".format(trainDocuments.map(_.sentences.size).sum))
    val start = System.currentTimeMillis
    val trainer = new LogLinearMaximumLikelihood(model)
    trainer.processAll(trainLabelsSentences, 1) // Do just one iteration for initial timing
    println("One iteration took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
    //System.exit(0)

    trainer.processAll(trainLabelsSentences) // Keep training to convergence

    val objective = HammingLossObjective
    // slightly more memory efficient - kedarb
    println("*** Starting inference (#sentences=%d)".format(testDocuments.map(_.sentences.size).sum))
    testLabelsSentences.foreach {
      variables => new BPInferencer(model).inferTreewiseMax(variables)
    }
    println("test token accuracy=" + objective.aveScore(testLabelsSentences.flatten))

    println("Total training took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
  }
}
