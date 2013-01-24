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
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import collection.mutable.{ArrayBuffer, Seq => MSeq}

object ChainNER1ML {
  object TokenFeaturesDomain extends CategoricalDimensionTensorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  val model = new CombinedModel(
    // Bias term on each individual label 
    new DotTemplateWithStatistics1[ChainNerLabel] {
      //def statisticsDomains = Tuple1(Conll2003NerDomain)
      lazy val weights = new la.DenseTensor1(Conll2003NerDomain.size)
    },
    // Factor between label and observed token
    new DotTemplateWithStatistics2[ChainNerLabel,TokenFeatures] {
      //def statisticsDomains = ((Conll2003NerDomain, TokenFeaturesDomain))
      lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, TokenFeaturesDomain.dimensionSize)
      def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
    },
    // Transition factors between two successive labels
    new DotTemplateWithStatistics2[ChainNerLabel, ChainNerLabel] {
      //def statisticsDomains = ((Conll2003NerDomain, Conll2003NerDomain))
      lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, Conll2003NerDomain.size)
      def unroll1(label: ChainNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[ChainNerLabel], label) else Nil
      def unroll2(label: ChainNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[ChainNerLabel]) else Nil
    }
  )

  
  def main(args:Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    val trainDocuments = LoadConll2003.fromFilename(args(0))
    val testDocuments = LoadConll2003.fromFilename(args(1))
    for (document <- (trainDocuments ++ testDocuments); token <- document.tokens) {
      val features = new TokenFeatures(token)
      features += "W="+token.string
      features += "SHAPE="+cc.factorie.app.strings.stringShape(token.string, 2)
      token.attr += features
    }
    val trainLabels = trainDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(10000)
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(2000)
    
    // Get the variables to be inferred
    val trainLabelsSentences: Seq[Seq[NerLabel]] = trainDocuments.map(_.tokens.map(_.nerLabel))
    val  testLabelsSentences: Seq[Seq[NerLabel]] = testDocuments.map(_.tokens.map(_.nerLabel))
    //val trainVariables = trainLabels
    //val testVariables = testLabels
    //val allTestVariables = testVariables.flatMap(l => l)
    //val allTokens: Seq[Token] = (trainSentences ++ testSentences).flatten
    // To enable Template.cachedStatistics
    //for (doc <- (trainDocuments ++ testDocuments); token <- doc.tokens) token.attr[TokenFeatures].freeze

    // Train and test
    println("*** Starting training (#sentences=%d)".format(trainDocuments.map(_.sentences.size).sum))
    val start = System.currentTimeMillis
    //throw new Error("DotMaximumLikelihood not yet working for linear-chains")

    val examples = trainLabelsSentences.map(s => new LikelihoodExample(s, InferByBPChainSum))
    val learner = new BatchTrainer(model)
    (1 to 10).foreach(_ => learner.processExamples(examples))
    val objective = HammingObjective
    // slightly more memory efficient - kedarb
    println("*** Starting inference (#sentences=%d)".format(testDocuments.map(_.sentences.size).sum))
    testLabelsSentences.foreach {
      variables => cc.factorie.BP.inferChainMax(variables, model)
    }
    println("test token accuracy=" + objective.accuracy(testLabelsSentences.flatten))

    println("Total training took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
  }
}
