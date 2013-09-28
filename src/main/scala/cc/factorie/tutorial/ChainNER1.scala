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
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.infer._
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load._
import cc.factorie.app.nlp.ner._
import java.io.File
import cc.factorie.variable.{HammingObjective, BinaryFeatureVectorVariable, CategoricalVectorDomain}
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, DotTemplateWithStatistics1, TemplateModel}
import cc.factorie.infer.{VariableSettingsSampler, GibbsSampler}

/** Simple, introductory linear-chain CRF for named-entity recognition.
    Demonstrates model creation, training and testing.
    Overly simple features to not, however, provide very high accuracy.
    See ChainNER3 for a related example with better features. 
    @author Andrew McCallum */

/*
object ChainNER1a {
  object TokenFeaturesDomain extends CategoricalVectorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  val model = new TemplateModel with Parameters {
    val bias = this += 
      // Bias term on each individual label
      new DotTemplateWithStatistics1[BioConllNerLabel] {
        val weights = Weights(new la.DenseTensor1(BioConllNerDomain.size))
      }
    val evidence = this +=
      // Factor between label and observed token
      new DotTemplateWithStatistics2[BioConllNerLabel,TokenFeatures] {
        val weights = Weights(new la.DenseTensor2(BioConllNerDomain.size, TokenFeaturesDomain.dimensionSize))
        def unroll1(label: BioConllNerLabel) = Factor(label, label.token.attr[TokenFeatures])
        def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[BioConllNerLabel], tf)
      }
    val markov = this +=
      // Transition factors between two successive labels
      new DotTemplateWithStatistics2[BioConllNerLabel, BioConllNerLabel] {
        val weights = Weights(new la.DenseTensor2(BioConllNerDomain.size, BioConllNerDomain.size))
        def unroll1(label: BioConllNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[BioConllNerLabel], label) else Nil
        def unroll2(label: BioConllNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[BioConllNerLabel]) else Nil
      }
  }

  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val trainDocuments = LoadConll2003.fromFilename(args(0))
    val testDocuments = LoadConll2003.fromFilename(args(1))
    for (document <- trainDocuments ++ testDocuments; token <- document.tokens) {
      val features = new TokenFeatures(token)
      features += "W="+token.string
      features += "SHAPE="+cc.factorie.app.strings.stringShape(token.string, 2)
      token.attr += features
    }
    val trainLabels : Seq[BioConllNerLabel] = trainDocuments.map(_.tokens).flatten.map(_.attr[BioConllNerLabel]) //.take(10000)
    val testLabels : Seq[BioConllNerLabel] = testDocuments.map(_.tokens).flatten.map(_.attr[BioConllNerLabel]) //.take(2000)
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val pieces = trainLabels.map(l => new SampleRankExample(l, new GibbsSampler(model, HammingObjective)))
    val predictor = new VariableSettingsSampler[BioConllNerLabel](model, null)
    Trainer.onlineTrain(model.parameters, pieces, maxIterations=5, evaluate = ()=> {
      predictor.processAll(testLabels)
      println("Train Acccuracy = "+HammingObjective.accuracy(trainLabels))
      println("Test Acccuracy = "+HammingObjective.accuracy(testLabels))
      println()
    })
  }

  object LabelDomain extends CategoricalDomain[String]
  class Label(tag:String, token:Token) extends labeled.Label[Sentence,Token,Label](tag, token) {
    def domain = LabelDomain
  }
  class Sentence extends labeled.TokenSeq[Token,Label,Sentence]

  // Define the model:
  val model = new TemplateModel(
    Foreach[Label] { label => Score(label) },
    Foreach[Label] { label => Score(label.prev, label, label.token) }
  )

  def main(args: Array[String]) : Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    
    // Read training and testing data.
    val trainSentences = labeled.TokenSeq.fromOWPL(new File(args(0)), ()=>new Sentence, (word,lab)=>new Token(word,lab))
    val testSentences =  labeled.TokenSeq.fromOWPL(new File(args(1)), ()=>new Sentence, (word,lab)=>new Token(word,lab))

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.labels)
    val testLabels = testSentences.flatMap(_.labels)
    (trainLabels ++ testLabels).foreach(_.setRandomly()) 

    // Train for 5 iterations
    val learner = new VariableSettingsSampler[Label](model) with SampleRank with GradientAscentUpdates 
    learner.processAll(trainLabels, 5)  // Train for 5 iterations through all Labels

    // Predict, also by sampling, visiting each variable 3 times.
    val predictor = new VariableSettingsSampler[Label](model)
    predictor.processAll(testLabels, 3)
    
    // Evaluate
    println("TRAIN "+labeled.labelEvaluation(trainLabels))
    println("TEST  "+labeled.labelEvaluation(testLabels))
  }

}

*/

