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
import cc.factorie.er._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._

/** Simple, introductory linear-chain CRF for named-entity recognition,
    using FACTORIE's "entity-relationship" language to define model structure.

    Demonstrates model creation, training and testing.
    Overly simple features to not, however, provide very high accuracy.
    See ChainNER3 for a related example with better features. 
    @author Andrew McCallum */
object ChainNER1a {
  object TokenFeaturesDomain extends CategoricalTensorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  val model = new TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[ChainNerLabel] {
      def statisticsDomains = Seq(Conll2003NerDomain) // TODO But sometimes there should be subclasses here? 
    }, 
    // Factor between label and observed token
    new TemplateWithDotStatistics2[ChainNerLabel,TokenFeatures] {
      def statisticsDomains = Seq(Conll2003NerDomain, TokenFeaturesDomain)
      def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
    },
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[ChainNerLabel, ChainNerLabel] {
      def statisticsDomains = Seq(Conll2003NerDomain, Conll2003NerDomain)
      def unroll1(label: ChainNerLabel) = if (label.token.hasPrev) Factor(label.token.prev.attr[ChainNerLabel], label) else Nil
      def unroll2(label: ChainNerLabel) = if (label.token.hasNext) Factor(label, label.token.next.attr[ChainNerLabel]) else Nil
    }
  )

  
  def main(args:Array[String]): Unit = {
    val trainDocuments = LoadConll2003.fromFilename(args(0))
    val testDocuments = LoadConll2003.fromFilename(args(1))
    for (document <- (trainDocuments ++ testDocuments); token <- document) {
      val features = new TokenFeatures(token)
      features += "W="+token.string
      features += "SHAPE="+cc.factorie.app.strings.stringShape(token.string, 2)
      token.attr += features
    }
    val trainLabels = trainDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(10000)
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(2000)
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    val learner = new VariableSettingsSampler[ChainNerLabel](model) with SampleRank with GradientAscentUpdates
    val predictor = new VariableSettingsSampler[ChainNerLabel](model, null)
    for (iteration <- 1 until 5) {
      learner.processAll(trainLabels)
      predictor.processAll(testLabels)
    }
    println("Train Acccuracy = "+HammingLossObjective.aveScore(trainLabels))
    println("Test Acccuracy = "+HammingLossObjective.aveScore(testLabels))
  }
}
  
// TODO Consider implementing cc.factorie.app.nlp.Token as a cc.factorie.er.Entity, 
// so that a model definition like in the following could work again
/*
object ChainNER1 {
  
  // Define the variable classes
  object TokenDomain extends CategoricalVectorDomain[String]
  class Token(word:String, labelString:String) extends labeled.Token[Sentence,Label,Token](word) {
    val label = new Label(labelString, this)
    def domain = TokenDomain
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

