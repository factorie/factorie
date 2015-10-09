/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import cc.factorie.infer.InferByBPChain
import cc.factorie.model.{DotTemplateWithStatistics1, DotTemplateWithStatistics2, Parameters, TemplateModel}
import cc.factorie.optimize.{LikelihoodExample, Trainer}
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain, HammingObjective}

/**
 * An example of a linear-chain CRF system NER which manually defines the model.
 *
 * For an example using actual factorie infrastructure see app.nlp.ner.ConllChainNer
 */
object ChainNERExample {
  object TokenFeaturesDomain extends CategoricalVectorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  val model = new TemplateModel with Parameters {
    addTemplates(
      // Bias term on each individual label
      new DotTemplateWithStatistics1[BioConllNerTag] {
        //def statisticsDomains = Tuple1(Conll2003NerDomain)
        val weights = Weights(new la.DenseTensor1(BioConllNerDomain.size))
      },
      // Factor between label and observed token
      new DotTemplateWithStatistics2[BioConllNerTag,TokenFeatures] {
        //def statisticsDomains = ((Conll2003NerDomain, TokenFeaturesDomain))
        val weights = Weights(new la.DenseTensor2(BioConllNerDomain.size, TokenFeaturesDomain.dimensionSize))
        def unroll1(label: BioConllNerTag) = Factor(label, label.token.attr[TokenFeatures])
        def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[BioConllNerTag], tf)
      },
      // Transition factors between two successive labels
      new DotTemplateWithStatistics2[BioConllNerTag, BioConllNerTag] {
        //def statisticsDomains = ((Conll2003NerDomain, Conll2003NerDomain))
        val weights = Weights(new la.DenseTensor2(BioConllNerDomain.size, BioConllNerDomain.size))
        def unroll1(label: BioConllNerTag) = if (label.token.hasPrev) Factor(label.token.prev.attr[BioConllNerTag], label) else Nil
        def unroll2(label: BioConllNerTag) = if (label.token.hasNext) Factor(label, label.token.next.attr[BioConllNerTag]) else Nil
      }
    )
  }


  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    val trainDocuments = load.LoadConll2003.fromFilename(args(0))
    val testDocuments = load.LoadConll2003.fromFilename(args(1))
    for (document <- trainDocuments ++ testDocuments; token <- document.tokens) {
      val features = new TokenFeatures(token)
      features += "W="+token.string
      features += "SHAPE="+cc.factorie.app.strings.stringShape(token.string, 2)
      token.attr += features
    }

    val trainLabelsSentences: Seq[Seq[LabeledBioConllNerTag]] = trainDocuments.map(_.tokens.toSeq.map(_.attr[LabeledBioConllNerTag]))
    val  testLabelsSentences: Seq[Seq[LabeledBioConllNerTag]] = testDocuments.map(_.tokens.toSeq.map(_.attr[LabeledBioConllNerTag]))
    assert(!testLabelsSentences.contains(null))

    // Train and test
    println("*** Starting training (#sentences=%d)".format(trainDocuments.map(_.sentences.size).sum))
    val start = System.currentTimeMillis

    val examples = trainLabelsSentences.map(s => new LikelihoodExample(s, model, InferByBPChain))
    Trainer.batchTrain(model.parameters, examples)
    println("*** Starting inference (#sentences=%d)".format(testDocuments.map(_.sentences.size).sum))
    testLabelsSentences.foreach {
      variables => cc.factorie.infer.BP.inferChainMax(variables, model).setToMaximize(null)
    }
    println("test token accuracy=" + HammingObjective.accuracy(testLabelsSentences.flatten))

    println("Total training took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
  }
}
