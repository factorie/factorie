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

package cc.factorie.app.nlp.pos

import cc.factorie._
import app.nlp._
import java.io.File
import app.chain.Observations.addNeighboringFeatureConjunctions
//import optimize.LimitedMemoryBFGS
//import bp._
//import bp.specialized.Viterbi
import util._

object PosFeaturesDomain extends CategoricalDimensionTensorDomain[String]
class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain }

object PosModel extends TemplateModel {
  // Bias term on each individual label
  val bias = new DotTemplateWithStatistics1[PosLabel] {
    //override def statisticsDomains = Tuple1(PosDomain)
    lazy val weights = new la.DenseTensor1(PosDomain.size)
  }
  // Factor between label and observed token
  val local = new DotTemplateWithStatistics2[PosLabel,PosFeatures] {
    //override def statisticsDomains = ((PosDomain, PosFeaturesDomain))
    lazy val weights = new la.DenseTensor2(PosDomain.size, PosFeaturesDomain.dimensionSize)
    def unroll1(label: PosLabel) = Factor(label, label.token.attr[PosFeatures])
    def unroll2(tf: PosFeatures) = Factor(tf.token.posLabel, tf)
  }
  // Transition factors between two successive labels
  val trans = new DotTemplateWithStatistics2[PosLabel, PosLabel] {
    //override def statisticsDomains = ((PosDomain, PosDomain))
    lazy val weights = new la.DenseTensor2(PosDomain.size, PosDomain.size)
    def unroll1(label: PosLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.posLabel, label) else Nil
    def unroll2(label: PosLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.posLabel) else Nil
  }

  // Add the templates
  this += local
  this += bias
  this += trans
}

object PosObjective extends HammingTemplate[PosLabel]

object POS {

  def initPosFeatures(documents: Seq[Document]): Unit = documents.map(initPosFeatures(_))
  def initPosFeatures(document: Document): Unit = {
    for (token <- document.tokens) {
      val rawWord = token.string
      val word = cc.factorie.app.strings.simplifyDigits(rawWord)
      val features = token.attr += new PosFeatures(token)
      features += "W=" + word
      features += "STEM=" + cc.factorie.app.strings.porterStem(word)
      features += "SHAPE2=" + cc.factorie.app.strings.stringShape(rawWord, 2)
      features += "SHAPE3=" + cc.factorie.app.strings.stringShape(rawWord, 3)
      // pre/suf of length 1..9
      //for (i <- 1 to 9) {
      val i = 3
        features += "SUFFIX" + i + "=" + word.takeRight(i)
        features += "PREFIX" + i + "=" + word.take(i)
      //}
      if (token.isCapitalized) features += "CAPITALIZED"
      if (token.string.matches("[A-Z]")) features += "CONTAINS_CAPITAL"
      if (token.string.matches("-")) features += "CONTAINS_DASH"
      if (token.containsDigit) features += "NUMERIC"
      if (token.isPunctuation) features += "PUNCTUATION"
    }
    // add conjunctions of word features
    for (sentence <- document.sentences)
      addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[PosFeatures], "W=", List(-2), List(-1), List(1), List(-2,-1), List(-1,0))
  }

  def train(documents: Seq[Document],
            devDocuments: Seq[Document],
            testDocuments: Seq[Document] = Seq.empty[Document],
            modelFile: String = ""): Unit = {

    val sentences = documents.flatMap(_.sentences.filter(s => s.length > 0))
    val labels = sentences.map(s => s.posLabels) // was flatMap -akm

//    val pieces = labels.map(ls => ModelExample(PosModel, ls))
//    val trainer = new ParallelTrainer(pieces, PosModel.familiesOfClass(classOf[DotFamily]))
//    val optimizer = new LimitedMemoryBFGS(trainer) {
//      override def postIteration(iter: Int): Unit = {
//        PosModel.save(modelFile + "-iter=" + iter)
//        test(documents, "train")
//        test(testDocuments, "test")
//        test(devDocuments, "dev")
//      }
//    }
//    optimizer.optimize()
    val examples = labels.map(l => new optimize.LikelihoodExample(l, InferByBPChainSum))
    val trainer = new optimize.BatchTrainer(PosModel)
    trainer.trainFromExamples(examples)
    //(1 to 100).foreach(i =>trainer.processExamples(examples))

    BinaryCubbieFileSerializer.serialize(new ModelCubbie(PosModel), new File(modelFile))
    test(documents, "train")
    test(testDocuments, "test")
    test(devDocuments, "dev")
  }

  def predictSentence(s: Sentence): Unit = predictSentence(s.tokens.map(_.posLabel))
  def predictSentence(vs: Seq[PosLabel], oldBp: Boolean = false): Unit =
    if (vs.nonEmpty) BP.inferChainMax(vs, PosModel)
    //if (vs.nonEmpty) Viterbi.searchAndSetToMax(vs, PosModel.local, PosModel.trans, PosModel.bias)

  def test(documents: Seq[Document], label: String): Unit = {
    val sentences = documents.flatMap(_.sentences)
    val labels = sentences.flatMap(s => s.tokens.map(_.posLabel))
    labels.map(_.setRandomly())
    sentences.map(predictSentence(_))
    println(label + " accuracy: " + PosObjective.accuracy(labels) + "%")
  }

  var modelLoaded = false
  def load(modelFile: String) = { BinaryCubbieFileSerializer.deserialize(new ModelCubbie(PosModel), new File(modelFile)); modelLoaded = true; }

  def process(documents: Seq[Document]): Unit = documents.map(process(_))
  def process(document: Document): Unit = {
    if (!modelLoaded) throw new Error("First call PosModel.load(\"path/to/model\")")

    // add the labels and features if they aren't there already.
    if (document.tokens.head.attr.get[PosLabel] == None) {
      val defaultCategory = PosDomain.categories.head
      document.tokens.foreach(t => t.attr += new PosLabel(t, defaultCategory))
      initPosFeatures(document)
    }

    document.sentences.foreach(predictSentence(_))
  }
  
  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILE", "")
      val devFile =   new CmdOption("dev", "", "FILE", "")
      val testFile =  new CmdOption("test", "", "FILE", "")
      val takeOnly =  new CmdOption("takeOnly", "-1", "", "")
      val modelDir =  new CmdOption("model", "pos.fac", "DIR", "Directory in which to save the trained model.")
      val runFiles =  new CmdOption("run", List("input.txt"), "FILE...", "Plain text files from which to get data on which to run.")
    }
    opts.parse(args)
    import opts._

    if (trainFile.wasInvoked && devFile.wasInvoked && testFile.wasInvoked && modelDir.wasInvoked) {
      Template.enableCachedStatistics = false // for contention free parallelism
      val labelMaker = (t: Token, l: String) => new PosLabel(t, l)
      def load(file: String) = LoadOWPL.fromFilename(file, labelMaker, takeOnly.value.toInt)

      val trainDocs = load(trainFile.value)
      val devDocs = load(devFile.value)
      val testDocs = load(testFile.value)

      println("train sentences: " + trainDocs.flatMap(_.sentences).size)
      println("docs loaded")

      initPosFeatures(trainDocs ++ devDocs ++ testDocs)
      println("train, test, and dev features calculated")

      train(trainDocs, devDocs, testDocs, modelDir.value)
    }
    else if (runFiles.wasInvoked) {
      println(this.getClass().getResource("pos-model").toURI)
      println(this.getClass().getResource("pos-model").toString)
    }
    else {
      println("Either provide files to process and a model, or provide train, test, dev files and a model ouput location.")
    }
  }

}

