package cc.factorie.tutorial

import cc.factorie._
import java.io.File
import cc.factorie.util.BinarySerializer
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosLabel, PennPosDomain}
import app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.optimize.Trainer
import cc.factorie.variable.{LabeledVarWithTarget, BinaryFeatureVectorVariable, CategoricalVectorDomain}
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, TemplateModel}
import cc.factorie.infer.{InferByBPChain, BP}
import cc.factorie.app.nlp.load.LoadOWPL

/**
 * Author: martin
 * Date: 2/8/12
 *
 * A simple chain POS tagger trained using L-BFGS on forward-backawrd gradients.
 * Prediction uses Viterbi.
 */

/** A simple demonstration of part-of-speech tagging with a finite-state linear-chain CRF.
    The set of features is impoverished in this demonstration, so the accuracy is not high */
object ForwardBackwardPOS {
  
  object PosFeaturesDomain extends CategoricalVectorDomain[String]
  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain }

  object PosModel extends TemplateModel with Parameters {
    // Factor between label and observed token
    val localTemplate = this += new DotTemplateWithStatistics2[PennPosLabel,PosFeatures] {
      //override def statisticsDomains = ((PosDomain, PosFeaturesDomain))
      val weights = Weights(new la.DenseTensor2(PennPosDomain.size, PosFeaturesDomain.dimensionSize))
      def unroll1(label: PennPosLabel) = Factor(label, label.token.attr[PosFeatures])
      def unroll2(tf: PosFeatures) = Factor(tf.token.posLabel, tf)
    }
    // Transition factors between two successive labels
    val transTemplate = this += new DotTemplateWithStatistics2[PennPosLabel, PennPosLabel] {
      //override def statisticsDomains = ((PosDomain, PosDomain))
      val weights = Weights(new la.DenseTensor2(PennPosDomain.size, PennPosDomain.size))
      def unroll1(label: PennPosLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.posLabel, label) else Nil
      def unroll2(label: PennPosLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.posLabel) else Nil
    }
  }

  def initPosFeatures(documents: Seq[Document]): Unit = documents.map(initPosFeatures(_))
  def initPosFeatures(document: Document): Unit = {
    for (token <- document.tokens) {
      val rawWord = token.string
      val word = cc.factorie.app.strings.simplifyDigits(rawWord)
      val features = new PosFeatures(token)
      // val features = token.attr += new PosFeatures(token)
      token.attr += features
      features += "W=" + word
      features += "SHAPE3=" + cc.factorie.app.strings.stringShape(rawWord, 3)
      val i = 3
      features += "SUFFIX" + i + "=" + word.takeRight(i)
      features += "PREFIX" + i + "=" + word.take(i)
      if (token.isCapitalized) features += "CAPITALIZED"
      if (token.string.matches("[A-Z]")) features += "CONTAINS_CAPITAL"
      if (token.string.matches("-")) features += "CONTAINS_DASH"
      if (token.containsDigit) features += "NUMERIC"
      if (token.isPunctuation) features += "PUNCTUATION"
    }
    for (sentence <- document.sentences)
      addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[PosFeatures], "W=", List(-2), List(-1), List(1), List(-2,-1), List(-1,0))
  }

  def percentageSetToTarget[L <: LabeledVarWithTarget](ls: Seq[L]): Double = {
    val numCorrect = ls.foldLeft(0.0)((partialSum, label) => partialSum + {if (label.valueIsTarget) 1 else 0})
    numCorrect / ls.size * 100
  }

  def predictSentence(s: Sentence): Unit = predictSentence(s.tokens.map(_.posLabel))
  def predictSentence(vs: Seq[PennPosLabel], oldBp: Boolean = false): Unit =
    BP.inferChainMax(vs, PosModel).setToMaximize(null)
    //Viterbi.searchAndSetToMax(vs, PosModel.localTemplate, PosModel.transTemplate)

  def train(
        documents: Seq[Document],
        devDocuments: Seq[Document],
        testDocuments: Seq[Document] = Seq.empty[Document],
        iterations: Int = 100,
        modelFile: String = "",
        extraId: String = "")(implicit random: scala.util.Random): Unit = {

    def testSavePrint(label: String): Unit = {
      test(documents, label = "train")
      if (testDocuments.nonEmpty)
        test(testDocuments, label = "test")
      if (devDocuments.nonEmpty)
        test(devDocuments, label = "dev")
      if (modelFile != "")
        BinarySerializer.serialize(PosFeaturesDomain, PosModel, new File(modelFile + label + extraId), gzip = true)
    }

    val sentences: Seq[Sentence] = documents.flatMap(_.sentences)
    val sentenceLabels = sentences.map(_.posLabels).filter(_.size > 0)

    val examples = sentenceLabels.map(s => new optimize.LikelihoodExample(s, PosModel, InferByBPChain))
    Trainer.onlineTrain(PosModel.parameters, examples, maxIterations=10)
    testSavePrint("final")
  }

  def test(documents: Seq[Document], label: String = "test"): Unit = {
    implicit val random = new scala.util.Random(0)
    val sentences = documents.flatMap(_.sentences)
    val labels = sentences.map(_.tokens).flatMap(_.map(t => t.posLabel))
    labels.map(_.setRandomly)
    sentences.map(predictSentence(_))
    println(label + " accuracy: " + percentageSetToTarget(labels) + "%")
  }

  var modelLoaded = false
  def load(modelFile: String) = { BinarySerializer.deserialize(PosFeaturesDomain, PosModel, new File(modelFile), gzip = true); modelLoaded = true }

  def process(documents: Seq[Document]): Unit = documents.map(process(_))
  def process(document: Document): Unit = {
    if (!modelLoaded) throw new Error("The model should be loaded before documents are processed.")

    // add the labels and features if they aren't there already.
    if (document.tokens.head.attr.get[PennPosLabel] == None) {
      document.tokens.foreach(t => t.attr += labelMaker(t))
      initPosFeatures(document)
    }

    document.sentences.foreach(predictSentence(_))
  }

  lazy val defaultCategory = {
    try { PennPosDomain.categories.head }
    catch { case e: NoSuchElementException => throw new Error("The domain must be loaded before it is accessed.") }
  }
  def labelMaker(t: Token, l: String = defaultCategory) = new PennPosLabel(t, l)

  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILE", "An OWPL train file.") { override def required = true }
      val devFile =   new CmdOption("dev", "", "FILE", "An OWPL dev file") { override def required = true }
      val testFile =  new CmdOption("test", "", "FILE", "An OWPL test file.") { override def required = true }
      val takeOnly =  new CmdOption("takeOnly", "-1", "INT", "A limit on the number of sentences loaded from each file.")
      val iterations =new CmdOption("iterations", "10", "INT", "The number of iterations to train for.")
      val modelDir =  new CmdOption("model", "", "DIR", "Directory in which to save the trained model.")
      val extraId =   new CmdOption("label", "", "STRING", "An extra identifier.  Useful for testing different sets of features.")
    }
    opts.parse(args)
    implicit val random = new scala.util.Random(0)
    import opts._

    if (trainFile.wasInvoked && devFile.wasInvoked && testFile.wasInvoked) {
      def load(f: String): Seq[Document] = LoadOWPL.fromFilename(f, labelMaker, takeOnly.value.toInt)

      // load the data
      val trainDocs = load(trainFile.value)
      val devDocs = load(devFile.value)
      val testDocs = load(testFile.value)
      //println("train sentences: " + trainDocs.flatMap(_.sentences).size)
      //println("dev   sentences: " + devDocs.flatMap(_.sentences).size)
      //println("test  sentences: " + testDocs.flatMap(_.sentences).size)
      //println("docs loaded")

      // calculate features
      initPosFeatures(trainDocs ++ devDocs ++ testDocs)
      //println("train, dev, and test features calculated")

      train(trainDocs, devDocs, testDocs,
        iterations = iterations.value.toInt,
        modelFile = modelDir.value,
        extraId = extraId.value
      )
    }
  }

}
