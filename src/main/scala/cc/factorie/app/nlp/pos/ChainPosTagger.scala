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
package cc.factorie.app.nlp.pos

import cc.factorie.app.nlp._
import cc.factorie.app.chain.ChainModel
import cc.factorie.app.chain.Observations._
import java.io._
import cc.factorie.util.{HyperparameterMain, ClasspathURL, BinarySerializer}
import cc.factorie.optimize.Trainer
import cc.factorie.variable.{HammingObjective, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalVariable, LabeledVar, LabeledMutableDiscreteVar}
import scala.reflect.ClassTag

/** A linear-chain CRF part-of-speech tagger, doing inference by Viterbi.
    @author Alexandre Passos, Andrew McCallum
 */
class  ChainPosTagger[A<:PosTag](tagConstructor:(Token)=>A)(implicit ct:ClassTag[A]) extends DocumentAnnotator {
  def this(tagConstructor:(Token)=>A, url:java.net.URL)(implicit ct:ClassTag[A]) = { this(tagConstructor); deserialize(url.openConnection().getInputStream) }
  def process(document: Document) = {
    document.sentences.foreach(s => {
      if (s.nonEmpty) {
        s.tokens.foreach(t => if (!t.attr.contains[A]) t.attr += tagConstructor(t))
        initPOSFeatures(s)
        model.maximize(s.tokens.map(_.attr[A]))(null)
      }
    })
    document
  }

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence])
  def postAttrs = Seq(classOf[PennPosTag])
  def tokenAnnotationString(token: Token) = { val label = token.attr[A with LabeledVar]; if (label ne null) label.categoryValue else "(null)" }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(PosFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()
  }
  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(PosFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(model, dstream)
    dstream.close()
  }

  def train(trainSentences:Seq[Sentence], testSentences:Seq[Sentence], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    // TODO Accomplish this TokenNormalization instead by calling POS3.preProcess
    trainSentences.foreach(initPOSFeatures)
    PosFeaturesDomain.freeze()
    testSentences.foreach(initPOSFeatures)
    def evaluate() {
      (trainSentences ++ testSentences).foreach(s => model.maximize(s.tokens.map(_.attr[A with LabeledVar]))(null))
      println("Train accuracy: "+ HammingObjective.accuracy(trainSentences.flatMap(s => s.tokens.map(_.attr[A with LabeledVar]))))
      println("Test accuracy: "+ HammingObjective.accuracy(testSentences.flatMap(s => s.tokens.map(_.attr[A with LabeledVar]))))
    }
    val examples =
    if(useHingeLoss)
      trainSentences.map(sentence => new model.ChainStructuredSVMExample(sentence.tokens.map(_.attr[A with LabeledMutableDiscreteVar]))).toSeq
    else
      trainSentences.map(sentence => new model.ChainLikelihoodExample(sentence.tokens.map(_.attr[A with LabeledMutableDiscreteVar])))
    //val optimizer = new cc.factorie.optimize.AdaGrad(rate=lrate)
    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = false)
  }


  object PosFeaturesDomain extends CategoricalVectorDomain[String]
  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain; override def skipNonCategories = true }

  val posDomain = tagConstructor(null).domain
  val model = new ChainModel[A, PosFeatures, Token](posDomain,
    PosFeaturesDomain,
    l => l.token.attr[PosFeatures],
    l => l.token,
    t => t.attr[A]){
    useObsMarkov = false
  }


  def initPOSFeatures(sentence: Sentence): Unit = {
    import cc.factorie.app.strings.simplifyDigits
    for (token <- sentence.tokens) {
      if(token.attr[PosFeatures] ne null)
        token.attr.remove[PosFeatures]

      val features = token.attr += new PosFeatures(token)
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
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
    addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[PosFeatures], "W=[^@]*$", List(-2), List(-1), List(1), List(-2,-1), List(-1,0))
  }
}

class WSJChainPosTagger(url:java.net.URL) extends ChainPosTagger((t:Token) => new PennPosTag(t, 0), url)
object WSJChainPosTagger extends WSJChainPosTagger(ClasspathURL[WSJChainPosTagger](".factorie"))

class OntonotesChainPosTagger(url:java.net.URL) extends ChainPosTagger((t:Token) => new PennPosTag(t, 0), url)
object OntonotesChainPosTagger extends OntonotesChainPosTagger(ClasspathURL[OntonotesChainPosTagger](".factorie"))

class CtbChainPosTagger(url:java.net.URL) extends ChainPosTagger((t:Token) => new CtbPosTag(t, 0), url) {

  override def initPOSFeatures(sentence: Sentence): Unit = {
    import cc.factorie.app.chineseStrings._

    for (token <- sentence.tokens) {
      if(token.attr[PosFeatures] ne null)
        token.attr.remove[PosFeatures]

      val features = token.attr += new PosFeatures(token)
      val rawWord = token.string

      features += "W="+word
      features += "SUFFIX=" + word.takeRight(1)
      features += "PREFIX=" + word.take(1)

      if (hasPunctuation(rawWord)) features += "PUNCTUATION"
      if (hasNumeric(rawWord)) features += "NUMERIC"
      if (hasChineseNumeric(rawWord)) features += "CHINESE_NUMERIC"
      if (hasAlpha(rawWord)) features += "ALPHA"
    }
    addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[PosFeatures], "W=[^@]*$", List(-2), List(-1), List(1), List(-2,-1), List(-1,0))
  }
}
object CtbChainPosTagger extends CtbChainPosTagger(ClasspathURL[CtbChainPosTagger](".factorie"))

class ChainPosTrainer[A<:PosTag](tagConstructor:(Token)=>A)(implicit ct:ClassTag[A]) extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new ForwardPosOptions
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)
    // Expects three command-line arguments: a train file, a test file, and a place to save the model in
    // the train and test files are supposed to be in OWPL format
    val pos = new ChainPosTagger(tagConstructor)

    val trainDocs = load.LoadOntonotes5.fromFilename(opts.trainFile.value)
    val testDocs =  load.LoadOntonotes5.fromFilename(opts.testFile.value)

    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainSentencesFull = trainDocs.flatMap(_.sentences)
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    val testSentencesFull = testDocs.flatMap(_.sentences)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)


    pos.train(trainSentences, testSentences,
              opts.rate.value, opts.delta.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, l1Factor=opts.l1.value, l2Factor=opts.l2.value)
    if (opts.saveModel.value) {
      pos.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      val pos2 = new ChainPosTagger(tagConstructor)
      pos2.deserialize(new FileInputStream(new java.io.File(opts.modelFile.value)))
    }
    val acc = HammingObjective.accuracy(testDocs.flatMap(d => d.sentences.flatMap(s => s.tokens.map(_.attr[LabeledPennPosTag]))))
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(acc,opts.targetAccuracy.value.toDouble)

    acc
  }
}
object ChainPosTrainer extends ChainPosTrainer((t:Token) => new PennPosTag(t, 0))

class CtbChainPosTrainer[A<:PosTag](tagConstructor:(Token)=>A)(implicit ct:ClassTag[A]) extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    val opts = new ForwardPosOptions
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)

    val pos = new ChainPosTagger(tagConstructor)

    val trainDocs = getDocs(opts.trainFile.value)
    val testDocs = getDocs(opts.testFile.value)

    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainSentencesFull = trainDocs.flatMap(_.sentences)
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    val testSentencesFull = testDocs.flatMap(_.sentences)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)


    pos.train(trainSentences, testSentences,
      opts.rate.value, opts.delta.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, l1Factor=opts.l1.value, l2Factor=opts.l2.value)
    if (opts.saveModel.value) {
      pos.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      val pos2 = new ChainPosTagger(tagConstructor)
      pos2.deserialize(new FileInputStream(new java.io.File(opts.modelFile.value)))
    }
    val acc = HammingObjective.accuracy(testDocs.flatMap(d => d.sentences.flatMap(s => s.tokens.map(_.attr[LabeledPennPosTag]))))
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(acc,opts.targetAccuracy.value.toDouble)

    acc
  }

  def getDocs(dirName: String): Seq[Document] = {
    val directory = new File(dirName)

    (for{
       fileName <- directory.listFiles
       file = new File(fileName)
       if file.isFile
       document = new Document
       line <- scala.io.Source.fromFile(file, "utf-8").getLines
       if line(0) != '<'
       sentence = new Sentence(document)
       wordLabelPair <- line.split(' ').map(_.split('_'))
       token = new Token(sentence, wordLabelPair(0))
       labeledTag = token.attr += new LabeledCtbPosTag(token, wordLabelPair(1))
     } yield document
    ).toSeq
  }
}

object ChainPosOptimizer {
  def main(args: Array[String]) {
    val opts = new ForwardPosOptions
    opts.parse(args)
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq(List(0,1,2,3)))
    val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.pos.ChainPosTrainer")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, l2, rate, delta, cutoff), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    opts.saveModel.setValue(true)
    println("Running best configuration...")
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 5.hours)
    println("Done")
  }
}

