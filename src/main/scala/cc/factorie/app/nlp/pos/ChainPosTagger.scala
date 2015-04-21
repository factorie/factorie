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
import java.util.{HashMap, HashSet}
import cc.factorie.util.{HyperparameterMain, ClasspathURL, BinarySerializer}
import cc.factorie.optimize.Trainer
import cc.factorie.variable.{HammingObjective, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalVariable, LabeledVar, LabeledMutableDiscreteVar}
import scala.reflect.ClassTag

/** A linear-chain CRF part-of-speech tagger, doing inference by Viterbi.
    @author Alexandre Passos, Andrew McCallum
 */
abstract class ChainPosTagger[A<:PosTag](val tagConstructor:(Token)=>A)(implicit ct:ClassTag[A]) extends DocumentAnnotator {
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
  def postAttrs = Seq(ct.runtimeClass)
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

  def train(trainSentences:Seq[Sentence],
            testSentences:Seq[Sentence],
            lrate:Double = 0.1,
            decay:Double = 0.01,
            cutoff:Int = 2,
            doBootstrap:Boolean = true,
            useHingeLoss:Boolean = false,
            numIterations: Int = 5,
            l1Factor:Double = 0.000001,
            l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    // TODO Accomplish this TokenNormalization instead by calling POS3.preProcess
    println("Initializing POS features for training sentences")
    trainSentences.foreach(initPOSFeatures)
    println("Finished initializing POS features for training sentences")
    PosFeaturesDomain.freeze()
    println("Initializing POS features for testing sentences")
    testSentences.foreach(initPOSFeatures)
    println("Finished initializing POS features for testing sentences")
    def evaluate() {
      println("Evaluating")
      (trainSentences ++ testSentences).foreach(s => model.maximize(s.tokens.map(_.attr[A with LabeledVar]))(null) )
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
    println("Running Parameter Optimization")
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

  def initPOSFeatures(sentence: Sentence): Unit
}

class WSJChainPosTagger extends ChainPosTagger((t:Token) => new PennPosTag(t, 0)) {
  def this(url: java.net.URL) = {
    this()  
    deserialize(url.openConnection().getInputStream)
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
object WSJChainPosTagger extends WSJChainPosTagger(ClasspathURL[WSJChainPosTagger](".factorie"))

class OntonotesChainPosTagger extends ChainPosTagger((t:Token) => new PennPosTag(t, 0)) {
  def this(url: java.net.URL) = {
    this()  
    deserialize(url.openConnection().getInputStream)
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
object OntonotesChainPosTagger extends OntonotesChainPosTagger(ClasspathURL[OntonotesChainPosTagger](".factorie"))


class ChainPosTrainer[A<:PosTag, B<:ChainPosTagger[A]](taggerConstructor: () => B, loadingMethod:(String) => Seq[Document])(implicit ct:ClassTag[A]) extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new ForwardPosOptions
    opts.parse(args)
    assert(opts.trainDir.wasInvoked)
    // Expects three command-line arguments: a train file, a test file, and a place to save the model in
    // the train and test files are supposed to be in OWPL format
    val pos = taggerConstructor()

    val trainDocs = loadingMethod(opts.trainDir.value)
    println("NUM TRAIN DOCS:" + trainDocs.size)
    val testDocs =  loadingMethod(opts.testDir.value)
    println("NUM TEST DOCS:" + testDocs.size)

    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    println("Flatmapping Training Sentences")
    val trainSentencesFull = trainDocs.flatMap(_.sentences)
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    println("Finished Flatmapping Training Sentences")
    println("Flatmapping Testing Sentences")
    val testSentencesFull = testDocs.flatMap(_.sentences)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)
    println("Finished Flatmapping Testing Sentences")

    println("Training")
    pos.train(trainSentences,
              testSentences,
              opts.rate.value,
              opts.delta.value,
              opts.cutoff.value,
              opts.updateExamples.value,
              opts.useHingeLoss.value,
              l1Factor=opts.l1.value,
              l2Factor=opts.l2.value)
    println("Finished Training")
    if (opts.saveModel.value) {
      println("Serializing Model")
      pos.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      println("Finished Serializing Model")
      val pos2 = taggerConstructor()
      println("Deserializing Model")
      pos2.deserialize(new FileInputStream(new java.io.File(opts.modelFile.value)))
      println("Finished Deserializing Model")
    }
    val acc = HammingObjective.accuracy(testDocs.flatMap(d => d.sentences.flatMap(s => s.tokens.map(_.attr[A with LabeledVar]))))
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(acc,opts.targetAccuracy.value.toDouble)

    acc
  }
}
object OntonotesChainPosTrainer extends ChainPosTrainer[PennPosTag, OntonotesChainPosTagger](
  () => new OntonotesChainPosTagger(),
  (dirName: String) => load.LoadOntonotes5.fromFilename(dirName)
)

object ChainPosOptimizer {
  def main(args: Array[String]) {
    val opts = new ForwardPosOptions
    val trainerName = args(0)
    opts.parse(args.slice(1, opts.size))
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq(List(0,1,2,3)))
    val qs = new cc.factorie.util.QSubExecutor(60, trainerName)
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


class SpanishChainPosTagger extends ChainPosTagger((t:Token) => new SpanishPosTag(t, 0)) {
  def this(url: java.net.URL) = {
    this()
    deserialize(url.openConnection().getInputStream)
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
object SpanishChainPosTagger extends SpanishChainPosTagger(ClasspathURL[SpanishChainPosTagger](".factorie"))
object SpanishChainPosTrainer extends ChainPosTrainer[SpanishPosTag, SpanishChainPosTagger](
  () => new SpanishChainPosTagger(),
  (dirName: String) => load.LoadSpanishConll2008.fromFilename(dirName)
)
