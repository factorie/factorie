package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp._

import java.io._
import cc.factorie.util.{HyperparameterMain, ClasspathURL, BinarySerializer}
import cc.factorie.variable._
import cc.factorie.optimize.Trainer
import cc.factorie.app.chain.ChainModel
import cc.factorie.app.chain.Observations._
import scala.io.Source
import cc.factorie.app.nlp.load.{BILOUChunkDomain, BILOUChunkTag, LoadConll2000}
import scala._
import cc.factorie.app.nlp.pos.PennPosTag

/**
 * User: cellier
 * Date: 10/7/13
 * Time: 2:49 PM
 */

class CRFChunker extends DocumentAnnotator {
  def process(document: Document) = {
    document.sentences.foreach(s => {
      if (s.nonEmpty) {
        s.tokens.foreach(t => if (!t.attr.contains[BILOUChunkTag]) t.attr += new BILOUChunkTag(t, "O"))
        features(s)
        model.maximize(s.tokens.map(_.attr[BILOUChunkTag]))(null)
      }
    })
    document
  }


  def prereqAttrs = Seq(classOf[Token], classOf[Sentence])
  def postAttrs = Seq(classOf[BILOUChunkTag])
  def tokenAnnotationString(token: Token) = { val label = token.attr[BILOUChunkTag]; if (label ne null) label.categoryValue else "(null)" }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(stream)
    BinarySerializer.serialize(ChunkFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()
  }
  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(stream)
    BinarySerializer.deserialize(ChunkFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(model, dstream)
    dstream.close()
  }

  def train(trainSentences:Seq[Sentence], testSentences:Seq[Sentence], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    LoadConll2000.convertBIOtoBILOU(trainSentences)
    LoadConll2000.convertBIOtoBILOU(testSentences)
    trainSentences.foreach(features)
    ChunkFeaturesDomain.freeze()
    println(ChunkFeaturesDomain.dimensionSize)
    testSentences.foreach(features)
    def evaluate() {
      (trainSentences ++ testSentences).foreach(s => model.maximize(s.tokens.map(_.attr[BILOUChunkTag]))(null))
      val buf = new StringBuffer
      buf.append(new LabeledDiscreteEvaluation(testSentences.flatMap(_.tokens.map(_.attr[BILOUChunkTag]))))
      val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[BILOUChunkTag](BILOUChunkDomain.categories.filter(_.length > 2).map(_.substring(2)))
      for (sentence <- testSentences) segmentEvaluation += sentence.tokens.map(_.attr[BILOUChunkTag])
      println(segmentEvaluation)
      println("Train accuracy: "+ HammingObjective.accuracy(trainSentences.flatMap(s => s.tokens.map(_.attr[BILOUChunkTag]))))
      println("Test accuracy: "+ HammingObjective.accuracy(testSentences.flatMap(s => s.tokens.map(_.attr[BILOUChunkTag]))))
    }
    val examples = trainSentences.map(sentence => new model.ChainStructuredSVMExample(sentence.tokens.map(_.attr[BILOUChunkTag]))).toSeq
    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = false)
  }

  object ChunkFeaturesDomain extends CategoricalVectorDomain[String]

  class ChunkFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = ChunkFeaturesDomain; override def skipNonCategories = true }

  val model = new ChainModel[BILOUChunkTag, ChunkFeatures, Token](BILOUChunkDomain,
    ChunkFeaturesDomain,
    l => l.token.attr[ChunkFeatures],
    l => l.token,
    t => t.attr[BILOUChunkTag]){
    useObsMarkov = false
  }

  def features(sentence: Sentence): Unit = {
    import cc.factorie.app.strings.simplifyDigits
    val tokens = sentence.tokens.zipWithIndex
    for ((token,i) <- tokens) {
      if(token.attr[ChunkFeatures] ne null)
        token.attr.remove[ChunkFeatures]
      val features = token.attr += new ChunkFeatures(token)

      val rawWord = token.string
      val posTag = token.attr[PennPosTag]
      val word = simplifyDigits(rawWord).toLowerCase
      features += "SENTLOC="+i
      features += "P="+posTag
      features += "W="+word
      features += "STEM=" + cc.factorie.app.strings.porterStem(word)
      features += "WSIZE=" + rawWord.length
      val j = 3
      features += "SUFFIX" + j + "=" + word.takeRight(j)
      features += "PREFIX" + j + "=" + word.take(j)
      features += "BIAS"
    }
    addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[ChunkFeatures], "W=[^@]*$", List(-2), List(-1), List(1),List(2), List(-1,0), List(0,1))
    addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[ChunkFeatures], "P=[^@]*$", List(-2), List(-1), List(1), List(2), List(-2,-1), List(-1,0), List(0,1), List(1,2),List(-2,-1,0),List(-1,0,1),List(0,1,2))
  }
}

object CRFChunker extends CRFChunker {
  deserialize(ClasspathURL[CRFChunker](".factorie").openConnection().getInputStream)
}


object CRFChunkingTrainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new ChunkerOpts
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)

    val chunk = new CRFChunker

    val trainDocs = LoadConll2000.fromSource(Source.fromFile(opts.trainFile.value))
    val testDocs =  LoadConll2000.fromSource(Source.fromFile(opts.testFile.value))

    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainSentencesFull = trainDocs.flatMap(_.sentences).filter(!_.isEmpty)
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    val testSentencesFull = testDocs.flatMap(_.sentences).filter(!_.isEmpty)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)


    chunk.train(trainSentences, testSentences,
      opts.rate.value, opts.delta.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, l1Factor=opts.l1.value, l2Factor=opts.l2.value)
    if (opts.saveModel.value) {
      chunk.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      val chunk2 = new CRFChunker
      chunk2.deserialize(new FileInputStream(new java.io.File(opts.modelFile.value)))
    }
    val acc = HammingObjective.accuracy(testDocs.flatMap(d => d.sentences.flatMap(s => s.tokens.map(_.attr[BILOUChunkTag]))))
    if(opts.targetAccuracy.wasInvoked) assert(acc > opts.targetAccuracy.value.toDouble, "Did not reach accuracy requirement")
    acc
  }
}


object CRFChunkOptimizer {
  def main(args: Array[String]) {
    val opts = new ChunkerOpts
    opts.parse(args)
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq(List(0,1,2,3)))
    val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.chunk.CRFChunkingTrainer")
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

class ChunkerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions{
  val conllPath = new CmdOption("rcv1Path", "../../data/conll2000", "DIR", "Path to folder containing RCV1-v2 dataset.")
  val outputPath = new CmdOption("ouputPath", "../../data/conll2000/output.txt", "FILE", "Path to write output for evaluation.")
  val modelFile = new CmdOption("model", "", "FILENAME", "Filename for the model (saving a trained model or reading a running model.")
  val testFile = new CmdOption("test", "src/main/resources/test.txt", "FILENAME", "OWPL test file.")
  val trainFile = new CmdOption("train", "src/main/resources/train.txt", "FILENAME", "OWPL training file.")
  val l1 = new CmdOption("l1", 0.000001,"FLOAT","l1 regularization weight")
  val l2 = new CmdOption("l2", 0.00001,"FLOAT","l2 regularization weight")
  val rate = new CmdOption("rate", 10.0,"FLOAT","base learning rate")
  val delta = new CmdOption("delta", 100.0,"FLOAT","learning rate decay")
  val cutoff = new CmdOption("cutoff", 2, "INT", "Discard features less frequent than this before training.")
  val updateExamples = new  CmdOption("update-examples", true, "BOOL", "Whether to update examples in later iterations during training.")
  val useHingeLoss = new CmdOption("use-hinge-loss", false, "BOOL", "Whether to use hinge loss (or log loss) during training.")
  val saveModel = new CmdOption("save-model", false, "BOOL", "Whether to save the trained model.")
  val runText = new CmdOption("run", "", "FILENAME", "Plain text file on which to run.")
  val numIters = new CmdOption("num-iterations","5","INT","number of passes over the data for training")
}