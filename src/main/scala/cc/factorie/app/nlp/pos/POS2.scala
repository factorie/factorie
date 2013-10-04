package cc.factorie.app.nlp.pos

import cc.factorie.app.nlp._
import cc.factorie.app.chain.ChainModel
import cc.factorie.app.chain.Observations._
import java.io._
import cc.factorie.util.{HyperparameterMain, ClasspathURL, BinarySerializer}
import cc.factorie.optimize.Trainer
import cc.factorie.variable.{HammingObjective, BinaryFeatureVectorVariable, CategoricalVectorDomain}

/**
 * User: apassos
 * Date: 7/15/13
 * Time: 2:55 PM
 */
class POS2 extends DocumentAnnotator {
  def process(document: Document) = {
    document.sentences.foreach(s => {
      if (s.nonEmpty) {
        s.tokens.foreach(t => if (!t.attr.contains[PennPosLabel]) t.attr += new PennPosLabel(t, "NN"))
        initPOSFeatures(s)
        model.maximize(s.tokens.map(_.posLabel))(null)
      }
    })
    document
  }

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence])
  def postAttrs = Seq(classOf[PennPosLabel])
  def tokenAnnotationString(token: Token) = { val label = token.attr[PennPosLabel]; if (label ne null) label.categoryValue else "(null)" }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(stream)
    BinarySerializer.serialize(PosFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()
  }
  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(stream)
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
      (trainSentences ++ testSentences).foreach(s => model.maximize(s.tokens.map(_.posLabel))(null))
      println("Train accuracy: "+ HammingObjective.accuracy(trainSentences.flatMap(s => s.tokens.map(_.posLabel))))
      println("Test accuracy: "+ HammingObjective.accuracy(testSentences.flatMap(s => s.tokens.map(_.posLabel))))
    }
    val examples = trainSentences.map(sentence => new model.ChainStructuredSVMExample(sentence.tokens.map(_.posLabel))).toSeq
    //val optimizer = new cc.factorie.optimize.AdaGrad(rate=lrate)
    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = false)
  }


  object PosFeaturesDomain extends CategoricalVectorDomain[String]
  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain; override def skipNonCategories = true }


  val model = new ChainModel[PennPosLabel, PosFeatures, Token](PennPosDomain,
    PosFeaturesDomain,
    l => l.token.attr[PosFeatures],
    l => l.token,
    t => t.attr[PennPosLabel]){
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

object POS2 extends POS2 {
  deserialize(ClasspathURL[POS2](".factorie").openConnection().getInputStream)
}


object POS2Trainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new POS1Opts
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)
    // Expects three command-line arguments: a train file, a test file, and a place to save the model in
    // the train and test files are supposed to be in OWPL format
    val pos = new POS2
    val trainDocs = cc.factorie.app.nlp.load.LoadOntonotes5.fromFilename(opts.trainFile.value)
    val testDocs = cc.factorie.app.nlp.load.LoadOntonotes5.fromFilename(opts.testFile.value)
    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))
    pos.train(trainDocs.flatMap(_.sentences), testDocs.flatMap(_.sentences),
              opts.rate.value, opts.delta.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, l1Factor=opts.l1.value, l2Factor=opts.l2.value)
    if (opts.saveModel.value) {
      pos.serialize(new FileOutputStream(new File(opts.modelFile.value)))
      val pos2 = new POS2
      pos2.deserialize(new FileInputStream(new java.io.File(opts.modelFile.value)))
    }
    HammingObjective.accuracy(testDocs.flatMap(d => d.sentences.flatMap(s => s.tokens.map(_.posLabel))))
  }
}

object POS2Optimizer {
  def main(args: Array[String]) {
    val opts = new POS1Opts
    opts.parse(args)
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq(List(0,1,2,3)))
    val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.pos.POS2Trainer")
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

