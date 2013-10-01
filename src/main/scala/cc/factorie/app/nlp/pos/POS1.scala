package cc.factorie.app.nlp.pos
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.la._
import cc.factorie.util.{BinarySerializer, CubbieConversions, DoubleAccumulator}
import java.io.{File,InputStream,FileInputStream}
import cc.factorie.util.HyperparameterMain
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain}
import cc.factorie.app.classify.LinearMultiClassClassifier
import cc.factorie.optimize.Trainer

class POS1 extends DocumentAnnotator {
  // Different ways to load saved parameters
  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = this(url.openConnection.getInputStream)
  
  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(t:Tensor1) extends BinaryFeatureVectorVariable[String] { def domain = FeatureDomain; set(t)(null) } // Only used for printing diagnostics
  lazy val model = new LinearMultiClassClassifier(PennPosDomain.size, FeatureDomain.dimensionSize)
  
  /** Local lemmatizer used for POS features. */
  protected def lemmatize(string:String): String = cc.factorie.app.strings.replaceDigits(string)
  /** A special IndexedSeq[String] that will return "null" for indices out of bounds, rather than throwing an error */
  class Lemmas(tokens:Seq[Token]) extends IndexedSeq[String] {
    val inner: IndexedSeq[String] = tokens.toIndexedSeq.map((t:Token) => cc.factorie.app.strings.replaceDigits(t.string))
    val innerlc = inner.map(_.toLowerCase)
    val length: Int = inner.length
    def apply(i:Int): String = if (i < 0 || i > length-1) null else inner(i)
    def lc(i:Int): String = if (i < 0 || i > length-1) null else innerlc(i)
  }
  protected def lemmas(tokens:Seq[Token]) = new Lemmas(tokens)
  
  /** Infrastructure for building and remembering a list of training data words that nearly always have the same POS tag.
      Used as cheap "stacked learning" features when looking-ahead to words not yet predicted by this POS tagger.
      The key into the ambiguityClasses is app.strings.replaceDigits().toLowerCase */
  object WordData {
    val ambiguityClasses = collection.mutable.HashMap[String,String]()
    val ambiguityClassThreshold = 0.7
    val wordInclusionThreshold = 1

    def preProcess(tokens: Iterable[Token]) {
      val wordCounts = collection.mutable.HashMap[String,Int]()
      val posCounts = collection.mutable.HashMap[String,Array[Int]]()
      var tokenCount = 0
      tokens.foreach(t => {
        tokenCount += 1
        if (t.attr[PennPosLabel] eq null) {
          println("POS1.WordData.preProcess tokenCount "+tokenCount)
          println("POS1.WordData.preProcess token "+t.prev.string+" "+t.prev.attr)
          println("POS1.WordData.preProcess token "+t.string+" "+t.attr)
          throw new Error("Found training token with no PennPosLabel.")
        }
        val lemma = lemmatize(t.string).toLowerCase
        if (!wordCounts.contains(lemma)) {
          wordCounts(lemma) = 0
          posCounts(lemma) = Array.fill(PennPosDomain.size)(0)
        }
        wordCounts(lemma) += 1
        posCounts(lemma)(t.attr[PennPosLabel].intValue) += 1
      })
      wordCounts.keys.foreach(w => {
        if (wordCounts(w) >= wordInclusionThreshold) {
          val counts = wordCounts(w)
          val pos = posCounts(w)
          val bestPos = (0 until 45).maxBy(i => pos(i))
          if (pos(bestPos) > ambiguityClassThreshold*counts)
            ambiguityClasses(w) = PennPosDomain.category(bestPos)
        }
      })
    }
  }
  
  def features(token:Token, lemmaIndex:Int, lemmas:Lemmas): SparseBinaryTensor1 = {
    def lemmaStringAtOffset(offset:Int): String = "L@"+offset+"="+lemmas.lc(lemmaIndex + offset) // this is lowercased
    def wordStringAtOffset(offset:Int): String = "W@"+offset+"="+lemmas(lemmaIndex + offset) // this is not lowercased, but still has digits replaced
    def affinityTagAtOffset(offset:Int): String = "A@"+offset+"="+WordData.ambiguityClasses.getOrElse(lemmas.lc(lemmaIndex + offset), null)
    def posTagAtOffset(offset:Int): String = { val t = token.next(offset); "P@"+offset+(if (t ne null) t.attr[PennPosLabel].categoryValue else null) }
    def take(s:String, n:Int): String = { val l = s.length; if (l < n) s else s.substring(0,n) }
    def takeRight(s:String, n:Int): String = { val l = s.length; if (l < n) s else s.substring(l-n,l) }
    val tensor = new SparseBinaryTensor1(FeatureDomain.dimensionSize); tensor.sizeHint(40)
    def addFeature(s:String): Unit = if (s ne null) { val i = FeatureDomain.dimensionDomain.index(s); if (i >= 0) tensor += i }
    // Original word, with digits replaced, no @
    val Wm3 = if (lemmaIndex > 2) lemmas(lemmaIndex-3) else ""
    val Wm2 = if (lemmaIndex > 1) lemmas(lemmaIndex-2) else ""
    val Wm1 = if (lemmaIndex > 0) lemmas(lemmaIndex-1) else ""
    val W = lemmas(lemmaIndex)
    val Wp1 = if (lemmaIndex < lemmas.length-1) lemmas(lemmaIndex+1) else ""
    val Wp2 = if (lemmaIndex < lemmas.length-2) lemmas(lemmaIndex+2) else ""
    val Wp3 = if (lemmaIndex < lemmas.length-3) lemmas(lemmaIndex+3) else ""
    // Original words at offsets, with digits replaced, marked with @
    val wm2 = wordStringAtOffset(-2)
    val wm1 = wordStringAtOffset(-1)
    val w0 = wordStringAtOffset(0)
    val wp1 = wordStringAtOffset(1)
    val wp2 = wordStringAtOffset(2)
    // Lemmas at offsets
    val lm3 = lemmaStringAtOffset(-3)
    val lm2 = lemmaStringAtOffset(-2)
    val lm1 = lemmaStringAtOffset(-1)
    val l0 = lemmaStringAtOffset(0)
    val lp1 = lemmaStringAtOffset(1)
    val lp2 = lemmaStringAtOffset(2)
    val lp3 = lemmaStringAtOffset(3)
    // Affinity classes at next offsets
    val a0 = affinityTagAtOffset(0)
    val ap1 = affinityTagAtOffset(1)
    val ap2 = affinityTagAtOffset(2)
    val ap3 = affinityTagAtOffset(3)
    // POS tags at prev offsets
    val pm1 = posTagAtOffset(-1)
    val pm2 = posTagAtOffset(-2)
    val pm3 = posTagAtOffset(-3)
    addFeature(wm2)
    addFeature(wm1)
    addFeature(w0)
    addFeature(wp1)
    addFeature(wp2)
    // The paper also includes wp3 and wn3
    addFeature(lp3)
    addFeature(lp2)
    addFeature(lp1)
    addFeature(l0)
    addFeature(lm1)
    addFeature(lm2)
    addFeature(lm3)
    addFeature(pm3)
    addFeature(pm2)
    addFeature(pm1)
    addFeature(a0)
    addFeature(ap1)
    addFeature(ap2)
    addFeature(ap3)
    addFeature(lm2+lm1)
    addFeature(lm1+l0)
    addFeature(l0+lp1)
    addFeature(lp1+lp2)
    addFeature(lm1+lp1)
    addFeature(pm2+pm1)
    addFeature(ap1+ap2)
    addFeature(pm1+ap1)
    addFeature(pm1+a0) // Not in http://www.aclweb.org/anthology-new/P/P12/P12-2071.pdf
    addFeature(a0+ap1) // Not in http://www.aclweb.org/anthology-new/P/P12/P12-2071.pdf
    addFeature(lm2+lm1+l0)
    addFeature(lm1+l0+lp1)
    addFeature(l0+lp1+lp2)
    addFeature(lm2+lm1+lp1)
    addFeature(lm1+lp1+lp2)
    addFeature(pm2+pm1+a0)
    addFeature(pm1+a0+ap1)
    addFeature(pm2+pm1+ap1)
    addFeature(pm1+ap1+ap2)
    addFeature(a0+ap1+ap2) // Not in http://www.aclweb.org/anthology-new/P/P12/P12-2071.pdf
    addFeature("PREFIX2="+take(W, 2))
    addFeature("PREFIX3="+take(W, 3))
    addFeature("PREFIX2@1="+take(Wp1, 2))
    addFeature("PREFIX3@1="+take(Wp1, 3))
    addFeature("PREFIX2@2="+take(Wp2, 2))
    addFeature("PREFIX3@2="+take(Wp2, 3))
    addFeature("SUFFIX1="+takeRight(W, 1))
    addFeature("SUFFIX2="+takeRight(W, 2))
    addFeature("SUFFIX3="+takeRight(W, 3))
    addFeature("SUFFIX4="+takeRight(W, 4))
    addFeature("SUFFIX1@1="+takeRight(Wp1, 1))
    addFeature("SUFFIX2@1="+takeRight(Wp1, 2))
    addFeature("SUFFIX3@1="+takeRight(Wp1, 3))
    addFeature("SUFFIX4@1="+takeRight(Wp1, 4))
    addFeature("SUFFIX2@2="+takeRight(Wp2, 2))
    addFeature("SUFFIX3@2="+takeRight(Wp2, 3))
    addFeature("SUFFIX4@2="+takeRight(Wp2, 4))
    addFeature("SHAPE@-2="+cc.factorie.app.strings.stringShape(Wm2, 2))
    addFeature("SHAPE@-1="+cc.factorie.app.strings.stringShape(Wm1, 2))
    addFeature("SHAPE@0="+cc.factorie.app.strings.stringShape(W, 2))
    addFeature("SHAPE@1="+cc.factorie.app.strings.stringShape(Wp1, 2))
    addFeature("SHAPE@2="+cc.factorie.app.strings.stringShape(Wp2, 2))
    // TODO(apassos): add the remaining jinho features not contained in shape
    addFeature("HasPeriod="+(w0.indexOf('.') >= 0))
    addFeature("HasHyphen="+(w0.indexOf('-') >= 0))
    addFeature("HasDigit="+(l0.indexOf('0', 4) >= 0)) // The 4 is to skip over "W@0="
    //addFeature("MiddleHalfCap="+token.string.matches(".+1/2[A-Z].*")) // Paper says "contains 1/2+capital(s) not at the beginning".  Strange feature.  Why? -akm
    tensor
  }
  def features(tokens:Seq[Token]): Seq[SparseBinaryTensor1] = {
    val lemmaStrings = lemmas(tokens)
    tokens.zipWithIndex.map({case (t:Token, i:Int) => features(t, i, lemmaStrings)})
  }

  var exampleSetsToPrediction = false
  class SentenceClassifierExample(val tokens:Seq[Token], model:LinearMultiClassClassifier, lossAndGradient: optimize.LinearObjectives.MultiClass) extends optimize.Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
      val lemmaStrings = lemmas(tokens)
      for (index <- 0 until tokens.length) {
        val token = tokens(index)
        val posLabel = token.attr[PennPosLabel]
        val featureVector = features(token, index, lemmaStrings)
        new optimize.LinearMultiClassExample(model.weights, featureVector, posLabel.targetIntValue, lossAndGradient, 1.0).accumulateValueAndGradient(value, gradient)
  //      new optimize.LinearMultiClassExample(featureVector, posLabel.targetIntValue, lossAndGradient).accumulateValueAndGradient(model, gradient, value)
        if (exampleSetsToPrediction) {
          posLabel.set(model.classification(featureVector).bestLabelIndex)(null)
        }
      }
    }
  }
  
  def predict(tokens: Seq[Token]): Unit = {
    val lemmaStrings = lemmas(tokens)
    for (index <- 0 until tokens.length) {
      val token = tokens(index)
      val posLabel = token.attr[PennPosLabel]
      val featureVector = features(token, index, lemmaStrings)
      if (token.attr[PennPosLabel] eq null) token.attr += new PennPosLabel(token, "NNP")
      token.attr[PennPosLabel].set(model.classification(featureVector).bestLabelIndex)(null)
    }
  }
  def predict(span: TokenSpan): Unit = predict(span.tokens)
  def predict(document: Document): Unit = {
    for (section <- document.sections)
      if (section.hasSentences) document.sentences.foreach(predict(_))  // we have Sentence boundaries 
      else predict(section.tokens) // we don't // TODO But if we have trained with Sentence boundaries, won't this hurt accuracy?
  }

  // Serialization
  def serialize(filename: String): Unit = {
    val file = new File(filename); if (file.getParentFile ne null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import CubbieConversions._
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(model.weights.value.dim1, model.weights.value.dim2, new la.SparseIndexedTensor1(_))
    model.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(stream)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    BinarySerializer.serialize(WordData.ambiguityClasses, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import CubbieConversions._
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, dstream)
    model.weights.set(new la.DenseLayeredTensor2(PennPosDomain.size, FeatureDomain.dimensionDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    BinarySerializer.deserialize(WordData.ambiguityClasses, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  
  def accuracy(sentences:Iterable[Sentence]): Double = {
    var total = 0.0
    var correct = 0.0
    var totalTime = 0L
    sentences.foreach(s => {
      val t0 = System.currentTimeMillis()
      predict(s)
      totalTime += (System.currentTimeMillis()-t0)
      for (token <- s.tokens) {
        total += 1
        if (token.attr[PennPosLabel].valueIsTarget) correct += 1.0
      }
    })
    correct/total
  }
  
  def train(trainSentences:Seq[Sentence], testSentences:Seq[Sentence], lrate:Double = 0.1, decay:Double = 0.01, cutoff:Int = 2, doBootstrap:Boolean = true, useHingeLoss:Boolean = false, numIterations: Int = 5, l1Factor:Double = 0.000001, l2Factor:Double = 0.000001)(implicit random: scala.util.Random) {
    // TODO Accomplish this TokenNormalization instead by calling POS3.preProcess
    for (sentence <- trainSentences ++ testSentences; token <- sentence.tokens) cc.factorie.app.nlp.segment.PlainTokenNormalizer.processToken(token)
    WordData.preProcess(trainSentences.flatMap(_.tokens))
    // Prune features by count
    FeatureDomain.dimensionDomain.gatherCounts = true
    for (sentence <- trainSentences) features(sentence.tokens) // just to create and count all features
    FeatureDomain.dimensionDomain.trimBelowCount(cutoff)
    FeatureDomain.freeze()
    println("After pruning using %d features.".format(FeatureDomain.dimensionDomain.size))
    println("POS1.train\n"+trainSentences(3).tokens.map(_.string).zip(features(trainSentences(3).tokens).map(t => new FeatureVariable(t).toString)).mkString("\n"))
    def evaluate() {
      exampleSetsToPrediction = doBootstrap
      println("Train accuracy: "+accuracy(trainSentences))
      println("Test  accuracy: "+accuracy(testSentences))
      println(s"Sparsity: ${model.weights.value.toSeq.count(_ == 0).toFloat/model.weights.value.length}")
    }
    val examples = trainSentences.shuffle.par.map(sentence =>
      new SentenceClassifierExample(sentence.tokens, model, if (useHingeLoss) cc.factorie.optimize.LinearObjectives.hingeMultiClass else cc.factorie.optimize.LinearObjectives.sparseLogMultiClass)).seq
    //val optimizer = new cc.factorie.optimize.AdaGrad(rate=lrate)
    val optimizer = new cc.factorie.optimize.AdaGradRDA(rate=lrate, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
    Trainer.onlineTrain(model.parameters, examples, maxIterations=numIterations, optimizer=optimizer, evaluate=evaluate, useParallelTrainer = true)
    if (false) {
      // Print test results to file
      val source = new java.io.PrintStream(new File("pos1-test-output.txt"))
      for (s <- testSentences) {
        for (t <- s.tokens) { val p = t.posLabel; source.println("%s %20s  %6s %6s".format(if (p.valueIsTarget) " " else "*", t.string, p.target.categoryValue, p.categoryValue)) }
        source.println()
      }
      source.close()
    }
  }

  def process(d: Document) = { predict(d); d }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token], classOf[Sentence], classOf[segment.PlainNormalizedTokenString])
  def postAttrs: Iterable[Class[_]] = List(classOf[PennPosLabel])
  override def tokenAnnotationString(token:Token): String = { val label = token.attr[PennPosLabel]; if (label ne null) label.categoryValue else "(null)" }
}

// object POS1 is defined in app/nlp/pos/package.scala

/** The default POS1, trained on Penn Treebank Wall Street Journal, with parameters loaded from resources in the classpath. */
object POS1WSJ extends POS1(cc.factorie.util.ClasspathURL[POS1]("-WSJ.factorie"))

/** The default POS1, trained on all Ontonotes training data (including Wall Street Journal), with parameters loaded from resources in the classpath. */
// TODO Set up so that POS1WSJ and POS1Ontonotes parameter loading locations can be set independently. 
//class POS1Ontonotes(url:java.net.URL) extends POS1(url)
//object POS1Ontonotes extends POS1Ontonotes(cc.factorie.util.ClasspathURL[POS1Ontonotes](".factorie"))
object POS1Ontonotes extends POS1(cc.factorie.util.ClasspathURL[POS1]("-Ontonotes.factorie"))

class POS1Opts extends cc.factorie.util.DefaultCmdOptions {
  val modelFile = new CmdOption("model", "", "FILENAME", "Filename for the model (saving a trained model or reading a running model.")
  val testFile = new CmdOption("test", "", "FILENAME", "OWPL test file.")
  val trainFile = new CmdOption("train", "", "FILENAME", "OWPL training file.")
  val l1 = new CmdOption("l1", 0.000001,"FLOAT","l1 regularization weight")
  val l2 = new CmdOption("l2", 0.00001,"FLOAT","l2 regularization weight")
  val rate = new CmdOption("rate", 10.0,"FLOAT","base learning rate")
  val delta = new CmdOption("delta", 100.0,"FLOAT","learning rate decay")
  val cutoff = new CmdOption("cutoff", 2, "INT", "Discard features less frequent than this before training.")
  val updateExamples = new  CmdOption("update-examples", true, "BOOL", "Whether to update examples in later iterations during training.")
  val useHingeLoss = new CmdOption("use-hinge-loss", false, "BOOL", "Whether to use hinge loss (or log loss) during training.")
  val saveModel = new CmdOption("save-model", false, "BOOL", "Whether to save the trained model.")
  val runText = new CmdOption("run", "", "FILENAME", "Plain text file on which to run.")
}


object POS1Trainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new POS1Opts
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)
    // Expects three command-line arguments: a train file, a test file, and a place to save the model in
    // the train and test files are supposed to be in OWPL format
    val pos = new POS1
    val trainDocs = load.LoadOntonotes5.fromFilename(opts.trainFile.value)
    val testDocs = load.LoadOntonotes5.fromFilename(opts.testFile.value)
    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.tokenCount).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.tokenCount).sum))
    pos.train(trainDocs.flatMap(_.sentences), testDocs.flatMap(_.sentences),
              opts.rate.value, opts.delta.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, l1Factor=opts.l1.value, l2Factor=opts.l2.value)
    if (opts.saveModel.value) {
      println("pre serialize accuracy: " + pos.accuracy(testDocs.flatMap(_.sentences)))
      pos.serialize(opts.modelFile.value)
      val pos2 = new POS1
      pos2.deserialize(new java.io.File(opts.modelFile.value))
      println(s"pre accuracy: ${pos.accuracy(testDocs.flatMap(_.sentences))} post accuracy: ${pos2.accuracy(testDocs.flatMap(_.sentences))}")
    }
    pos.accuracy(testDocs.flatMap(_.sentences))
  }
}

object POS1Optimizer {
  def main(args: Array[String]) {
    val opts = new POS1Opts
    opts.parse(args)
    opts.saveModel.setValue(false)
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new cc.factorie.util.LogUniformDoubleSampler(1e-10, 1e2))
    val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
    val cutoff = cc.factorie.util.HyperParameter(opts.cutoff, new cc.factorie.util.SampleFromSeq(List(0,1,2,3)))
    /*
    val ssh = new cc.factorie.util.SSHActorExecutor("apassos",
      Seq("avon1", "avon2"),
      "/home/apassos/canvas/factorie-test",
      "try-log/",
      "cc.factorie.app.nlp.parse.DepParser2",
      10, 5)
      */
    val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.pos.POS1Trainer")
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

