package cc.factorie.app.nlp.pos
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.classify.{MultiClassModel, LogLinearTemplate2, LogLinearModel}
import cc.factorie.la._
import cc.factorie.optimize.{ConstantLearningRate, AdaGrad, HogwildTrainer}
import cc.factorie.util.{BinarySerializer, CubbieConversions, DoubleAccumulator}
import scala.collection.mutable.HashMap
import java.io.File
import org.junit.Assert._

class POS3 extends DocumentAnnotator {
  def this(filename: String) = { this(); deserialize(filename) }
  object FeatureDomain extends CategoricalDimensionTensorDomain[String]
  class ClassifierModel extends MultiClassModel {
    val evidence = Weights(new la.DenseTensor2(PTBPosDomain.size, FeatureDomain.dimensionSize))
  }
  val model = new ClassifierModel

  def lemmatize(string:String): String = cc.factorie.app.strings.simplifyDigits(string) // .toLowerCase

  object WordData {
    val ambiguityClasses = collection.mutable.HashMap[String,String]()
    val ambiguityClassThreshold = 0.7
    val wordInclusionThreshold = 1

    def preProcess(documents: Seq[Document]) {
      val wordCounts = collection.mutable.HashMap[String,Int]()
      val posCounts = collection.mutable.HashMap[String,Array[Int]]()
      var tokenCount = 0
      documents.foreach(doc => {
        //println("POS3.WordData.preProcess doc.tokens.length "+doc.tokens.length)
        doc.tokens.foreach(t => {
          tokenCount += 1
          if (t.attr[PTBPosLabel] eq null) {
            println("POS3.WordData.preProcess tokenCount "+tokenCount)
            println("POS3.WordData.preProcess token "+t.prev.string+" "+t.prev.attr)
            println("POS3.WordData.preProcess token "+t.string+" "+t.attr)
          }
          val lemma = t.lemmaString
          if (!wordCounts.contains(lemma)) {
            wordCounts(lemma) = 0
            posCounts(lemma) = Array.fill(PTBPosDomain.size)(0)
          }
          wordCounts(lemma) += 1
          posCounts(lemma)(t.attr[PTBPosLabel].intValue) += 1
        })
      })
      wordCounts.keys.foreach(w => {
        if (wordCounts(w) >= wordInclusionThreshold) {
          val counts = wordCounts(w)
          val pos = posCounts(w)
          val bestPos = (0 until 45).maxBy(i => pos(i))
          if (pos(bestPos) > ambiguityClassThreshold*counts)
            ambiguityClasses(w) = bestPos.toString
        }
      })
    }
  }
  
  /** Return the features for the given Token */
  def features(token:Token): SparseBinaryTensor1 = {
    val result = new SparseBinaryTensor1(FeatureDomain.dimensionSize)
    result.sizeHint(40)
    def lemmaStringAtOffset(offset:Int): String = "W@"+offset+"="+token.lemmaStringAtOffset(offset)
    def affinityTagAtOffset(offset:Int): String = "A@"+offset+"="+WordData.ambiguityClasses.getOrElse(token.lemmaStringAtOffset(offset), null)
    def posTagAtOffset(offset:Int): String = { val t = token.next(offset); if (t ne null) t.attr[PTBPosLabel].categoryValue else null }
    def addFeature(s:String): Unit = if (s ne null) { val i = FeatureDomain.dimensionDomain.index(s); if (i >= 0) result._appendUnsafe(i) }
    // Lemmas at offsets
    val wp3 = lemmaStringAtOffset(-3)
    val wp2 = lemmaStringAtOffset(-2)
    val wp1 = lemmaStringAtOffset(-1)
    val w0 = lemmaStringAtOffset(0)
    val wm1 = lemmaStringAtOffset(1)
    val wm2 = lemmaStringAtOffset(2)
    val wm3 = lemmaStringAtOffset(3)
    // Affinity classes at next offsets
    val a0 = affinityTagAtOffset(0)
    val ap1 = affinityTagAtOffset(1)
    val ap2 = affinityTagAtOffset(2)
    val ap3 = affinityTagAtOffset(3)
    // POS tags at prev offsets
    val pm1 = posTagAtOffset(-1)
    val pm2 = posTagAtOffset(-2)
    val pm3 = posTagAtOffset(-3)
    
    addFeature(wp3)
    addFeature(wp2)
    addFeature(wp1)
    addFeature(w0)
    addFeature(wm1)
    addFeature(wm2)
    addFeature(wm3)
    addFeature(pm3)
    addFeature(pm2)
    addFeature(pm1)
    addFeature(a0)
    addFeature(ap1)
    addFeature(ap2)
    addFeature(ap3)
    addFeature(wm2+wm1)
    addFeature(wm1+w0)
    addFeature(w0+wp1)
    addFeature(wp1+wp2)
    addFeature(wm1+wp1)
    addFeature(pm2+pm1)
    addFeature(ap1+ap2)
    addFeature(pm1+ap1)
    addFeature(pm1+a0)
    addFeature(a0+ap1)
    addFeature(wm2+wm1+w0)
    addFeature(wm1+w0+wp1)
    addFeature(w0+wp1+wp2)
    addFeature(wm2+wm1+wp1)
    addFeature(wm1+wp1+wp2)
    addFeature(pm2+pm1+a0)
    addFeature(pm1+a0+ap1)
    addFeature(pm2+pm1+ap1)
    addFeature(pm1+ap1+ap2)
    addFeature(a0+ap1+ap2)
    addFeature("PREFIX3="+w0.take(3))
    addFeature("SUFFIX4="+w0.takeRight(4))
    //addFeature("SUFFIX2="+w0.takeRight(2))  // I think this would help with NNPS -akm
    addFeature("SHAPE="+cc.factorie.app.strings.stringShape(w0, 2)) // TODO(apassos): add the remaining jinho features not contained in shape
    addFeature("HasPeriod="+(w0.indexOf('.') >= 0))
    addFeature("HasHyphen="+(w0.indexOf('-') >= 0))
    addFeature("HasDigit="+w0.matches(".*[0-9].*"))
    result
  }


  var exampleSetsToPrediction = false
  class TokenClassifierExample(model: ClassifierModel, val token: Token, lossAndGradient: optimize.LinearObjectives.MultiClass) extends optimize.Example {
    override def accumulateExampleInto(gradient: TensorSetAccumulator, value: DoubleAccumulator) {
      val featureVector = features(token)
      val posLabel = token.attr[PTBPosLabel]
      new optimize.LinearMultiClassExample(model.evidence, featureVector, posLabel.targetIntValue, lossAndGradient).accumulateExampleInto(gradient, value)
      if (exampleSetsToPrediction) {
        val prediction = model.evidence.value * featureVector
        token.attr[PTBPosLabel].set(prediction.maxIndex)(null)
      }
    }
  }
  
  def predict(tokens: Seq[Token]): Unit = {
    val weightsMatrix = model.evidence.value
    for (token <- tokens) {
      assert(token.attr[cc.factorie.app.nlp.lemma.SimplifyDigitsTokenLemma] ne null)
      if (token.attr[PTBPosLabel] eq null) token.attr += new PTBPosLabel(token, "NNP")
      val featureVector = features(token)
      val prediction = weightsMatrix * featureVector
      token.attr[PTBPosLabel].set(prediction.maxIndex)(null)
    }
  }
  def predict(span: TokenSpan): Unit = predict(span.tokens)
  def predict(document: Document): Unit = {
    if (document.sentences.length > 0) document.sentences.foreach(predict(_))  // we have Sentence boundaries 
    else predict(document.tokens) // we don't // TODO But if we have trained with Sentence boundaries, won't this hurt accuracy?
  }

  def serialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename); if (file.getParentFile != null && !file.getParentFile.exists) file.getParentFile.mkdirs()
    assert(FeatureDomain.dimensionDomain ne null); assert(model ne null); assert(WordData.ambiguityClasses ne null)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, model, WordData.ambiguityClasses, file)
    //val acCubbie: Cubbie = WordData.ambiguityClasses
    //BinarySerializer.serialize(FeatureDomain.dimensionDomain, file)
  }

  def deserialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename)
    assert(file.exists(), "Trying to load non-existent file: '" +file)
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, model, WordData.ambiguityClasses, file)
  }

  def train(trainingFile: String, testFile: String, modelFile: String, alpha: Double, gamma: Double, cutoff: Int, doBootstrap: Boolean, useHingeLoss: Boolean, saveModel: Boolean) {
    val trainDocs = LoadOWPL.fromFilename(trainingFile, (t,s) => new PTBPosLabel(t,s))
    val testDocs = LoadOWPL.fromFilename(testFile, (t,s) => new PTBPosLabel(t,s))
    //for (d <- trainDocs) println("POS3.train 1 trainDoc.length="+d.length)
    println("Read %d training tokens.".format(trainDocs.map(_.length).sum))
    println("Read %d testing tokens.".format(testDocs.map(_.length).sum))
    // TODO Accomplish this lemmatizing instead by calling POS3.preProcess
    //for (d <- trainDocs) println("POS3.train 2 trainDoc.length="+d.length)
    for (doc <- (trainDocs ++ testDocs)) {
      cc.factorie.app.nlp.segment.SimplifyPTBTokenNormalizer.process(doc)
      cc.factorie.app.nlp.lemma.SimplifyDigitsLemmatizer.process(doc)
      //println("POS3.train:"); doc.tokens.take(50).foreach(t => println(t.string))
    }
    //for (d <- trainDocs) println("POS3.train 3 trainDoc.length="+d.length)
    WordData.preProcess(trainDocs)
    val sentences = trainDocs.flatMap(_.sentences)
    val testSentences = testDocs.flatMap(_.sentences)
    // Prune features by count
    FeatureDomain.dimensionDomain.gatherCounts = true
    for (sentence <- sentences; token <- sentence.tokens) features(token)
    FeatureDomain.dimensionDomain.trimBelowCount(cutoff)
    FeatureDomain.freeze()
    println("After pruning using %d features.".format(FeatureDomain.dimensionDomain.size))
    val numIterations = 2
    var iteration = 0
    
    //val trainer = new cc.factorie.optimize.SGDTrainer(model, new cc.factorie.optimize.LazyL2ProjectedGD(l2=1.0, rate=1.0), maxIterations = 10, logEveryN=100000)
    val trainer = new cc.factorie.optimize.OnlineTrainer(model.parameters, new cc.factorie.optimize.AdaGrad(rate=1.0), maxIterations = 10, logEveryN=100000)
    while (iteration < numIterations && !trainer.isConverged) {
      iteration += 1
      val examples = sentences.shuffle.flatMap(sentence => {
        (0 until sentence.length).map(i => new TokenClassifierExample(model, sentence.tokens(i),
            if (useHingeLoss) cc.factorie.optimize.LinearObjectives.hingeMultiClass else cc.factorie.optimize.LinearObjectives.logMultiClass))
      })
      trainer.processExamples(examples)
      exampleSetsToPrediction = doBootstrap
      var total = 0.0
      var correct = 0.0
      var totalTime = 0L
      testSentences.foreach(s => {
        val t0 = System.currentTimeMillis()
        predict(s)
        totalTime += (System.currentTimeMillis()-t0)
        for (i <- 0 until s.length) {
          total += 1
          if (s.tokens(i).attr[PTBPosLabel].valueIsTarget) correct += 1.0
        }
      })
      println("Accuracy: " + (correct/total) + " tokens/sec: " + 1000.0*testSentences.map(_.length).sum/totalTime)
    }
    if (saveModel) serialize(modelFile)
  }

  def process1(d: Document) = { predict(d); d }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[lemma.SimplifyDigitsTokenLemma])
  def postAttrs: Iterable[Class[_]] = List(classOf[PTBPosLabel])
  override def tokenAnnotationString(token:Token): String = { val label = token.attr[PTBPosLabel]; if (label ne null) label.categoryValue else "(null)" }
}


object POS3 {
  def main(args: Array[String]) {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val modelFile = new CmdOption("model", "", "FILENAME", "Filename for the model (saving a trained model or reading a running model.")
      val testFile = new CmdOption("test", "", "FILENAME", "OWPL test file.")
      val trainFile = new CmdOption("train", "", "FILENAME", "OWPL training file.")
      val lrate = new CmdOption("lrate", 0.1, "FLOAT", "Learning rate for training.")
      val decay = new CmdOption("decay", 0.01, "FLOAT", "Learning rate decay for training.")
      val cutoff = new CmdOption("cutoff", 2, "INT", "Discard features less frequent than this before training.")
      val updateExamples = new  CmdOption("update-examples", true, "BOOL", "Whether to update examples in later iterations during training.")
      val useHingeLoss = new CmdOption("use-hinge-loss", false, "BOOL", "Whether to use hinge loss (or log loss) during training.")
      val saveModel = new CmdOption("save-model", false, "BOOL", "Whether to save the trained model.")
      val runText = new CmdOption("run", "", "FILENAME", "Plain text file on which to run.")
    }
    opts.parse(args)
    if (opts.trainFile.wasInvoked) {
      // Expects three command-line arguments: a train file, a test file, and a place to save the model in
      // the train and test files are supposed to be in OWPL format
      val Pos = new POS3
      Pos.train(opts.trainFile.value, opts.testFile.value, opts.modelFile.value,
                opts.lrate.value, opts.decay.value, opts.cutoff.value, opts.updateExamples.value, opts.useHingeLoss.value, opts.saveModel.value)
    } else if (opts.runText.wasInvoked) {
      val pos = new POS3
      pos.deserialize(opts.modelFile.value)
      val doc = cc.factorie.app.nlp.LoadPlainText.fromFile(new java.io.File(opts.runText.value), segmentSentences=true)
      pos.process(doc)
      println(doc.owplString(List((t:Token)=>t.attr[PTBPosLabel].categoryValue)))
    }
  }

  def fromFilename(name: String): POS3 = {
    val c = new POS3
    c.deserialize(name)
    c
  }
}
