package cc.factorie.app.nlp.pos
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.classify.{LogLinearTemplate2, LogLinearModel}
import cc.factorie.la._
import cc.factorie.optimize.{StepwiseGradientAscent, AdaGrad, HogwildTrainer}
import cc.factorie.util.DoubleAccumulator
import scala.collection.mutable.HashMap
import java.io.File
import org.junit.Assert._

class POS3 extends DocumentProcessor {
  object FeatureDomain extends CategoricalDimensionTensorDomain[String]
  val model = new LogLinearModel[CategoricalVariable[String], CategoricalDimensionTensorVar[String]]((a) => null, (b) => null, PTBPosDomain, FeatureDomain)
  
  def lemmatize(string:String): String = cc.factorie.app.strings.simplifyDigits(string) // .toLowerCase

  object WordData {
    val ambiguityClasses = collection.mutable.HashMap[String,String]()
    val ambiguityClassThreshold = 0.7
    val wordInclusionThreshold = 1

    def preProcess(documents: Seq[Document]) {
      val wordCounts = collection.mutable.HashMap[String,Int]()
      val posCounts = collection.mutable.HashMap[String,Array[Int]]()
      documents.foreach(doc => {
        doc.tokens.foreach(t => {
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
    addFeature("SUFIX4="+w0.takeRight(4))
    addFeature("SHAPE="+cc.factorie.app.strings.stringShape(w0, 2)) // TODO(apassos): add the remaining jinho features not contained in shape
    addFeature("HasPeriod="+w0.contains("."))
    addFeature("HasDigit="+w0.contains("[0-9]"))
    addFeature("HasHyphen="+w0.contains("-"))
    result
  }


  var exampleSetsToPrediction = false
  class TokenClassifierExample(val token:Token, lossAndGradient: optimize.ObjectiveFunctions.MultiClassObjectiveFunction) extends optimize.Example[LogLinearModel[_,_]] {
    override def accumulateExampleInto(model: LogLinearModel[_,_], gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
      val featureVector = features(token)
      val posLabel = token.attr[PTBPosLabel]
      new optimize.GLMExample(featureVector, posLabel.targetIntValue, lossAndGradient).accumulateExampleInto(model, gradient, value) 
      if (exampleSetsToPrediction) {
        val weightsMatrix = model.evidenceTemplate.weights
        val prediction = weightsMatrix * featureVector
        token.attr[PTBPosLabel].set(prediction.maxIndex)(null)
      }
    }
  }
  
  def predict(s: Sentence)(implicit d: DiffList = null) {
    // TODO What is the best way to test/ensure that lemmatization has been done already?
    for (token <- s.tokens) token.setLemmaString(lemmatize(token.string))
    s.tokens.foreach(t => if (t.attr[PTBPosLabel] eq null) t.attr += new PTBPosLabel(t, "NNP"))
    val weightsMatrix = model.evidenceTemplate.weights
    for (i <- 0 until s.length) {
      val featureVector = features(s.tokens(i))
      val prediction = weightsMatrix * featureVector
      s.tokens(i).attr[PTBPosLabel].set(prediction.maxIndex)
    }
  }

  def serialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename); if (file.getParentFile eq null) file.getParentFile.mkdirs()
    assert(FeatureDomain.dimensionDomain ne null); assert(model ne null); assert(WordData.ambiguityClasses ne null)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, model, WordData.ambiguityClasses, file)
    //val acCubbie: Cubbie = WordData.ambiguityClasses
    //BinarySerializer.serialize(FeatureDomain.dimensionDomain, file)
  }

  def deSerialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename)
    assert(file.exists(), "Trying to load non-existent file: '" +file)
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, model, WordData.ambiguityClasses, file)
  }

  def train(trainingFile: String, testFile: String, modelFile: String, alpha: Double, gamma: Double, cutoff: Int, doBootstrap: Boolean, useHingeLoss: Boolean, saveModel: Boolean) {
    val trainDocs = LoadOWPL.fromFilename(trainingFile, (t,s) => new PTBPosLabel(t,s))
    val testDocs = LoadOWPL.fromFilename(testFile, (t,s) => new PTBPosLabel(t,s))
    println("Read %d training tokens.".format(trainDocs.map(_.length).sum))
    // TODO Lemmatizing should be pulled into a a DocumentProcessor
    for (doc <- (trainDocs ++ testDocs); token <- doc.tokens) token.setLemmaString(lemmatize(token.string)) // Calculate and set the lemmas once and for all.
    WordData.preProcess(trainDocs)
    val sentences = trainDocs.flatMap(_.sentences)
    val testSentences = testDocs.flatMap(_.sentences)
    // Prune features by count
    FeatureDomain.dimensionDomain.gatherCounts = true
    for (sentence <- sentences; token <- sentence.tokens) features(token)
    FeatureDomain.dimensionDomain.trimBelowCount(cutoff)
    FeatureDomain.freeze()
    println("After pruning using %d features.".format(FeatureDomain.dimensionDomain.size))
    val numIterations = 3
    var iteration = 0
    
    //val trainer = new cc.factorie.optimize.SGDTrainer(model, new cc.factorie.optimize.LazyL2ProjectedGD(l2=1.0, rate=1.0), maxIterations = 10, logEveryN=100000)
    val trainer = new cc.factorie.optimize.SGDTrainer(model, new cc.factorie.optimize.AdaGrad(rate=1.0), maxIterations = 10, logEveryN=100000)
    while (iteration < numIterations && !trainer.isConverged) {
      iteration += 1
      val examples = sentences.shuffle.flatMap(sentence => {
        (0 until sentence.length).map(i => new TokenClassifierExample(sentence.tokens(i),
            if (useHingeLoss) cc.factorie.optimize.ObjectiveFunctions.hingeMultiClassObjective else cc.factorie.optimize.ObjectiveFunctions.logMultiClassObjective))
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

  // NOTE: this method may mutate and return the same document that was passed in
  def process(d: Document) = { d.sentences.foreach(predict(_)); d}

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
      pos.deSerialize(opts.modelFile.value)
      val doc = cc.factorie.app.nlp.LoadPlainText.fromFile(new java.io.File(opts.runText.value), segmentSentences=true)
      pos.process(doc)
      println(doc.owplString(List((t:Token)=>t.attr[PTBPosLabel].categoryValue)))
    }
  }

  def load(name: String): ClassifierPos = {
    val c = new ClassifierPos
    c.deSerialize(name)
    c
  }
}
