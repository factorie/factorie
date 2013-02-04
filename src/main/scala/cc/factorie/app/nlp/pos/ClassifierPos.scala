package cc.factorie.app.nlp.pos

import cc.factorie._
import cc.factorie.app._
import cc.factorie.app.nlp._
import classify.{LogLinearTemplate2, LogLinearModel}
import la._
import optimize.{StepwiseGradientAscent, AdaGrad, HogwildTrainer}
import util.DoubleAccumulator
import collection.mutable.HashMap
import java.io.File
import org.junit.Assert._

/**
 * Created with IntelliJ IDEA.
 * User: apassos
 * Date: 1/21/13
 * Time: 4:32 PM
 * To change this template use File | Settings | File Templates.
 */
class ClassifierPos extends DocumentProcessor {
  object ClassifierPosFeatureDomain extends CategoricalDimensionTensorDomain[String]()
  val model = new LogLinearModel[CategoricalVariable[String], CategoricalDimensionTensorVar[String]]((a) => null, (b) => null, PosDomain, ClassifierPosFeatureDomain)

  object WordData {
    val ambiguityClasses = collection.mutable.HashMap[String,String]()
    val ambiguityClassThreshold = 0.7
    val wordInclusionThreshold = 1

    def preProcess(documents: Seq[Document]) {
      val wordCounts = collection.mutable.HashMap[String,Int]()
      val posCounts = collection.mutable.HashMap[String,Array[Int]]()

      documents.foreach(doc => {
        doc.tokens.foreach(t => {
          val lemma = strings.simplifyDigits(t.string).toLowerCase
          if (!wordCounts.contains(lemma)) {
            wordCounts(lemma) = 0
            posCounts(lemma) = Array.fill(PosDomain.size)(0)
          }
          wordCounts(lemma) += 1
          posCounts(lemma)(t.attr[PosLabel].intValue) += 1
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


  def addFeature(v: SparseBinaryTensor1, f: String) {
    val i = ClassifierPosFeatureDomain.dimensionDomain.index(f)
    if (i != -1) v._appendUnsafe(i)
  }
  def getAffinity(sent: SentenceData, pos: Int) = {
    val f = sent.get(sent.lemmas, pos)
    if (WordData.ambiguityClasses.contains(f))
      WordData.ambiguityClasses(f)
    else
      ""
  }
  def getLemmaFeature(sent: SentenceData, pos: Int, dif: Int) = {
    val prefix = "W"+(dif)+"="
    val lemma = sent.get(sent.lemmas, pos+dif)
    prefix+lemma
  }
  def addFeatures(sent: SentenceData, pos: Int, f: SparseBinaryTensor1) {
    val wp3 = getLemmaFeature(sent, pos, +3)
    val wp2 = getLemmaFeature(sent, pos, +2)
    val wp1 = getLemmaFeature(sent, pos, +1)
    val wf = getLemmaFeature(sent, pos, 0)
    val wm1 = getLemmaFeature(sent, pos, -1)
    val wm2 = getLemmaFeature(sent, pos, -2)
    val wm3 = getLemmaFeature(sent, pos, -3)

    val pm3 = "POS-3="+sent.sent.tokens(math.max(0,pos-3)).attr[PosLabel].categoryValue
    val pm2 = "POS-2="+sent.sent.tokens(math.max(0,pos-2)).attr[PosLabel].categoryValue
    val pm1 = "POS-1="+sent.sent.tokens(math.max(0,pos-1)).attr[PosLabel].categoryValue
    val a0 = "A="+getAffinity(sent, pos)
    val ap1 = "A+1="+getAffinity(sent, pos+1)
    val ap2 = "A+2="+getAffinity(sent, pos+2)
    val ap3 = "A+3="+getAffinity(sent, pos+3)
    f._hintSize(40)

    addFeature(f, wp3)
    addFeature(f, wp2)
    addFeature(f, wp1)
    addFeature(f, wf)
    addFeature(f, wm1)
    addFeature(f, wm2)
    addFeature(f, wm3)
    addFeature(f, pm3)
    addFeature(f, pm2)
    addFeature(f, pm1)
    addFeature(f, a0)
    addFeature(f, ap1)
    addFeature(f, ap2)
    addFeature(f, ap3)
    addFeature(f, wm2+wm1)
    addFeature(f, wm1+wf)
    addFeature(f, wf+wp1)
    addFeature(f, wp1+wp2)
    addFeature(f, wm1+wp1)
    addFeature(f, pm2+pm1)
    addFeature(f, ap1+ap2)
    addFeature(f, pm1+ap1)
    addFeature(f, pm1+a0)
    addFeature(f, a0+ap1)
    addFeature(f, wm2+wm1+wf)
    addFeature(f, wm1+wf+wp1)
    addFeature(f, wf+wp1+wp2)
    addFeature(f, wm2+wm1+wp1)
    addFeature(f, wm1+wp1+wp2)
    addFeature(f, pm2+pm1+a0)
    addFeature(f, pm1+a0+ap1)
    addFeature(f, pm2+pm1+ap1)
    addFeature(f, pm1+ap1+ap2)
    addFeature(f, a0+ap1+ap2)
    addFeature(f, "PREFX3="+wf.take(3))
    addFeature(f, "SUFX4="+wf.takeRight(4))
    addFeature(f, "Shape="+strings.stringShape(wf, 2)) // TODO(apassos): add the remaining jinho features not contained in shape
    addFeature(f, "HasPeriod="+wf.contains("."))
    addFeature(f, "HasDigit="+wf.contains("0"))
    addFeature(f, "HasHyphen="+wf.contains("-"))
  }


  class SentenceData(val sent: Sentence) {
    val length = sent.length
    val lemmas = sent.tokens.map(t => strings.simplifyDigits(t.string).toLowerCase)
    def get(s: Seq[String], i: Int) = if ((0 <= i) && (i < s.length)) s(i) else ""
  }

  var setToPrediction = false
  class LocalClassifierExample(val sentData: SentenceData, pos: Int, lossAndGradient: optimize.ObjectiveFunctions.MultiClassObjectiveFunction) extends optimize.Example[LogLinearModel[_,_]] {
    override def accumulateExampleInto(model: LogLinearModel[_,_], gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator) {
      val featureVector = new SparseBinaryTensor1(ClassifierPosFeatureDomain.dimensionSize)
      addFeatures(sentData, pos, featureVector)
      new optimize.GLMExample(featureVector, sentData.sent(pos).attr[PosLabel].intValue, lossAndGradient).accumulateExampleInto(model, gradient, value, margin)
      if (setToPrediction) {
        val weightsMatrix = model.evidenceTemplate.weights
        val prediction = weightsMatrix * featureVector
        sentData.sent.tokens(pos).attr[PosLabel].set(prediction.maxIndex)(null)
      }
    }
  }

  def predict(s: Sentence)(implicit d: DiffList = null) {
    val sent = new SentenceData(s)
    val weightsMatrix = model.evidenceTemplate.weights
    for (i <- 0 until s.length) {
      val featureVector = new SparseBinaryTensor1(ClassifierPosFeatureDomain.dimensionSize)
      addFeatures(sent, i, featureVector)
      val prediction = weightsMatrix * featureVector
      s.tokens(i).attr[PosLabel].set(prediction.maxIndex)
    }
  }

  def serialize(prefix: String) {
    val modelFile = new File(prefix + "-model")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    BinaryFileSerializer.serialize(model, modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinaryFileSerializer.serialize(PosDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinaryFileSerializer.serialize(ClassifierPosFeatureDomain.dimensionDomain, featuresDomainFile)
    val ambClassFile = new File(prefix + "-ambiguityClasses")
    BinaryFileSerializer.serialize(WordData.ambiguityClasses, ambClassFile)
  }

  def deSerialize(prefix: String) {
    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-labelDomain'")
    BinaryFileSerializer.deserialize(PosDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinaryFileSerializer.deserialize(ClassifierPosFeatureDomain.dimensionDomain, featuresDomainFile)
    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix + "-model'")
    BinaryFileSerializer.deserialize(model, modelFile)
    val ambClassFile = new File(prefix + "-ambiguityClasses")
    BinaryFileSerializer.deserialize(WordData.ambiguityClasses, ambClassFile)
  }

  def train(trainingFile: String, testFile: String, modelFile: String, alpha: Double, gamma: Double, cutoff: Int, doBootstrap: Boolean, useHingeLoss: Boolean, saveModel: Boolean) {
    val trainDocs = LoadOWPL.fromFilename(trainingFile, (t,s) => new PosLabel(t,s))
    val testDocs = LoadOWPL.fromFilename(testFile, (t,s) => new PosLabel(t,s))
    WordData.preProcess(trainDocs)
    PosDomain.freeze()
    val sentences = trainDocs.flatMap(_.sentences)
    val testSentences = testDocs.flatMap(_.sentences)
    ClassifierPosFeatureDomain.dimensionDomain.gatherCounts = true
    sentences.shuffle.flatMap(s => {
      val sd = new SentenceData(s)
      (0 until s.length).map(i => {
        val featureVector = new SparseBinaryTensor1(Int.MaxValue)
        addFeatures(new SentenceData(s), i, featureVector)
      })
    })
    ClassifierPosFeatureDomain.dimensionDomain.trimBelowCount(cutoff)
    ClassifierPosFeatureDomain.freeze()
    val trainer = new optimize.SGDTrainer(model, new optimize.AdaGradDualAveraging(l1=0.0000, rate=alpha, delta=gamma), maxIterations = 10, logEveryN=100000)
    while(!trainer.isConverged) {
      val examples = sentences.shuffle.flatMap(s => {
        val sd = new SentenceData(s)
        (0 until s.length).map(i => new LocalClassifierExample(sd, i, if (useHingeLoss) optimize.ObjectiveFunctions.hingeMultiClassObjective else optimize.ObjectiveFunctions.logMultiClassObjective))
      })
      trainer.processExamples(examples)
      setToPrediction = doBootstrap
      var total = 0.0
      var correct = 0.0
      var totalTime = 0L
      testSentences.foreach(s => {
        val t0 = System.currentTimeMillis()
        predict(s)
        totalTime += (System.currentTimeMillis()-t0)
        for (i <- 0 until s.length) {
          total += 1
          if (s.tokens(i).attr[PosLabel].valueIsTarget) correct += 1.0
        }
      })
      println("Accuracy: " + (correct/total) + " tokens/sec: " + 1000.0*testSentences.map(_.length).sum/totalTime)
      if (saveModel) serialize(modelFile+"-iter-"+trainer.iteration)
    }
  }

  // NOTE: this method may mutate and return the same document that was passed in
  def process(d: Document) = { d.sentences.foreach(predict(_)); d}

}

object ClassifierPos {
  def main(args: Array[String]) {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val modelFile = new CmdOption("model", "", "FILE", "model file prefix")
      val testFile = new CmdOption("test", "", "FILE", "test file")
      val trainFile = new CmdOption("train", "", "FILE", "train file")
      val lrate = new CmdOption("lrate", "0.1", "FLOAT", "learning rate")
      val decay = new CmdOption("decay", "0.01", "FLOAT", "learning rate decay")
      val cutoff = new CmdOption("cutoff", "2", "INT", "discard features less frequent than this")
      val updateExamples = new  CmdOption("updateExamples", "true", "BOOL", "whether to update examples in later iterations")
      val useHingeLoss = new CmdOption("useHingeLoss", "false", "BOOL", "whether to use hinge loss (or log loss)")
      val saveModel = new CmdOption("saveModel", "false", "BOOL", "whether to save the model")
    }
    opts.parse(args)
    // Expects three command-line arguments: a train file, a test file, and a place to save the model in
    // the train and test files are supposed to be in OWPL format
    val Pos = new ClassifierPos
    Pos.train(opts.trainFile.value,
      opts.testFile.value,
      opts.modelFile.value,
      opts.lrate.value.toFloat,
      opts.decay.value.toFloat,
      opts.cutoff.value.toInt,
      opts.updateExamples.value.toBoolean,
      opts.useHingeLoss.value.toBoolean,
      opts.saveModel.value.toBoolean)
  }

  def load(name: String): ClassifierPos = {
    val c = new ClassifierPos
    c.deSerialize(name)
    c
  }
}

object TestClassifierPos {
  def main(args: Array[String]) {
    val modelFile = args(0)
    val testFile = args(1)
    val pos = ClassifierPos.load(modelFile)
    val data = LoadOWPL.fromFilename(testFile, (t,s) => new PosLabel(t,s))
    val t0 = System.currentTimeMillis()
    data.foreach(d => pos.process(d))
    val time = System.currentTimeMillis() - t0
    var total = 0.0
    var correct = 0.0
    data.flatMap(_.tokens).foreach(t => {
      total += 1
      if (t.posLabel.valueIsTarget)
        correct += 1
    })
    println("Test accuracy: " + (correct/total) +" time " + time + " tokens " + total + " tokens/second " + 1000*total.toFloat/time)
  }
}