package cc.factorie.app.nlp.ner


/**
 * Created by kate on 6/17/14.
 */

import java.io._
import java.net.URL
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, Lexicon, StaticLexicons}
import cc.factorie.util._
import cc.factorie.variable._
import cc.factorie.app.chain.ChainModel
import cc.factorie.optimize.{Trainer, AdaGrad, ParameterAveraging}
import cc.factorie.app.chain.SegmentEvaluation
import scala.reflect.{ClassTag, classTag}

/**
 * NER tagger for the CoNLL 2003 corpus
 *
 * Training time: ~3 minutes (on blake, 30 Oct. 4:00pm)
 * tokens per second: 8431.02310444517
 * docs per second: 48.24287793720109 (avg doc length = 200 tokens)
 *
 * CoNLL 2003 dev set (eng.testa)
 * OVERALL  f1=0.933593 p=0.939802 r=0.927465 (tp=5511 fp=353 fn=431 true=5942 pred=5864) acc=0.985865 (50636/51362)
 * LOC      f1=0.965931 p=0.967249 r=0.964616 (tp=1772 fp=60 fn=65 true=1837 pred=1832)
 * MISC     f1=0.876404 p=0.909091 r=0.845987 (tp=780 fp=78 fn=142 true=922 pred=858)
 * ORG      f1=0.892065 p=0.899848 r=0.884415 (tp=1186 fp=132 fn=155 true=1341 pred=1318)
 * PER      f1=0.958897 p=0.955280 r=0.962541 (tp=1773 fp=83 fn=69 true=1842 pred=1856)
 *
 * CoNLL 2003 test set (eng.testb)
 * OVERALL  f1=0.885633 p=0.888315 r=0.882967 (tp=4987 fp=627 fn=661 true=5648 pred=5614) acc=0.973253 (45193/46435)
 * LOC      f1=0.915375 p=0.909953 r=0.920863 (tp=1536 fp=152 fn=132 true=1668 pred=1688)
 * MISC     f1=0.791034 p=0.803231 r=0.779202 (tp=547 fp=134 fn=155 true=702 pred=681)
 * ORG      f1=0.842767 p=0.838498 r=0.847080 (tp=1407 fp=271 fn=254 true=1661 pred=1678)
 * PER      f1=0.940327 p=0.955329 r=0.925788 (tp=1497 fp=70 fn=120 true=1617 pred=1567)
 *
 */
class ConllChainNer(implicit mp:ModelProvider[ConllChainNer], lexicons:StaticLexicons)
  extends ChainNer[ConllNerSpan, BilouConllNerTag](
    BilouConllNerDomain,
    (t, s) => new BilouConllNerTag(t, s),
    l => l.token,
    mp.provide,
    lexicons) with Serializable {
  def loadDocs(fileName: String): Seq[Document] = cc.factorie.app.nlp.load.LoadConll2003(BILOU=true).fromFilename(fileName)

  def newSpan(sec: Section, start: Int, length: Int, category: String) = new ConllNerSpan(sec, start, length, category)

  def newBuffer = new ConllNerSpanBuffer
}

//TODO this serialized model doesn't exist yet?
object ConllChainNer extends ConllChainNer()(ModelProvider.classpath(), new StaticLexicons()(LexiconsProvider.classpath)) with Serializable

class OntonotesChainNer()(implicit sCt:ClassTag[OntonotesNerSpan], tCt:ClassTag[BilouOntonotesNerTag], mp:ModelProvider[OntonotesChainNer], lexicons:StaticLexicons)
  extends ChainNer[OntonotesNerSpan, BilouOntonotesNerTag](BilouOntonotesNerDomain, (t, s) => new BilouOntonotesNerTag(t, s), l => l.token, mp.provide, lexicons) {
  def newBuffer = new OntonotesNerSpanBuffer()

  def newSpan(sec: Section, start: Int, length: Int, category: String) = new OntonotesNerSpan(sec, start, length, category)
}

// todo a serialized model for this does not exist
object OntonotesChainNer extends OntonotesChainNer()(classTag[OntonotesNerSpan], classTag[BilouOntonotesNerTag], ModelProvider.classpath(), new StaticLexicons()(LexiconsProvider.classpath))

/**
 * A base class for finite-state named entity recognizers
 */
abstract class ChainNer[S<: NerSpan, L<:NerTag](labelDomain: CategoricalDomain[String],
                                   newLabel: (Token, String) => L,
                                   labelToToken: L => Token,
                                   modelIs: InputStream=null,
                                   val lexicon: StaticLexicons)(implicit m: ClassTag[L], s: ClassTag[S]) extends NerAnnotator[S, L] {

  def prereqAttrs = Seq(classOf[Sentence])

  def annotateTokens(document: Document) =
    if(document.tokenCount > 0) {
      if (!document.tokens.head.attr.contains(m.runtimeClass))
        document.tokens.map(token => token.attr += newLabel(token, "O"))
      if (!document.tokens.head.attr.contains(classOf[ChainNERFeatures])) {
        document.tokens.map(token => {token.attr += new ChainNERFeatures(token)})
        addFeatures(document, (t:Token)=>t.attr[ChainNERFeatures])
      }
      document.sentences.collect {
        case sentence if sentence.nonEmpty =>
          val vars = sentence.tokens.map(_.attr[L]).toSeq
          model.maximize(vars)(null)
      }
      document
    } else {
      document
    }

  synchronized {

    lexicon.iesl.Month.toString()
    lexicon.iesl.Day.toString()

    lexicon.iesl.PersonFirst.toString()
    lexicon.iesl.PersonFirstHigh.toString()
    lexicon.iesl.PersonFirstHighest.toString()
    lexicon.iesl.PersonFirstMedium.toString()

    lexicon.iesl.PersonLast.toString()
    lexicon.iesl.PersonLastHigh.toString()
    lexicon.iesl.PersonLastHighest.toString()
    lexicon.iesl.PersonLastMedium.toString()

    lexicon.iesl.PersonHonorific.toString()

    lexicon.iesl.Company.toString()
    lexicon.iesl.JobTitle.toString()
    lexicon.iesl.OrgSuffix.toString()

    lexicon.iesl.Country.toString()
    lexicon.iesl.City.toString()
    lexicon.iesl.PlaceSuffix.toString()
    lexicon.iesl.UsState.toString()
    lexicon.iesl.Continents.toString()

    lexicon.wikipedia.Person.toString()
    lexicon.wikipedia.Event.toString()
    lexicon.wikipedia.Location.toString()
    lexicon.wikipedia.Organization.toString()
    lexicon.wikipedia.ManMadeThing.toString()
    lexicon.iesl.Demonym.toString()

    lexicon.wikipedia.Book.toString()
    lexicon.wikipedia.Business.toString()
    lexicon.wikipedia.Film.toString()

    lexicon.wikipedia.LocationAndRedirect.toString()
    lexicon.wikipedia.PersonAndRedirect.toString()
    lexicon.wikipedia.OrganizationAndRedirect.toString()
  }

  println("loaded lexicons")

  object ChainNERFeaturesDomain extends CategoricalVectorDomain[String]
  class ChainNERFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = ChainNERFeaturesDomain
    override def skipNonCategories = true
  }
  class ChainNERModel[Features <: CategoricalVectorVar[String]:ClassTag](featuresDomain: CategoricalVectorDomain[String],
                                                                labelToFeatures: L => Features,
                                                                labelToToken: L => Token,
                                                                tokenToLabel: Token => L)
    extends ChainModel(labelDomain, featuresDomain, labelToFeatures, labelToToken, tokenToLabel) //with Parameters {

  val model = new ChainNERModel[ChainNERFeatures](ChainNERFeaturesDomain, l => labelToToken(l).attr[ChainNERFeatures], labelToToken, t => t.attr[L])
  val objective = cc.factorie.variable.HammingObjective

  if (modelIs != null) {
    deserialize(modelIs)
    ChainNERFeaturesDomain.freeze()
    println("found model")
  }

  def serialize(stream: java.io.OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(ChainNERFeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: java.io.InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(ChainNERFeaturesDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    is.close()
  }

  def prefix( prefixSize : Int, cluster : String ) : String = if(cluster.length > prefixSize) cluster.substring(0, prefixSize) else cluster
  val clusters = JavaHashMap[String, String]()

  def addFeatures(document: Document, vf: Token => CategoricalVectorVar[String]): Unit = {
    document.annotators(classOf[ChainNERFeatures]) = ChainNer.this.getClass
    import cc.factorie.app.strings.simplifyDigits
    val tokenSequence = document.tokens.toSeq

    lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH")
    lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY")

    lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM")

    lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM")

    lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC")

    lexicon.iesl.Company.tagText(tokenSequence,vf, "COMPANY")
    lexicon.iesl.JobTitle.tagText(tokenSequence,vf, "JOB-TITLE")
    lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf, "ORG-SUFFIX")

    lexicon.iesl.Country.tagText(tokenSequence,vf, "COUNTRY")
    lexicon.iesl.City.tagText(tokenSequence,vf, "CITY")
    lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf, "PLACE-SUFFIX")
    lexicon.iesl.UsState.tagText(tokenSequence,vf, "USSTATE")
    lexicon.iesl.Continents.tagText(tokenSequence,vf, "CONTINENT")

    lexicon.wikipedia.Person.tagText(tokenSequence,vf, "WIKI-PERSON")
    lexicon.wikipedia.Event.tagText(tokenSequence,vf, "WIKI-EVENT")
    lexicon.wikipedia.Location.tagText(tokenSequence,vf, "WIKI-LOCATION")
    lexicon.wikipedia.Organization.tagText(tokenSequence,vf, "WIKI-ORG")
    lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf, "MANMADE")
    lexicon.iesl.Demonym.tagText(tokenSequence,vf, "DEMONYM")

    lexicon.wikipedia.Book.tagText(tokenSequence,vf, "WIKI-BOOK")
    lexicon.wikipedia.Business.tagText(tokenSequence,vf, "WIKI-BUSINESS")
    lexicon.wikipedia.Film.tagText(tokenSequence,vf, "WIKI-FILM")

    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf, "WIKI-LOCATION-REDIRECT")
    lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf, "WIKI-PERSON-REDIRECT")
    lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf, "WIKI-ORG-REDIRECT")

    for (token <- document.tokens) {
      val features = vf(token)
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += s"W=$word"
      features += s"SHAPE=${cc.factorie.app.strings.stringShape(rawWord, 2)}"
      if (token.isPunctuation) features += "PUNCTUATION"
      if (clusters.nonEmpty && clusters.contains(rawWord)) {
        features += "CLUS="+prefix(4,clusters(rawWord))
        features += "CLUS="+prefix(6,clusters(rawWord))
        features += "CLUS="+prefix(10,clusters(rawWord))
        features += "CLUS="+prefix(20,clusters(rawWord))
      }
    }

    for (sentence <- document.sentences)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))

    for (token <- document.tokens) {
      val word = simplifyDigits(token.string).toLowerCase
      val features = vf(token)
      if (word.length < 5){
        features += "P="+cc.factorie.app.strings.prefix(word, 4)
        features += "S="+cc.factorie.app.strings.suffix(word, 4)
      }
    }

    // Add features from window of 4 words before and after
    document.tokens.foreach(t => vf(t) ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.tokens.foreach(t => vf(t) ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))

    document.tokens.foreach(t => {
      if (t.isCapitalized && t.string.length > 1 &&
        !vf(t).activeCategories.exists(f => f.matches(".*FIRSTMENTION.*")))
      {
        var t2 = t
        while (t2.hasNext) {
          t2 = t2.next
          if (t2.string == t.string) {
            vf(t2) ++= vf(t).activeCategories.map(f => "FIRSTMENTION="+f)
          }
        }
      }
    })
    document.tokens.foreach(t => if (t.string.matches("[A-Za-z]+")) vf(t) ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
  }

  def sampleOutputString(tokens: Iterable[Token]): String = {
    val sb = new StringBuffer
    for (token <- tokens)
      sb.append(
        "%s %20s %10s %10s\n".format(
          if (token.attr[L with LabeledMutableCategoricalVar[String]].valueIsTarget) " " else "*",
          token.string, token.attr[L with LabeledMutableCategoricalVar[String]].target.categoryValue,
          token.attr[L].categoryValue))
    sb.toString
  }

  def train(trainDocs: Seq[Document], testDocs: Seq[Document], rate: Double=0.18, delta: Double=0.066)(implicit random: scala.util.Random): Double = {

    def labels(docs: Iterable[Document]): Iterable[L with LabeledMutableDiscreteVar] = {
      docs.flatMap(doc => doc.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))
    }

    println("initializing training features...")
    (trainDocs ++ testDocs).foreach(_.tokens.map(token => token.attr += new ChainNERFeatures(token)))
    trainDocs.foreach(addFeatures(_, (t:Token)=>t.attr[ChainNERFeatures]))
    ChainNERFeaturesDomain.freeze()
    println("initializing testing features...")
    testDocs.foreach(addFeatures(_, (t:Token)=>t.attr[ChainNERFeatures]))
    println(sampleOutputString(trainDocs.take(20).last.tokens.take(100)))

    val trainLabels = labels(trainDocs).toIndexedSeq
    val testLabels = labels(testDocs).toIndexedSeq

    val examples = trainDocs.flatMap(_.sentences.filter(_.length > 1).map(sentence => new model.ChainLikelihoodExample(sentence.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))))
    val optimizer = new AdaGrad(rate=rate, delta=delta) with ParameterAveraging

    def evaluate(){
      val segmentEvaluation = new SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](
        labelDomain.categories.filter(_.length > 2).map(_.substring(2)),
        "(B|U)-", "(I|L)-"
      )
      trainDocs.foreach(doc => {
        process(doc)
        for (sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
      })
      println(s"Train accuracy ${objective.accuracy(trainLabels)}")
      println(segmentEvaluation)
      if (testDocs.nonEmpty) {
        val testSegmentEvaluation = new SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](
          labelDomain.categories.filter(_.length > 2).map(_.substring(2)),
          "(B|U)-", "(I|L)-"
        )
        testDocs.foreach(doc => {
          process(doc)
          for (sentence <- doc.sentences) testSegmentEvaluation += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
        })
        println(s"Test accuracy ${objective.accuracy(testLabels)}")
        println(testSegmentEvaluation)
      }
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }

    println(s"training with ${examples.length} examples")
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)

    val finalEval = new SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](labelDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    val buf = new StringBuffer
    buf.append(new LabeledDiscreteEvaluation(testDocs.flatMap(_.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))))
    for (doc <- testDocs; sentence <- doc.sentences) finalEval += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
    println("final results:")
    println(finalEval)
    finalEval.f1
  }

  def printEvaluation(trainDocs: Iterable[Document], testDocs: Iterable[Document], iteration: String): Double = {
    println(s"TRAIN ${evaluationString(trainDocs)}")
    val result = evaluationString(testDocs)
    println(s"TEST $result")
    result
  }

  def evaluationString(documents: Iterable[Document]): Double = {
    val buf = new StringBuffer
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](labelDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    for (doc <- documents; sentence <- doc.sentences)
      segmentEvaluation += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
    println(s"Segment evaluation $segmentEvaluation")
    segmentEvaluation.f1
  }
}

class ChainNerOpts extends cc.factorie.util.CmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "CoNLLChainNer.factorie", "FILE", "Filename for the model (saving a trained model or reading a running model.")
  val serialize = new CmdOption("serialize", true, "BOOLEAN", "Whether to serialize at all")
  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data in CoNLL 2003 one-word-per-lineformat.")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data in CoNLL 2003 one-word-per-lineformat.")
  val brownClusFile = new CmdOption("brown", "brownBllipClusters", "FILE", "File containing brown clusters.")
  val trainDir = new CmdOption("train-dir", "", "STRING", "Path to directory of training data.")
  val testDir = new CmdOption("test-dir", "/iesl/canvas/ksilvers/data/final-ace/test", "STRING", "Path to directory of test data.")
  val l1 = new CmdOption("l1", 0.02, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 0.000001, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 1.0, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val rate = new CmdOption("rate", 0.18, "DOUBLE", "learning rate")
  val delta = new CmdOption("delta", 0.066, "DOUBLE", "learning delta")
  val modelFile = new CmdOption("model-file", "", "STRING", "Filename of the serialized model that you want to load.")
  val useTagger = new CmdOption("use-tagger", "", "STRING", "Which tagger? (remove me later)")
}


object ConllChainNerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    val opts = new ChainNerOpts
    implicit val random = new scala.util.Random(0)
    opts.parse(args)
    val ner = new ConllChainNer()(ModelProvider.empty, new StaticLexicons()(LexiconsProvider.classpath))
    if (opts.brownClusFile.wasInvoked) {
      println(s"Reading brown cluster file: ${opts.brownClusFile.value}")
      for (line <- scala.io.Source.fromFile(opts.brownClusFile.value).getLines()) {
        val splitLine = line.split("\t")
        ner.clusters(splitLine(1)) = splitLine(0)
      }
    }
    assert(opts.train.wasInvoked && opts.test.wasInvoked, "No train/test data file provided.")
    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value else 1.0
    val trainDocsFull = ner.loadDocs(opts.train.value)
    val testDocsFull = ner.loadDocs(opts.test.value)
    val trainDocs = trainDocsFull.take((trainDocsFull.length*trainPortionToTake).floor.toInt)
    val testDocs = testDocsFull.take((testDocsFull.length*testPortionToTake).floor.toInt)
    println(s"using training set: ${opts.train.value} ; test set: ${opts.test.value}")
    println(s"$trainPortionToTake of training data; $testPortionToTake of test data:")
    println(s"using ${trainDocs.length} / ${trainDocsFull.length} train docs, ${trainDocs.map(_.tokenCount).sum} tokens")
    println(s"using ${testDocs.length} / ${testDocsFull.length} test docs, ${testDocs.map(_.tokenCount).sum} tokens")

    val ret = ner.train(trainDocs, testDocs, opts.rate.value, opts.delta.value)

    if (opts.serialize.value) {
      println("serializing model to " + opts.saveModel.value)
      ner.serialize(new FileOutputStream(opts.saveModel.value))
    }

    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(ret,opts.targetAccuracy.value.toDouble)
    ret
  }
}

object ConllNerOptimizer {
  def main(args: Array[String]) {
    val opts = new ChainNerOpts
    opts.parse(args)
    opts.serialize.setValue(false)
    import cc.factorie.util.LogUniformDoubleSampler
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-12, 1))
    val l2 = cc.factorie.util.HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-12, 1))
    val lr = cc.factorie.util.HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-3, 10))
    val qs = new cc.factorie.util.QSubExecutor(10, "cc.factorie.app.nlp.ner.ConllChainNerTrainer")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, l2, lr), qs.execute, 100, 90, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.serialize.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}
