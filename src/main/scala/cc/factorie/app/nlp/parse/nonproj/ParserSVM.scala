package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.classify.LogLinearTemplate2
import cc.factorie.app.classify.ModelBasedClassifier
import cc.factorie.app.classify.SVMTrainer
import cc.factorie.app.classify.LabelList
import cc.factorie.TemplateModel
import cc.factorie.Model
import ParserSupport._
import ParserConstants._
import java.io.File
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.parse.ParseTreeLabel
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain
import cc.factorie._
import cc.factorie.app.nlp.pos.PTBPosLabel
import scala.collection.mutable.ListBuffer
import cc.factorie.util.BinarySerializer

object ParserSVM {
  def main(args: Array[String]) = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFiles =  new CmdOption("train", List(""), "FILE...", "")
      val testFiles =  new CmdOption("test", List(""), "FILE...", "")
      val devFiles =   new CmdOption("dev", List(""), "FILE", "")
      val ontonotes = new CmdOption("onto", "", "", "")
      val cutoff    = new CmdOption("cutoff", "0", "", "")
      val loadModel = new CmdOption("load", "", "", "")
      val modelDir =  new CmdOption("model", "model", "DIR", "Directory in which to save the trained model.")
      val bootstrapping = new CmdOption("bootstrap", "0", "INT", "The number of bootstrapping iterations to do. 0 means no bootstrapping.")
    }
    opts.parse(args)
    import opts._


    // Load the sentences
    var loader = LoadConll2008.fromFilename(_)
    if (ontonotes.wasInvoked)
      loader = LoadOntonotes5.fromFilename(_)

    def loadSentences(o: CmdOption[List[String]]): Seq[Sentence] = {
      if (o.wasInvoked) o.value.flatMap(f => loader(f).head.sentences)
      else Seq.empty[Sentence]
    }

    val sentences = loadSentences(trainFiles)
    val devSentences = loadSentences(devFiles)
    val testSentences = loadSentences(testFiles)

    println("Total train sentences: " + sentences.size)
    println("Total test sentences: " + testSentences.size)


    def testSingle(c: ParserSVM, ss: Seq[Sentence], extraText: String = ""): Unit = {
      if (ss.nonEmpty) {
	    println(extraText)
	    println("------------")
	    c.testAcc(ss)
	    println("\n")
      }
    }

    def testAll(c: ParserSVM, extraText: String = ""): Unit = {
      println("\n")
      testSingle(c, sentences,     "Train " + extraText)
      testSingle(c, devSentences,  "Dev "   + extraText)
      testSingle(c, testSentences, "Test "  + extraText)
    }

    // Load other parameters
    val numBootstrappingIterations = bootstrapping.value.toInt

    val modelUrl: String = if (modelDir.wasInvoked) modelDir.value else modelDir.defaultValue + System.currentTimeMillis().toString() + ".parser"
    var modelFolder: File = new File(modelUrl)

    // Do training if we weren't told to load a model
    if (!loadModel.wasInvoked) {
      val c = new ParserSVM

      var trainingVs = c.generateDecisions(sentences, new ParserAlgorithm(0))
      c.featuresDomain.freeze()
      println("# features " + c.featuresDomain.dimensionDomain.size)

      c.trainFromVariables(trainingVs)

      // save the initial model
      println("Saving the model...")
      c.save(modelFolder, gzip = true)
      println("...DONE")

      testAll(c)

      println("Loading it back for serialization testing...")
      val d = new ParserSVM
      d.load(modelFolder, gzip = true)

      testAll(d)

      trainingVs = null // GC the old training labels

      for (i <- 0 until numBootstrappingIterations) {
        c.boosting(sentences)

        testAll(c, "Boosting" + i)

        // save the model
        modelFolder = new File(modelUrl + "-bootstrap-iter=" + i)
        modelFolder.mkdir()
        c.save(modelFolder, gzip = true)
      }

    }
    else {
      val c = new ParserSVM
      c.load(modelFolder, gzip = true)
      testAll(c)
    }

  }
}

class ParserSVM extends DocumentAnnotator {
  object labelDomain extends CategoricalDomain[String]
  labelDomain += ParserSupport.defaultCategory
  object featuresDomain extends CategoricalDimensionTensorDomain[String]

  def lTof(l: ParseDecisionVariable) = l.features
  val parserClassifier = new ModelBasedClassifier[ParseDecisionVariable, TemplateModel](new TemplateModel {
      addTemplates(new LogLinearTemplate2[ParseDecisionVariable, NonProjDependencyParserFeatures](this, lTof, labelDomain, featuresDomain))
  }, labelDomain)

  def save(file: File, gzip: Boolean) = BinarySerializer.serialize(labelDomain, featuresDomain, parserClassifier.model, file, gzip=gzip)
  def load(file: File, gzip: Boolean) = BinarySerializer.deserialize(labelDomain, featuresDomain, parserClassifier.model, file, gzip=gzip)
  def classify(v: ParseDecisionVariable): ParseDecision = new ParseDecision(labelDomain.category(parserClassifier.classify(v).bestLabelIndex))

  def generateDecisions(ss: Seq[Sentence], p: ParserAlgorithm): Seq[ParseDecisionVariable] = {
    var i = 0
    val vs = ss.flatMap { s =>
      i += 1
      if (i % 1000 == 0)
        println("Parsed: " + i)
      val parser = new ParserAlgorithm(mode = p.mode); parser.predict = p.predict;
      parser.clear()
      parser.parse(s, labelDomain, featuresDomain)
      parser.instances
    }
    vs
  }

  def boosting(ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]) {
    val p = new ParserAlgorithm(mode = 2)
    p.predict = (v: ParseDecisionVariable) => { classify(v) }
    val newVs = generateDecisions(ss, p)
    trainFromVariables(addlVs ++ newVs)
  }

  def predict(ss: Seq[Sentence], parallel: Boolean = true): (Seq[Seq[(Int, String)]], Seq[Seq[(Int, String)]]) = {
    val p = new ParserAlgorithm(mode = 1)
    p.predict = (v: ParseDecisionVariable) => { classify(v) }
    val parsers = new ThreadLocal[ParserAlgorithm] { override def initialValue = { val _p = new ParserAlgorithm(mode = p.mode); _p.predict = p.predict; _p }}
    val (gold, pred) = ss.zipWithIndex.map({ case (s, i) =>
      if (i % 1000 == 0)
        println("Parsed: " + i)
      val parser = parsers.get
      parser.clear()
      val gold = parser.getSimpleDepArcs(s)
      parser.clear()
      val dts = parser.parse(s, labelDomain, featuresDomain)
      p.clear()
      val pred = (dts.drop(1).map { dt =>
        if (dt.hasHead) dt.head.depToken.thisIdx -> dt.head.label
        else -1 -> null.asInstanceOf[String]
      } toSeq)

      (gold, pred)

    }).foldLeft(new ListBuffer[Seq[(Int, String)]], new ListBuffer[Seq[(Int, String)]])({ case (prev, curr) =>
      prev._1 append curr._1
      prev._2 append curr._2
      prev
    })

    (gold.toSeq, pred.toSeq)
  }

  def testAcc(ss: Seq[Sentence]): Unit = {
    val (gold, pred) = predict(ss)
    println("LAS: " + ParserEval.calcLas(gold, pred))
    println("UAS: " + ParserEval.calcUas(gold, pred))
  }

  def trainFromSentences(ss: Seq[Sentence]) {
    val p = new ParserAlgorithm(mode = 0)
    val vs = generateDecisions(ss, p)
    trainFromVariables(vs)
  }
  def trainFromVariables(vs: Seq[ParseDecisionVariable]) {
    SVMTrainer.train(parserClassifier.model, parallel = true,
      new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](vs, lTof))
  }
  
  def process1(doc: Document) = { doc.sentences.foreach(process(_)); doc }
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PTBPosLabel])
  def postAttrs = Seq(classOf[ParseTree])

  def process(s: Sentence): Sentence = {
    val p = new ParserAlgorithm(mode = PREDICTING)
    p.predict = classify
    val parse = new ParseTree(s)
    p.parse(s, labelDomain, featuresDomain).drop(1).filter(_.hasHead).map { dt =>
      parse.setParent(dt.thisIdx - 1, dt.head.depToken.thisIdx - 1)
      // TODO: why is this necessary? Shouldn't I be able to do set(s: String)?
      parse.label(dt.thisIdx - 1).set(ParseTreeLabelDomain.index(dt.head.label))(null)
    }
    s.attr += parse
    s
  }

}
