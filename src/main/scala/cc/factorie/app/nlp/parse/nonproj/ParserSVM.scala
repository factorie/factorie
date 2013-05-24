package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.nlp.{Sentence, LoadOntonotes5, LoadConll2008}
import java.io.File
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.{ParseDecision, NonProjDependencyParserFeatures, ParseDecisionVariable}
import cc.factorie.app.classify.{LogLinearTemplate2, ModelBasedClassifier, LabelList, SVMTrainer}
import cc.factorie.TemplateModel
import cc.factorie.util.BinarySerializer
import cc.factorie.app.nlp.parse.{ParserEval, ParseTree}

/**
 * User: apassos
 * Date: 5/23/13
 * Time: 3:26 PM
 */


class ParserSVM extends Parser {
  val parserClassifier = new ModelBasedClassifier[ParseDecisionVariable, TemplateModel](new TemplateModel {
      addTemplates(new LogLinearTemplate2[ParseDecisionVariable, NonProjDependencyParserFeatures](this, lTof, labelDomain, featuresDomain))
  }, labelDomain)

  def lTof(l: ParseDecisionVariable) = l.features
  import cc.factorie.util.CubbieConversions._
  def save(file: File, gzip: Boolean) = BinarySerializer.serialize(labelDomain, featuresDomain, parserClassifier.model, file, gzip=gzip)
  def load(file: File, gzip: Boolean) = BinarySerializer.deserialize(labelDomain, featuresDomain, parserClassifier.model, file, gzip=gzip)
  def classify(v: ParseDecisionVariable): ParseDecision = new ParseDecision(labelDomain.category(parserClassifier.classify(v).bestLabelIndex))
  def trainFromVariables(vs: Seq[ParseDecisionVariable]) {
    SVMTrainer.train(parserClassifier.model, parallel = true,
      new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](vs, lTof))
  }
}

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
        ss.foreach(c.process)
        val pred = ss.map(_.attr[ParseTree])
        println("LAS: " + ParserEval.calcLas(pred))
        println("UAS: " + ParserEval.calcUas(pred))
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
      var trainingVs = c.generateDecisions(sentences, 0)
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

