package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.WeightsSet
import cc.factorie.optimize._
import cc.factorie.app.classify.{MultiClassModel, LabelList, SVMTrainer}
import cc.factorie.la.{Tensor1, DenseTensor2}
import cc.factorie.app.nlp.{Sentence, LoadOntonotes5, LoadConll2008}
import java.io.File
import cc.factorie.util.BinarySerializer
import cc.factorie.app.nlp.parse.{ParserEval, ParseTree}

/**
 * User: apassos
 * Date: 5/23/13
 * Time: 3:27 PM
 */

class ParserWithTrainer(newTrainer: WeightsSet => Trainer, loss: LinearObjectives.MultiClass) extends Parser {
  val model = new MultiClassModel {
    val evidence = Weights(new DenseTensor2(labelDomain.size, featuresDomain.dimensionDomain.size))
  }
  def classify(v: ParseDecisionVariable) = new ParseDecision(labelDomain.category((model.evidence.value * v.features.tensor.asInstanceOf[Tensor1]).maxIndex))

  def trainFromVariables(vs: Seq[ParseDecisionVariable]) {
    val trainer = newTrainer(model.parameters)
    val examples = vs.map(v => new LinearMultiClassExample(model.evidence, v.features.value.asInstanceOf[Tensor1], v.targetIntValue, loss))
    while (!trainer.isConverged) trainer.processExamples(examples)
  }
  import cc.factorie.util.CubbieConversions._
  def save(file: File, gzip: Boolean) = BinarySerializer.serialize(labelDomain, featuresDomain, model, file, gzip=gzip)
  def load(file: File, gzip: Boolean) = BinarySerializer.deserialize(labelDomain, featuresDomain, model, file, gzip=gzip)
}

object ParserWithTrainer {
  def main(args: Array[String]) = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile =  new CmdOption("train", "", "FILE...", "")
      val testFile =  new CmdOption("test", "", "FILE...", "")
      val devFile =   new CmdOption("dev", "", "FILE", "")
      val modelFile =  new CmdOption("model", "model", "DIR", "Directory in which to save the trained model.")
      val bootstrapping = new CmdOption("bootstrap", "0", "INT", "The number of bootstrapping iterations to do. 0 means no bootstrapping.")
    }
    opts.parse(args)
    import opts._

    val sentences = LoadOntonotes5.fromFilename(trainFile.value).flatMap(_.sentences)
    val devSentences = LoadOntonotes5.fromFilename(devFile.value).flatMap(_.sentences)
    val testSentences = LoadOntonotes5.fromFilename(testFile.value).flatMap(_.sentences)

    println("Total train sentences: " + sentences.size)
    println("Total test sentences: " + testSentences.size)


    def testSingle(c: Parser, ss: Seq[Sentence], extraText: String = ""): Unit = {
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

    def testAll(c: Parser, extraText: String = ""): Unit = {
      println("\n")
      testSingle(c, sentences,     "Train " + extraText)
      testSingle(c, devSentences,  "Dev "   + extraText)
      testSingle(c, testSentences, "Test "  + extraText)
    }

    // Load other parameters
    val numBootstrappingIterations = bootstrapping.value.toInt
    val modelUrl: String = if (modelFile.wasInvoked) modelFile.value else modelFile.defaultValue + System.currentTimeMillis().toString() + ".parser"

    val optimizer = new AdaMira(1.0, 0.01, 1.0) with ParameterAveraging
    val trainer = (w: WeightsSet) => new SynchronizedOptimizerOnlineTrainer(w, optimizer, maxIterations = 4)
    val loss = LinearObjectives.sparseLogMultiClass

    val c = new ParserWithTrainer(trainer, loss)
    var trainingVs = c.generateDecisions(sentences, 0)
    c.featuresDomain.freeze()
    println("# features " + c.featuresDomain.dimensionDomain.size)
    c.trainFromVariables(trainingVs)
    // save the initial model
    println("Saving the model...")
    c.save(new File(modelUrl), gzip = true)
    println("...DONE")
    testAll(c)

    println("Loading it back for serialization testing...")
    val d = new ParserWithTrainer(trainer, loss)
    d.load(new File(modelUrl), gzip = true)
    testAll(d)
    trainingVs = null // GC the old training labels
    for (i <- 0 until numBootstrappingIterations) {
      c.boosting(sentences)
      testAll(c, "Boosting" + i)
      // save the model
      val modelFile = new File(modelUrl + "-bootstrap-iter=" + i)
      c.save(modelFile, gzip = true)
    }
  }
}

