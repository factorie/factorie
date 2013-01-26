package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.nlp._
import ParserSupport._
import java.io.File

import scala.collection.mutable.ListBuffer

abstract class Parser {
  
  def train[V <: ParseDecisionVariable](vs: Seq[V]): ParserClassifier
  
  def save(c: ParserClassifier, folder: File, gzip: Boolean = true): Unit
  
  def load(file: File, gzip: Boolean = true): ParserClassifier
  
  //def process(c: ParserClassifier, doc: Document): Document
  
}
  
trait ParserImpl {
  this: Parser => 
 
  def lTof(l: ParseDecisionVariable) = l.features
  
  def generateDecisions(ss: Seq[Sentence], p: ParserAlgorithm): Seq[ParseDecisionVariable] = {

    var i = 0
    val vs = ss.par.flatMap { s => 
      i += 1
	  if (i % 1000 == 0)
	    println("Parsed: " + i)
      val parser = new ParserAlgorithm(mode = p.mode); parser.predict = p.predict;
	  parser.clear()
      parser.parse(s)
	  parser.instances
    } seq

    vs
  }
  
  def train[A <: Sentence](ss: Seq[A])(implicit s: DummyImplicit): ParserClassifier = { 
    var p = new ParserAlgorithm(mode = 0)
    val vs = generateDecisions(ss, p)
    train(vs)
  }
  
  def boosting(existingClassifier: ParserClassifier, ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]): ParserClassifier = {
    var p = new ParserAlgorithm(mode = 2)
    p.predict = (v: ParseDecisionVariable) => { existingClassifier.classify(v) }
    var newVs = generateDecisions(ss, p)
    train(addlVs ++ newVs)
  }
  
  def predict(c: ParserClassifier, ss: Seq[Sentence], parallel: Boolean = true): (Seq[Seq[(Int, String)]], Seq[Seq[(Int, String)]]) = {
    val p = new ParserAlgorithm(mode = 1)
    p.predict = (v: ParseDecisionVariable) => { c.classify(v) }
    
    val parsers = new ThreadLocal[ParserAlgorithm] { override def initialValue = { val _p = new ParserAlgorithm(mode = p.mode); _p.predict = p.predict; _p }}
    
    val (gold, pred) = ss.par.zipWithIndex.map({ case (s, i) => 
	  if (i % 1000 == 0)
	    println("Parsed: " + i)
	    
	  val parser = parsers.get
	  parser.clear()
	  
	  val gold = parser.getSimpleDepArcs(s)
	  parser.clear()

      val dts = parser.parse(s)
	  p.clear()
	  var pred = (dts.drop(1).map { dt => 
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

  def testAcc(c: ParserClassifier, ss: Seq[Sentence]): Unit = {
    val (gold, pred) = predict(c, ss)
    println("LAS: " + ParserEval.calcLas(gold, pred))
    println("UAS: " + ParserEval.calcUas(gold, pred))
  }
  
  def run(args: Array[String]) = {
    
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
    
    
    def testSingle(c: ParserClassifier, ss: Seq[Sentence], extraText: String = ""): Unit = {
      if (ss.nonEmpty) {
	    println(extraText)
	    println("------------")
	    testAcc(c, ss)
	    println("\n")
      }
    }
    
    def testAll(c: ParserClassifier, extraText: String = ""): Unit = {
      println("\n")
      testSingle(c, sentences,     "Train " + extraText)
      testSingle(c, devSentences,  "Dev "   + extraText)
      testSingle(c, testSentences, "Test "  + extraText)
    }
    
    // Load other parameters
    val numBootstrappingIterations = bootstrapping.value.toInt
    
    var modelUrl: String = if (modelDir.wasInvoked) modelDir.value else modelDir.defaultValue + System.currentTimeMillis().toString() + ".parser"
    var modelFolder: File = new File(modelUrl)
    
    // Do training if we weren't told to load a model
    if (!loadModel.wasInvoked) {
	  modelFolder.mkdir()
	  
      var trainingVs = generateDecisions(sentences, new ParserAlgorithm(0)) 
      NonProjParserFeaturesDomain.freeze()
	  println("# features " + NonProjParserFeaturesDomain.dimensionDomain.size)
	  
	  var c = train(trainingVs)
	  
	  // save the initial model
	  println("Saving the model...")
	  save(c, modelFolder, gzip = true)
	  println("...DONE")
	  
	  testAll(c)
	  
	  println("Loading it back for serialization testing...")
	  c = load(modelFolder, gzip = true)
	  
	  testAll(c)
	  
      trainingVs = null // GC the old training labels
      
      for (i <- 0 until numBootstrappingIterations) {
        val cNew = boosting(c, sentences)
      
        testAll(cNew, "Boosting" + i)
      
        // save the model
        modelFolder = new File(modelUrl + "-bootstrap-iter=" + i)
        modelFolder.mkdir()
        save(cNew, modelFolder, gzip = true)
        
        c = cNew
      }
	  
    }
    else {
      val c = load(modelFolder, gzip = true)
      testAll(c)
    }
    
  }
  
}
