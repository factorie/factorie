package cc.factorie.app.nlp.parse.nonproj

import cc.factorie._
import cc.factorie.optimize.LinearL2SVM
import cc.factorie.la.{WeightsTensor, Tensor1}
import cc.factorie.app.classify._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.nonproj.ParserSupport._
import collection.mutable.ListBuffer
import collection.mutable.HashSet
import collection.mutable.ArrayBuffer
import io.Source
import java.io.File
import java.io.PrintStream
import java.io.PrintWriter
import java.io.BufferedReader
import java.io.FileReader

import collection.GenSeq

/**
 * A non-projective shift-reduce dependency parser based on Jinho Choi's thesis work and ClearNLP.
 *
 * @author Brian Martin
 */

// An abstraction which allows for easily changing the predictor
trait ParserClassifier {
  def classify(v: ParseDecisionVariable): ParseDecision
}

// The standard SVM one-vs-all classifier
// The interface here is simpler than it seems: we're only saving, loading, and classifying.
// To clean this up, we need better serialization support.
class BaseParserClassifier(val backingClassifier: ModelBasedClassifier[ParseDecisionVariable]) extends ParserClassifier {
  
  private var _gzip = false
  
  private def saveModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    BinaryFileSerializer.serialize(backingClassifier.model, file, gzip = _gzip)
  }
  
  private def loadModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    BinaryFileSerializer.deserialize(backingClassifier.model, file, gzip = _gzip)
  }
  
  private def saveLabelDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "label-domain")
    val pw = new PrintWriter(file)
    for (c <- DecisionDomain.categories.drop(1)) // drop the default category
      pw.println("%d %d %s" format (c.leftOrRightOrNo, c.shiftOrReduceOrPass, c.label))
    pw.close()
  }
  
  val DecisionLine = """(-?\d) (-?\d) (.*)""".r
  private def loadLabelDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "label-domain")
    for (l <- Source.fromFile(file).getLines())
      l match { case DecisionLine(leftRight, shiftReduce, label) => DecisionDomain.index(new ParseDecision(leftRight.toInt, shiftReduce.toInt, label)) }
    DecisionDomain.freeze()
  }
  
  private def saveFeatureDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "feat-domain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
  }
  
  private def loadFeatureDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "feat-domain")
    BinaryCubbieFileSerializer.deserialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
    NonProjParserFeaturesDomain.freeze()
  }
  
  def save(folder: File, gzip: Boolean): Unit = { 
    _gzip = gzip
    saveModel(folder)
    saveLabelDomain(folder)
    saveFeatureDomain(folder)
  }
  
  def load(folder: File, gzip: Boolean): Unit = { 
    _gzip = gzip
    loadLabelDomain(folder)
    loadFeatureDomain(folder)
    loadModel(folder)
  }
  
  def classify(v: ParseDecisionVariable): ParseDecision =
    DecisionDomain.category(backingClassifier.classify(v).bestLabelIndex)
  
}


object TrainParserSVM {
  def main(args: Array[String]): Unit = {
    val trainer = new TrainParserSVM()
    trainer.run(args)
  }
}
class TrainParserSVM extends TrainParser {
  
  def getEmptyModel(): Model = new TemplateModel(new LogLinearTemplate2[ParseDecisionVariable, NonProjDependencyParserFeatures](lTof, DecisionDomain, NonProjParserFeaturesDomain))
  
  def save(c: ParserClassifier, folder: File, gzip: Boolean): Unit = c.asInstanceOf[BaseParserClassifier].save(folder, gzip)
  
  def load(folder: File, gzip: Boolean): ParserClassifier = {
    val c = new BaseParserClassifier(new ModelBasedClassifier(getEmptyModel(), DecisionDomain))
    c.load(folder, gzip)
    c
  }
  
  def train[B <: ParseDecisionVariable](vs: Seq[B]): ParserClassifier = {
    val backingClassifier = (new SVMTrainer).train(new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](vs, lTof)).asInstanceOf[ModelBasedClassifier[ParseDecisionVariable]]
    new BaseParserClassifier(backingClassifier)
  }
  
}

abstract class TrainParser {
  
  def train[V <: ParseDecisionVariable](vs: Seq[V]): ParserClassifier
  
  def save(c: ParserClassifier, folder: File, gzip: Boolean = true): Unit
  
  def load(file: File, gzip: Boolean = true): ParserClassifier
  
  //////////////////////////////////////////////////////////
 
  def lTof(l: ParseDecisionVariable) = l.features
  
  def generateDecisions(ss: Seq[Sentence], p: Parser): Seq[ParseDecisionVariable] = {

    var i = 0
    val vs = ss.par.flatMap { s => 
      i += 1
	  if (i % 1000 == 0)
	    println("Parsed: " + i)
      val parser = new Parser(mode = p.mode); parser.predict = p.predict;
	  parser.clear()
      parser.parse(s)
	  parser.instances
    } seq

    vs
  }
  
  def train[A <: Sentence](ss: Seq[A])(implicit s: DummyImplicit): ParserClassifier = { 
    var p = new Parser(mode = 0)
    val vs = generateDecisions(ss, p)
    train(vs)
  }
  
  def boosting(existingClassifier: ParserClassifier, ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]): ParserClassifier = {
    var p = new Parser(mode = 2)
    p.predict = (v: ParseDecisionVariable) => { existingClassifier.classify(v) }
    var newVs = generateDecisions(ss, p)
    train(addlVs ++ newVs)
  }
  
  def predict(c: ParserClassifier, ss: Seq[Sentence], parallel: Boolean = true): (Seq[Seq[(Int, String)]], Seq[Seq[(Int, String)]]) = {
    val p = new Parser(mode = 1)
    p.predict = (v: ParseDecisionVariable) => { c.classify(v) }
    
    val parsers = new ThreadLocal[Parser] { override def initialValue = { val _p = new Parser(mode = p.mode); _p.predict = p.predict; _p }}
    
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
	  
      var trainingVs = generateDecisions(sentences, new Parser(0)) 
      NonProjParserFeaturesDomain.freeze()
	  println("# features " + NonProjParserFeaturesDomain.dimensionDomain.size)
	  
	  var c = train(trainingVs)
	  
	  // save the initial model
	  println("Saving the model...")
	  save(c, modelFolder, gzip = true)
	  println("...DONE")
      
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

class Parser(var mode: Int = 0) {
  
  import ParserConstants._
  
  var predict: ParseDecisionVariable => ParseDecision = null
  
  var goldHeads: Seq[DepArc] = null
  var state: ParseState = null
  var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
  
  var debug = false
  
  val TRAINING   = 0
  val PREDICTING = 1
  val BOOSTING   = 2

  def training   = mode == TRAINING
  def predicting = mode == PREDICTING
  def boosting   = mode == BOOSTING

  def clear(): Unit = {
    goldHeads = null
    state = null
    instances.clear()
  }

  def parse(s: Sentence): Array[DepToken] = {
    
    val depTokens = getDepTokens(s)
    
    state = new ParseState(0, 1, new HashSet[Int], depTokens)
    
    if (training || boosting)
      goldHeads = getDepArcs(depTokens, s)
      
    var lastAction = null

    while(state.input < depTokens.length) {
      if (debug)
	      println()
	      
      if (state.stack < 0)
        noShift()
      else {
        val label = getDecision()
        
//        if(debug)
//	        println(instances.last.features.activeCategories.mkString("\n"))
        //println(label, label.shift_?, label.reduce_?, label.pass_?)

        if (label.left_?) {
          if (state.stack == ROOT_ID)
            noShift()
          else if (state.inputToken(0).isDescendentOf(state.stackToken(0)))
            noPass()
          else if (label.reduce_?)
            leftReduce(label.label)
          else
            leftPass(label.label)
        }
        else if (label.right_?) {
            if (state.stackToken(0).isDescendentOf(state.inputToken(0)))
                noPass()
            else if (label.shift_?)
                rightShift(label.label)
            else
                rightPass(label.label)
        }
        else {
            if (label.shift_?)
                noShift()
            else if (label.reduce_? && state.stackToken(0).hasHead)
                noReduce()
            else
                noPass()
        }

      }
    }
    
    return depTokens
  }

  def getDepTokens(s: Sentence): Array[DepToken] = {
    val a = Array.ofDim[DepToken](s.length + 1)
    a(0) = DepToken.root
    for ((t, i) <- s.links.map(DepToken(_)).zipWithIndex) {
      a(i+1) = t
      a(i+1).state = state
      a(i+1).thisIdx = i + 1
    }
    a
  }

  def setTokenHeads(ts: Array[DepToken], as: Seq[DepArc]): Unit =
    ts.zip(as).foreach { case (t: DepToken, a: DepArc) => t.head = a }
  
  def getSimpleDepArcs(s: Sentence): Seq[(Int, String)] = {
    s.parse.parents.map(_ + 1).zip(s.parse.labels.map(_.value.category))
  }
  
  def getDepArcs(ts: Array[DepToken], s: Sentence): Seq[DepArc] = {
    Seq(new DepArc(ts(0), "<ROOT-ROOT>")) ++
	    getSimpleDepArcs(s).map { 
	      case (i: Int, l: String) => new DepArc(ts(i), l)
	    }
  }

  private def noShift() = shift()
  private def noReduce() = reduce()
  private def noPass() = pass()

  private def leftArc(label: String) = {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    lambda.setHead(beta, label)
  }

  private def rightArc(label: String) {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    beta.setHead(lambda, label)
  }

  private def shift() = {
    debugPrint("Shift")
    state.stack = state.input
    state.input += 1
  }

  private def reduce() = {
    debugPrint("Reduce")
    state.reducedIds.add(state.stack)
    passAux()
  }

  private def pass() = passAux()

  private def passAux(): Unit = {
    debugPrint("Pass")
    var i = state.stack - 1
    while (i >= 0) {
      if (!state.reducedIds.contains(i)) {
          state.stack = i
          return
      }
      i -= 1
    }

    state.stack = i
  }

  private def leftReduce(label: String) { debugPrint("LeftReduce");   leftArc(label);  reduce() }
  private def leftPass(label: String)   { debugPrint("LeftPass");  leftArc(label);  pass()   }
  private def rightShift(label: String) { debugPrint("RightShift"); rightArc(label); shift()  }
  private def rightPass(label: String)  { debugPrint("RightPass");  rightArc(label); pass()   }

  private def getDecision(): ParseDecision = {
    mode match {
      case TRAINING => {
	    val decision = getGoldDecision()
	    instances += new ParseDecisionVariable(decision, state)
	    decision
	  }
      case BOOSTING => {
	    val label = new ParseDecisionVariable(getGoldDecision(), state)
	    instances += label
	    predict(label)
	  }
      case PREDICTING => {
        predict(new ParseDecisionVariable(state))
      }
    }
  }

  def getGoldDecision(): ParseDecision = {

    val label: ParseDecision = getGoldLabelArc()

    label.shiftOrReduceOrPass =
      label.leftOrRightOrNo match {
        case LEFT  => if (shouldGoldReduce(true)) REDUCE else PASS
        case RIGHT => if (shouldGoldShift)        SHIFT  else PASS
        case _ => {
          if (shouldGoldShift) SHIFT
          else if (shouldGoldReduce(false)) REDUCE
          else PASS
        }
      }

    label
  }

  def getGoldLabelArc(): ParseDecision = {

    val headIdx   = goldHeads(state.stack).depToken.thisIdx
    val headLabel = goldHeads(state.stack).label

    // if beta is the head of lambda
    if (headIdx == state.input)
      return new ParseDecision(LEFT, -1, headLabel)

    val inputHeadIdx   = goldHeads(state.input).depToken.thisIdx
    val inputHeadLabel = goldHeads(state.input).label

    // if lambda is the head of beta
    if (inputHeadIdx == state.stack)
      return new ParseDecision(RIGHT, -1, inputHeadLabel)

    // lambda doesn't have a head
    return new ParseDecision(NO, -1, "")
  }

  def shouldGoldShift: Boolean = {
    // if the head of the input is to the left of the stack: don't shift
    if (goldHeads(state.input).depToken.thisIdx < state.stack)
      return false
    // if the head of any token in the stack is the input: don't shift
    else
      for (i <- (state.stack - 1) until 0 by -1) if (!state.reducedIds.contains(i)) {
        if (goldHeads(i).depToken.thisIdx == state.input)
          return false
      }

    // else shift
    true
  }

  def shouldGoldReduce(hasHead: Boolean): Boolean = {

    if (!hasHead && !state.stackToken(0).hasHead)
      return false

    for (i <- (state.input + 1) until state.sentenceTokens.length)
      if (goldHeads(i).depToken.thisIdx == state.stack)
        return false

    true
  }
  
  private def debugPrint(str: String): Unit = {
    if (debug)
	  println(str + " " + state)
  }

}
