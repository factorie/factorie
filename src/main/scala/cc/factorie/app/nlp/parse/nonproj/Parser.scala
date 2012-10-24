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
import java.io.File
import java.io.PrintStream
import java.io.BufferedReader
import java.io.FileReader

import collection.GenSeq

/**
 * A non-projective shift-reduce dependency parser based on Jinho Choi's thesis work and ClearNLP.
 *
 * @author Brian Martin
 */

trait ParserClassifier {
  
  def save(file: File, gzip: Boolean = true): Unit
  def load(file: File, gzip: Boolean = true): Unit
  def classify(v: ParseDecisionVariable): ParseDecision
  
}

class BaseParserClassifier(val backingClassifier: ModelBasedClassifier[ParseDecisionVariable]) extends ParserClassifier {
  
  def save(file: File, gzip: Boolean = true): Unit = Serializer.serialize( backingClassifier.model, file, gzip = gzip)
  def load(file: File, gzip: Boolean = true): Unit = throw new Error()
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
  
  def train[B <: ParseDecisionVariable](vs: Seq[B]): ParserClassifier = {
    val backingClassifier = (new SVMTrainer).train(new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](vs, lTof)).asInstanceOf[ModelBasedClassifier[ParseDecisionVariable]]
    new BaseParserClassifier(backingClassifier)
  }
  
}

abstract class TrainParser {
  
  def train[V <: ParseDecisionVariable](vs: Seq[V]): ParserClassifier
  
  //////////////////////////////////////////////////////////
 
  def lTof(l: ParseDecisionVariable) = l.features
  
  def generateDecisions(ss: Seq[Sentence], p: Parser): Seq[ParseDecisionVariable] = {

    val parsers = new ThreadLocal[Parser] { override def initialValue = { val _p = new Parser(mode = p.mode); _p.predict = p.predict; _p }}
    
    val vs = ss.par.zipWithIndex.flatMap { case (s, i) => 
	  if (i % 1000 == 0)
	    println("Parsed: " + i)
	    
      val parser = parsers.get 
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
  
  def boosting(existingClassifier: ParserClassifier, ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable]): ParserClassifier = {
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
      val devFiles =   new CmdOption("dev", List(""), "FILE", "") { override def required = true }
      val ontonotes = new CmdOption("onto", "", "", "")
      val cutoff    = new CmdOption("cutoff", "0", "", "")
      val load      = new CmdOption("load", "", "", "")
      val modelDir =  new CmdOption("model", "model", "DIR", "Directory in which to save the trained model.")
    }
    opts.parse(args)
    import opts._
    
    var loader = LoadConll2008.fromFilename(_)
    if (ontonotes.wasInvoked)
      loader = LoadOntonotes5.fromFilename(_)
    
    val sentences =   trainFiles.value.flatMap(f => loader(f).head.sentences)
    val testSentences = devFiles.value.flatMap(f => loader(f).head.sentences)
    
    println("Total train sentences: " + sentences.size)
    println("Total test sentences: " + testSentences.size)
    
    var modelUrl: String = if (modelDir.wasInvoked) modelDir.value else modelDir.defaultValue + System.currentTimeMillis().toString() + ".parser"
    var modelFile: File = new File(modelUrl)
    
    // generate training (even if predicting -- to fill domain)
    val trainingVs = generateDecisions(sentences, new Parser(0)) 
    
    NonProjParserFeaturesDomain.freeze()
    
	println("# features " + NonProjParserFeaturesDomain.dimensionDomain.size)
    
    if (!load.wasInvoked) {
	  modelFile.createNewFile()
	  
	  val c = train(trainingVs)
      
      println("Train Regular")
      println("------------")
      testAcc(c, sentences)
      println("Test Regular")
      println("------------")
      testAcc(c, testSentences)
      
      val c2 = boosting(c, sentences, trainingVs)
      
      println("Train Boosting")
      println("--------------")
      testAcc(c2, sentences)
      println("Test Boosting")
      println("------------")
      testAcc(c2, testSentences)
      
      c2.save(modelFile, gzip = true)
	  
    }
    else {
      
//      val (m, _) = getEmptyModelAndTemplate(new LabelList[ParseDecisionVariable, NonProjDependencyParserFeatures](Seq(trainingVs.head), lTof))
//      Serializer.deserialize(m, modelFile, gzip = true)
//      
//      val c = new ModelBasedClassifier[ParseDecisionVariable](m, DecisionDomain)     
      
//      testAcc(c, sentences)
//      println("Test")
//      println("------------")
//      testAcc(c, testSentences)
      
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
