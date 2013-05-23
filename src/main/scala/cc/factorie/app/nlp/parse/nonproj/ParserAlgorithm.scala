package cc.factorie.app.nlp.parse.nonproj

import cc.factorie._
import cc.factorie.optimize.LinearL2SVM
import cc.factorie.la.{Tensor1}
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

class ParserAlgorithm(val mode: Int = 0, val predict: ParseDecisionVariable => ParseDecision = null) {
  import ParserConstants._
  
  var goldHeads: Seq[DepArc] = null
  var state: ParseState = null
  var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
  
  var debug = false

  def training   = mode == TRAINING
  def predicting = mode == PREDICTING
  def boosting   = mode == BOOSTING

  def clear() = {
    goldHeads = null
    state = null
    instances.clear()
  }

  def parse(s: Sentence, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]): Array[DepToken] = {
    val depTokens = getDepTokens(s)
    state = new ParseState(0, 1, HashSet[Int](), depTokens)
    
    if (training || boosting)
      goldHeads = getDepArcs(depTokens, s)
      
    while(state.input < depTokens.length) {
      if (state.stack < 0)
        noShift()
      else {
        val label = getDecision(domain, featureDomain)
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
    depTokens
  }

  def getDepTokens(s: Sentence): Array[DepToken] = {
    val a = Array.ofDim[DepToken](s.length + 1)
    a(0) = DepToken.root
    s.tokens.zipWithIndex.foreach(i => a(i._2+1) = DepToken(i._1, i._2+1, state))
    a
  }

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

  private def leftArc(label: String) {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    lambda.setHead(beta, label)
  }

  private def rightArc(label: String) {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    beta.setHead(lambda, label)
  }

  private def shift() {
    debugPrint("Shift")
    state.stack = state.input
    state.input += 1
  }

  private def reduce() {
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

  private def getDecision(domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]): ParseDecision = {
    mode match {
      case TRAINING => {
	    val decision = getGoldDecision
	    instances += new ParseDecisionVariable(decision, state, domain, featureDomain)
	    decision
	  }
      case BOOSTING => {
	    val label = new ParseDecisionVariable(getGoldDecision, state, domain, featureDomain)
	    instances += label
	    predict(label)
	  }
      case PREDICTING => {
        predict(new ParseDecisionVariable(state, domain, featureDomain))
      }
    }
  }

  def getGoldDecision: ParseDecision = {
    val label: ParseDecision = getGoldLabelArc
    val shiftOrReduceOrPass =
      label.leftOrRightOrNo match {
        case LEFT  => if (shouldGoldReduce(hasHead=true)) REDUCE else PASS
        case RIGHT => if (shouldGoldShift)        SHIFT  else PASS
        case _ => {
          if (shouldGoldShift) SHIFT
          else if (shouldGoldReduce(hasHead=false)) REDUCE
          else PASS
        }
      }
    new ParseDecision(label.leftOrRightOrNo + " " + shiftOrReduceOrPass + " " + label.label)
  }

  def getGoldLabelArc: ParseDecision = {
    val headIdx   = goldHeads(state.stack).depToken.thisIdx
    val headLabel = goldHeads(state.stack).label
    val inputHeadIdx   = goldHeads(state.input).depToken.thisIdx
    val inputHeadLabel = goldHeads(state.input).label
    // if beta is the head of lambda
    if (headIdx == state.input) {
       new ParseDecision(LEFT + " " + (-1) + " " + headLabel)
    } else if (inputHeadIdx == state.stack) {
      new ParseDecision(RIGHT + " " + (-1) + " " + inputHeadLabel)
    } else {
      // lambda doesn't have a head
      new ParseDecision(NO + " " + (-1) + " N")
    }
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
