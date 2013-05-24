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

class NonProjectiveShiftReduce(val mode: Int = 0, val predict: ParseDecisionVariable => ParseDecision = null) {
  import ParserConstants._
  
  var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
  
  def training   = mode == TRAINING
  def predicting = mode == PREDICTING
  def boosting   = mode == BOOSTING

  def clear() = {
    instances.clear()
  }

  def parse(s: Sentence, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]): Array[DepToken] = {
    val state = new ParseState(0, 1, HashSet[Int](), s)
    val depTokens = state.sentenceTokens
    val goldHeads = if (training || boosting) getDepArcs(depTokens, s) else null.asInstanceOf[Seq[DepArc]]
      
    while(state.input < depTokens.length) {
      if (state.stack < 0)
        noShift(state)
      else {
        val label = getDecision(domain, featureDomain, goldHeads, state)
        if (label.left_?) {
          if (state.stack == ROOT_ID) noShift(state)
          else if (state.inputToken(0).isDescendentOf(state.stackToken(0))) noPass(state)
          else if (label.reduce_?) leftReduce(label.label, state)
          else leftPass(label.label, state)
        }
        else if (label.right_?) {
            if (state.stackToken(0).isDescendentOf(state.inputToken(0))) noPass(state)
            else if (label.shift_?) rightShift(label.label, state)
            else rightPass(label.label, state)
        }
        else {
            if (label.shift_?) noShift(state)
            else if (label.reduce_? && state.stackToken(0).hasHead) noReduce(state)
            else noPass(state)
        }
      }
    }
    depTokens
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

  private def noShift(state: ParseState) = shift(state)
  private def noReduce(state: ParseState) = reduce(state)
  private def noPass(state: ParseState) = pass(state)

  private def leftArc(label: String, state: ParseState) {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    lambda.setHead(beta, label)
  }

  private def rightArc(label: String, state: ParseState) {
    val lambda = state.stackToken(0)
    val beta = state.inputToken(0)
    beta.setHead(lambda, label)
  }

  private def shift(state: ParseState) {
    state.stack = state.input
    state.input += 1
  }

  private def reduce(state: ParseState) {
    state.reducedIds.add(state.stack)
    passAux(state)
  }

  private def pass(state: ParseState) = passAux(state: ParseState)

  private def passAux(state: ParseState): Unit = {
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

  private def leftReduce(label: String, state: ParseState) { leftArc(label, state);  reduce(state) }
  private def leftPass(label: String, state: ParseState)   { leftArc(label, state);  pass(state)   }
  private def rightShift(label: String, state: ParseState) { rightArc(label, state); shift(state)  }
  private def rightPass(label: String, state: ParseState)  { rightArc(label, state); pass(state)   }

  private def getDecision(domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String], goldHeads: Seq[DepArc], state: ParseState): ParseDecision = {
    mode match {
      case TRAINING => {
	    val decision = getGoldDecision(goldHeads, state)
	    instances += new ParseDecisionVariable(decision, state, domain, featureDomain)
	    decision
	  }
      case BOOSTING => {
	    val label = new ParseDecisionVariable(getGoldDecision(goldHeads, state), state, domain, featureDomain)
	    instances += label
	    predict(label)
	  }
      case PREDICTING => {
        predict(new ParseDecisionVariable(state, domain, featureDomain))
      }
    }
  }

  def getGoldDecision(goldHeads: Seq[DepArc], state: ParseState): ParseDecision = {
    val label: ParseDecision = getGoldLabelArc(goldHeads, state)
    val shiftOrReduceOrPass =
      label.leftOrRightOrNo match {
        case LEFT  => if (shouldGoldReduce(hasHead=true, goldHeads=goldHeads, state=state)) REDUCE else PASS
        case RIGHT => if (shouldGoldShift(goldHeads, state=state)) SHIFT else PASS
        case _ => {
          if (shouldGoldShift(goldHeads, state=state)) SHIFT
          else if (shouldGoldReduce(hasHead=false, goldHeads=goldHeads, state=state)) REDUCE
          else PASS
        }
      }
    new ParseDecision(label.leftOrRightOrNo + " " + shiftOrReduceOrPass + " " + label.label)
  }

  def getGoldLabelArc(goldHeads: Seq[DepArc], state: ParseState): ParseDecision = {
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

  def shouldGoldShift(goldHeads: Seq[DepArc], state: ParseState): Boolean = {
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

  def shouldGoldReduce(hasHead: Boolean, goldHeads: Seq[DepArc], state: ParseState): Boolean = {
    if (!hasHead && !state.stackToken(0).hasHead)
      return false

    for (i <- (state.input + 1) until state.sentenceTokens.length)
      if (goldHeads(i).depToken.thisIdx == state.stack)
        return false

    true
  }
}
