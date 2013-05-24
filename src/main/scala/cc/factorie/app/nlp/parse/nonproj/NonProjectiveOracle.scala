package cc.factorie.app.nlp.parse.nonproj

import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.parse.nonproj.ParserSupport._
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.ParserConstants._
import cc.factorie.{CategoricalDimensionTensorDomain, CategoricalDomain}
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.ParseDecision
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.DepArc

/**
 * User: apassos
 * Date: 5/24/13
 * Time: 12:52 PM
 */


trait NonProjectiveOracle {
  val sentence: Sentence
  def predict(state: ParseDecisionVariable): ParseDecision

  var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }

  def getSimpleDepArcs = sentence.parse.parents.map(_ + 1).zip(sentence.parse.labels.map(_.value.category))
  def getDepArcs = {
    Seq((-1, "<ROOT-ROOT>")) ++
	    getSimpleDepArcs.map {
	      case (i: Int, l: String) => (i, l)
	    }
  }
  val goldHeads = getDepArcs

  def getGoldDecision(state: ParseState): ParseDecision = {
    val label: ParseDecision = getGoldLabelArc(state)
    val shiftOrReduceOrPass =
      label.leftOrRightOrNo match {
        case LEFT  => if (shouldGoldReduce(hasHead=true, state=state)) REDUCE else PASS
        case RIGHT => if (shouldGoldShift(state=state)) SHIFT else PASS
        case _ => {
          if (shouldGoldShift(state=state)) SHIFT
          else if (shouldGoldReduce(hasHead=false, state=state)) REDUCE
          else PASS
        }
      }
    new ParseDecision(label.leftOrRightOrNo + " " + shiftOrReduceOrPass + " " + label.label)
  }

  def getGoldLabelArc(state: ParseState): ParseDecision = {
    val headIdx   = goldHeads(state.stack)._1
    val headLabel = goldHeads(state.stack)._2
    val inputHeadIdx   = goldHeads(state.input)._1
    val inputHeadLabel = goldHeads(state.input)._2
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

  def shouldGoldShift(state: ParseState): Boolean = {
    // if the head of the input is to the left of the stack: don't shift
    if (goldHeads(state.input)._1 < state.stack)
      return false
    // if the head of any token in the stack is the input: don't shift
    else
      for (i <- (state.stack - 1) until 0 by -1) if (!state.reducedIds.contains(i)) {
        if (goldHeads(i)._1 == state.input)
          return false
      }
    // else shift
    true
  }

  def shouldGoldReduce(hasHead: Boolean, state: ParseState): Boolean = {
    if (!hasHead && !state.stackToken(0).hasHead)
      return false

    for (i <- (state.input + 1) until state.sentenceTokens.length)
      if (goldHeads(i)._1 == state.stack)
        return false

    true
  }
}

class NonprojectiveGoldOracle(val sentence: Sentence, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) extends NonProjectiveOracle {
  def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
    val decision = getGoldDecision(decisionVariable.state)
    instances += new ParseDecisionVariable(decision, decisionVariable.state, domain, featureDomain)
    decision
  }
}

class NonprojectiveBoostingOracle(val sentence: Sentence, basePredict: ParseDecisionVariable => ParseDecision, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) extends NonProjectiveOracle {
  def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
    val label = new ParseDecisionVariable(getGoldDecision(decisionVariable.state), decisionVariable.state, domain, featureDomain)
    instances += label
    basePredict(label)
  }
}