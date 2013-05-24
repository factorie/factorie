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
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.ParserConstants._
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.ParseDecision
import cc.factorie.app.nlp.parse.nonproj.ParserSupport.DepArc

/**
 * A non-projective shift-reduce dependency parser based on Jinho Choi's thesis work and ClearNLP.
 *
 * @author Brian Martin
 */

class NonProjectiveShiftReduce(val predict: ParseDecisionVariable => ParseDecision) {
  import ParserConstants._
  
  def parse(s: Sentence, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]): Array[DepToken] = {
    val state = new ParseState(0, 1, HashSet[Int](), s)
    while(state.input < state.sentenceTokens.length) {
      if (state.stack < 0)
        noShift(state)
      else {
        val label = predict(new ParseDecisionVariable(state, domain, featureDomain))
        if (label.leftOrRightOrNo == LEFT) {
          if (state.stack == ROOT_ID) noShift(state)
          else if (state.inputToken(0).isDescendentOf(state.stackToken(0))) noPass(state)
          else if (label.shiftOrReduceOrPass == REDUCE) leftReduce(label.label, state)
          else leftPass(label.label, state)
        }
        else if (label.leftOrRightOrNo == RIGHT) {
            if (state.stackToken(0).isDescendentOf(state.inputToken(0))) noPass(state)
            else if (label.shiftOrReduceOrPass == SHIFT) rightShift(label.label, state)
            else rightPass(label.label, state)
        }
        else {
            if (label.shiftOrReduceOrPass == SHIFT) noShift(state)
            else if (label.shiftOrReduceOrPass == REDUCE && state.stackToken(0).hasHead) noReduce(state)
            else noPass(state)
        }
      }
    }
    state.sentenceTokens
  }

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

  private def leftArc(label: String, state: ParseState)  { state.stackToken(0).setHead(state.inputToken(0), label) }
  private def rightArc(label: String, state: ParseState) { state.inputToken(0).setHead(state.stackToken(0), label) }

  private def shift(state: ParseState)  { state.stack = state.input; state.input += 1 }
  private def reduce(state: ParseState) { state.reducedIds.add(state.stack); passAux(state) }
  private def pass(state: ParseState)   { passAux(state: ParseState) }

  private def noShift(state: ParseState)  { shift(state) }
  private def noReduce(state: ParseState) { reduce(state) }
  private def noPass(state: ParseState)   { pass(state) }
  private def leftReduce(label: String, state: ParseState) { leftArc(label, state);  reduce(state) }
  private def leftPass(label: String, state: ParseState)   { leftArc(label, state);  pass(state)   }
  private def rightShift(label: String, state: ParseState) { rightArc(label, state); shift(state)  }
  private def rightPass(label: String, state: ParseState)  { rightArc(label, state); pass(state)   }
}
