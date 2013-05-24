package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp.parse.ParseTree

object ParserEval {
  def calcUas(trees: Seq[ParseTree]): Double = {
    var correct = 0.0
    var total = 0.0
    for (tree <- trees) {
      for (i <- 0 until tree.sentence.length) {
        if (!tree.sentence.tokens(i).isPunctuation) {
          total += 1
          if (tree._parents(i) == tree._targetParents(i)) correct += 1
        }
      }
    }
    correct / total
  }
  
  def calcLas(trees: Seq[ParseTree]): Double = {
    var correct = 0.0
    var total = 0.0
    for (tree <- trees) {
      for (i <- 0 until tree.sentence.length) {
        if (!tree.sentence.tokens(i).isPunctuation) {
          total += 1
          if (tree._parents(i) == tree._targetParents(i) && tree._labels(i).valueIsTarget) correct += 1
        }
      }
    }
    correct / total
  }

}