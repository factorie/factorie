package cc.factorie.app.nlp.parse

object ParserEval {
  def calcUas(trees: Iterable[ParseTree], includePunct: Boolean = false): Double = {
    var correct = 0.0
    var total = 0.0
    for (tree <- trees) {
      for (i <- 0 until tree.sentence.length) {
        if (includePunct || !tree.sentence.tokens(i).isPunctuation) {
          total += 1
          if (tree._parents(i) == tree._targetParents(i)) correct += 1
        }
      }
    }
    correct / total
  }
  
  def calcLas(trees: Iterable[ParseTree], includePunct: Boolean = false): Double = {
    var correct = 0.0
    var total = 0.0
    for (tree <- trees) {
      for (i <- 0 until tree.sentence.length) {
        if (includePunct || !tree.sentence.tokens(i).isPunctuation) {
          total += 1
          if (tree._parents(i) == tree._targetParents(i) && tree._labels(i).valueIsTarget) correct += 1
        }
      }
    }
    correct / total
  }

}