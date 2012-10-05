package cc.factorie.app.nlp.parse.nonproj

object ParserEval {
  
  def calcUas(goldTrees: Seq[Seq[(Int, String)]], predTrees: Seq[Seq[(Int, String)]]): Double = {
    var correct = 0.0
    var total = 0.0
    for ((g, p) <- goldTrees zip predTrees) {
      assert(g.size == p.size, "Comparing sentences of different lengths")
      for ((gLabel, pLabel) <- g zip p) {
        total += 1
        if (gLabel._1 == pLabel._1)
          correct += 1
      }
    }
    
    return correct / total
  }
  
  def calcLas(goldTrees: Seq[Seq[(Int, String)]], predTrees: Seq[Seq[(Int, String)]]): Double = {
    var correct = 0.0
    var total = 0.0
    for ((g, p) <- goldTrees zip predTrees) {
      assert(g.size == p.size, "Comparing sentences of different lengths")
      for ((gLabel, pLabel) <- g zip p) {
        total += 1
        if (gLabel == pLabel)
          correct += 1
      }
    }
    
    return correct / total
  }

}