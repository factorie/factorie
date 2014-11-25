package cc.factorie.app.nlp.hcoref

/**
 * @author John Sullivan
 */
object LinkingScorer {

  def scoreString(pred:Map[String,Set[String]], gold:Map[String,Set[String]]):String = {
    assert(gold.keySet == pred.keySet)
    assert(gold.values.flatten.toSet == pred.values.flatten.toSet)

    val microDenom = gold.values.map(_.size).sum.toDouble
    val microNum = gold.map{ case(entId, goldMentions) =>
      pred(entId).intersect(goldMentions).size
    }.sum
    val microAccuracy = microNum/microDenom

    val macroDenom = gold.keySet.size
    val macroNum = gold.map{ case(entId, goldMentions) =>
      pred(entId).intersect(goldMentions).size / goldMentions.size.toDouble
    }.sum

    val macroAccuracy = macroNum/macroDenom
    "P(mentions,entites) %d %d\nT(mentions,entities) %d %d\nmacro accuracy: %.4f\nmicro accuracy: %.4f".format(pred.values.map(_.size).sum, pred.keySet.size, gold.values.map(_.size).sum, gold.keySet.size,macroAccuracy, microAccuracy)
  }
}
