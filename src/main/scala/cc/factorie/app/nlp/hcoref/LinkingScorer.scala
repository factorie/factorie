/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
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
