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
package cc.factorie.app.nlp.relation

import cc.factorie.app.nlp.coref._
import cc.factorie.util.Attr
import cc.factorie.variable._

import scala.collection.mutable._

object RelationArgFeaturesDomain extends CategoricalDomain[String]

@deprecated("Marked for Possible Deletion", "Before 2014-11-17")
class ArgFeatures(val arg: Mention, val first: Boolean) extends BinaryFeatureVectorVariable[String] {
  def domain = RelationArgFeaturesDomain

  def compute() = {
    this += "BIAS"
    // TODO compute relation features using "first" and "arg"
    // TODO convert Lexicons (from refectorie.proj.jntinf) to app.chain.Lexicon
    for (tok <- arg.phrase.tokens) {
      this += "POS_" + tok.posTag.categoryValue
      if (tok.string(0).isLower)
        this += "STEM_" + tok.string.replaceAll("\\s+", " ").take(5)
    }

    this += "HEAD_POS_" + arg.phrase.headToken.posTag.categoryValue
  }
}

class RelationMentionsSet extends SetVariable[RelationMention]

class RelationMentionList extends ArrayBuffer[RelationMention]() with Attr

case class TACRelation(value:String, confidence:Double, provenance:String)

case class TACRelationList(value:Iterable[TACRelation])

class RelationMentionSeq extends SeqVariable[RelationMention]

class RelationMention(val arg1: Mention, val arg2: Mention, var isArg1First:Boolean=true) extends ArrowVariable(arg1, arg2) with Attr {
  val _relations = ArrayBuffer[TACRelation]()
  this.attr += TACRelationList(_relations)
  def relations = this.attr[TACRelationList]
}
