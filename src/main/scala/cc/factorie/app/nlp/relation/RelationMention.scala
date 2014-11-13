package cc.factorie.app.nlp.relation

import cc.factorie.variable._
import cc.factorie.app.nlp.coref._
import cc.factorie.util.Attr
import scala.collection.mutable

object RelationArgFeaturesDomain extends CategoricalDomain[String]

@deprecated("Marked for Possible Deletion")
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

case class TACRelation(value:String, confidence:Double, provenance:String)

case class TACRelationList(value:Iterable[TACRelation])


class RelationMentionSeq extends SeqVariable[RelationMention]

class RelationMention(val arg1: Mention, val arg2: Mention, var isArg1First:Boolean=true) extends ArrowVariable(arg1, arg2) with Attr {
  val _relations = mutable.ArrayBuffer[TACRelation]()
  this.attr += TACRelationList(_relations)
  def relations = this.attr[TACRelationList]
}
