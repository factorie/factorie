package cc.factorie.app.nlp

import cc.factorie.variable._
import cc.factorie.app.nlp.coref._
import cc.factorie.util.Attr

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

@deprecated("Marked for Possible Deletion")
class RelationMentionsSet extends SetVariable[RelationMention]

@deprecated("Marked for Possible Deletion")
class RelationMention(val arg1: Mention, val arg2: Mention, val relationType: String, val relationSubType: Option[String]) extends ArrowVariable(arg1, arg2) with Attr {
  val arg1Features = new ArgFeatures(arg1, true)
  val arg2Features = new ArgFeatures(arg2, false)
}
