package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.Attr
import cc.factorie.variable.{LabeledCategoricalVariable, CategoricalDomain}

// TODO Should we rename this to "Chunk"?  Then "NounChunk" and "NounChunker".  (But it is really a "NounPhraseChunk" and "NounPhraseChunker".)
// TODO What do we do about the name "MentionType"?  I like that we can talk about gender/number/type of noun phrases independently of coref.  Below we have "NounPhraseType".  Is this OK as a substitute for "MentionType"?
class Phrase(section:Section, start:Int, length:Int, val headTokenOffset: Int = -1) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  assert(headTokenOffset == -1 || headTokenOffset >= 0 && headTokenOffset < length, "Offset from beginning of span, headTokenOffset="+headTokenOffset+", but span only has length "+length)
  def headToken: Token = this.apply(headTokenOffset)
}
class PhraseList extends TokenSpanList[Phrase]

/** A simple subclass of Chunk reserved for verb phrases. */
class VerbPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)
class VerbPhraseList extends TokenSpanList[VerbPhrase]


/** A simple subclass of Chunk reserved for noun phrases.
    A Mention (used in coreference) inherits from this; (or rather should after FACTORIE NLP is further cleaned up). */
class NounPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)
class NounPhraseList extends TokenSpanList[NounPhrase]


/** Categorical variable indicating whether the noun phrase is a pronoun, nominal or proper noun. */
class NounPhraseType(val phrase:NounPhrase, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesNounPhraseTypeDomain
}
object OntonotesNounPhraseTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))

/** Categorical variable indicating whether the noun phrase is person, location, organization, etc. */
class NounPhraseEntityType(val phrase:NounPhrase, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesNounPhraseEntityTypeDomain
}
object OntonotesNounPhraseEntityTypeDomain extends CategoricalDomain[String]{
  this ++= ner.OntonotesNerDomain.categories
  this += "MISC"
}
