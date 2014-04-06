package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.Attr
import cc.factorie.variable.{CategoricalVariable, CategoricalDomain}
import cc.factorie.app.nlp.pos.PennPosDomain
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain

/** A Phrase is a TokenSpan that has a head token. */
class Phrase(section:Section, start:Int, length:Int, val headTokenOffset: Int = -1) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  assert(headTokenOffset == -1 || headTokenOffset >= 0 && headTokenOffset < length, "Offset from beginning of span, headTokenOffset="+headTokenOffset+", but span only has length "+length)
  def headToken: Token = { require(headTokenOffset >= 0); this.apply(headTokenOffset) }
  
  def isPronoun = { val i = headToken.posTag.intValue; i == PennPosDomain.prpIndex || i == PennPosDomain.prpdIndex || i == PennPosDomain.wpIndex || i == PennPosDomain.wpdIndex }
  def isProperNoun = { val i = headToken.posTag.intValue; i == PennPosDomain.nnpIndex || i == PennPosDomain.nnpsIndex }
  def isNoun = headToken.posTag.categoryValue(0) == 'N'
  def isPossessive = headToken.posTag.intValue == PennPosDomain.posIndex 
  def isAppositionOf(other:Phrase) : Boolean = (headToken.parseLabel.intValue == ParseTreeLabelDomain.appos) && (headToken.parseParent == other.headToken)

  
}

/** A collection of Phrases.  Typically used as an attribute of a Section or a Document. */
class PhraseList(spans:Iterable[Phrase]) extends TokenSpanList[Phrase](spans)

///** A simple subclass of Chunk reserved for verb phrases. */
//class VerbPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)
///** A collection of VerbPhrases.  Typically used as an attribute of a Section or a Document. */
//class VerbPhraseList(spans:Iterable[VerbPhrase]) extends TokenSpanList[VerbPhrase](spans)

///** A simple subclass of Chunk reserved for noun phrases.
//    A Mention (used in coreference) inherits from this; (or rather should after FACTORIE NLP is further cleaned up). */
//class NounPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length) {
//  def this(span:TokenSpan, headTokenOffset:Int) = this(span.section, span.start, span.length, headTokenOffset)
//}

/** A collection of Phrases that are noun phrases.  Typically used as an attribute of a Section or a Document. */
class NounPhraseList(phrases:Iterable[Phrase]) extends PhraseList(phrases)

/** Categorical variable indicating whether the noun phrase is a pronoun, nominal or proper noun. */
class NounPhraseType(val phrase:Phrase, targetValue:String) extends CategoricalVariable(targetValue) {
  def domain = OntonotesNounPhraseTypeDomain
}
object OntonotesNounPhraseTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))

