package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.Attr
import cc.factorie.variable.{CategoricalVariable, CategoricalDomain}

/** A Phrase is a TokenSpan that has a head token. */
class Phrase(section:Section, start:Int, length:Int, val headTokenOffset: Int = -1) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  assert(headTokenOffset == -1 || headTokenOffset >= 0 && headTokenOffset < length, "Offset from beginning of span, headTokenOffset="+headTokenOffset+", but span only has length "+length)
  def headToken: Token = this.apply(headTokenOffset)
}

/** A collection of Phrases.  Typically used as an attribute of a Section or a Document. */
class PhraseList(spans:Iterable[Phrase]) extends TokenSpanList[Phrase](spans)

/** A simple subclass of Chunk reserved for verb phrases. */
class VerbPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)
/** A collection of VerbPhrases.  Typically used as an attribute of a Section or a Document. */
class VerbPhraseList(spans:Iterable[VerbPhrase]) extends TokenSpanList[VerbPhrase](spans)


/** A simple subclass of Chunk reserved for noun phrases.
    A Mention (used in coreference) inherits from this; (or rather should after FACTORIE NLP is further cleaned up). */
class NounPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)

/** A collection of NounPhrases.  Typically used as an attribute of a Section or a Document. */
class NounPhraseList(spans:Iterable[NounPhrase]) extends TokenSpanList[NounPhrase](spans)

/** Categorical variable indicating whether the noun phrase is a pronoun, nominal or proper noun. */
class NounPhraseType(val phrase:NounPhrase, targetValue:String) extends CategoricalVariable(targetValue) {
  def domain = OntonotesNounPhraseTypeDomain
}
object OntonotesNounPhraseTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))

