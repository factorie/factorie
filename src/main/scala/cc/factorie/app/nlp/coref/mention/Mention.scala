package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp.coref._
import cc.factorie.util.Attr
import cc.factorie.app.nlp.{TokenSpan, TokenSpanList, Section}
import cc.factorie.variable.{LabeledCategoricalVariable, CategoricalDomain}
import cc.factorie.app.nlp.phrase.{Phrase}

//class MentionList(spans:Iterable[AbstractMention]) extends scala.collection.mutable.ArrayBuffer[AbstractMention]{
//  this ++= spans
//}

// TODO This class should be removed and replaced with something like:
// trait Mention { def string: String } or whatever else we think would be common among all mentions, 
// including those not coming from Document Phrases.
// Many places that currently use Mention would be replaced by 
// class PhraseMention(val phrase:NounPhrase, var entity:DocEntity)
// -akm

//See also CorefMention
/** A TokenSpan holding a mention of an entity.  
    Note that headTokenIndex is an offset from the beginning of this span, not the beginning of the Section.
    Note also that since Mention is a Span, and Span is a sequence over Tokens, "this.head" is the first token of the span, not the "natural language head" of the phrase; for the later use "this.headToken". */
//class Mention(section:Section, start:Int, length:Int, val headTokenIndex: Int = -1) extends Phrase(section,start,length,headTokenIndex) with Attr {
//  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
//}


object OntonotesMentionTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))

/** Categorical variable indicating whether the mention is a pronoun, nominal or proper noun.
    (Obviously different from MentionEntityType, which may indicate whether it is a person, location, organization, etc.) */
class MentionType(val mention:AbstractMention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesMentionTypeDomain
}

// TODO Is this really necessary?  I think an entity should be more than this. -akm
//class Entity(val name: String = "")




