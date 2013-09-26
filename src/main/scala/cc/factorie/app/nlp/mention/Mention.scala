package cc.factorie.app.nlp.mention

import collection.mutable.ArrayBuffer
import cc.factorie.util.Attr
import cc.factorie.app.nlp.{Sentence, TokenSpan, Document, Section, Token}
import cc.factorie.{CategoricalDomain, LabeledCategoricalVariable}

/**
 * Created with IntelliJ IDEA.
 * User: belanger
 * Date: 5/29/13
 * Time: 2:01 PM
 * To change this template use File | Settings | File Templates.
 */

class MentionList extends ArrayBuffer[Mention]

//object Mention{
//  def apply(sec: Section, start:Int, length: Int, headTokenIndex:Int) =  new Mention(sec, start, length, headTokenIndex)
//  def apply(span:TokenSpan, headTokenIndex: Int = -1): Mention = new Mention(span.section, span.start, span.length, headTokenIndex)
//}

// TODO Rename this to "Chunk"?  Then "Mention" will be a trait that has to do with coref, and which can be folded into lots of things.
/** A TokenSpan holding a mention of an entity.  
    Note that headTokenIndex is an offset from the beginning of this span, not the beginning of the Section.
    Note also that since Mention is a Span, and Span is a sequence over Tokens, "this.head" is the first token of the span, not the "natural language head" of the phrase; for the later use "this.headToken". */
class Mention(section:Section, start:Int, length:Int, val headTokenIndex: Int = -1) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  assert( headTokenIndex == -1 || headTokenIndex >= 0 && headTokenIndex <= length,"if provided, the headTokenIndex passed to the constructor must be an offset w.r.t the start of the mention. Specified " + headTokenIndex + ", but span is only " + length + " long")
  def headToken: Token = this.apply(headTokenIndex)
}


object OntonotesMentionTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))

/** Categorical variable indicating whether the mention is a pronoun, nominal or proper noun.
    (Obviously different from MentionEntityType, which may indicate whether it is a person, location, organization, etc.) */
class MentionType(val mention:Mention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesMentionTypeDomain
}

class Entity(val name: String = "")




