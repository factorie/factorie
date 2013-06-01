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

object Mention{
  def apply(sec: Section, start: Int, length: Int, headTokenIndex: Int) =  new Mention(new TokenSpan(sec, start, length),headTokenIndex)
}


//note that headTokenIndex has a span-level offset
case class Mention(span: TokenSpan, headTokenIndex: Int = -1) extends Attr{
  def document: Document = span.document
  def section: Section = span.section
  /** The Token position in its Section. */
  def start: Int = span.start
  /** The number of Tokens in the mention's span. */
  def length: Int = span.length
  assert( headTokenIndex == -1 || headTokenIndex >= 0 && headTokenIndex <= span.length,"if provided, the headTokenIndex passed to the constructure must be an offset w.r.t the start of the mention")
  def sentence: Sentence = span.sentence
  def headToken: Token = document.asSection.tokens(headTokenIndex) // This is buggy.  If the Document has multiple Sections, this won't work. -akm
}

//this differs from EntityType. This is about whether a mention is a pronoun, etc., not what it potentially refers to (person, location, etc.)
class MentionType(val mention:Mention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesMentionTypeDomain
}


object OntonotesMentionTypeDomain extends CategoricalDomain[String] {
  this ++= Seq(
    "PRO","NOM","NAM"
  )

  freeze()
}



