package cc.factorie.app.nlp.mention

import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.{Sentence, TokenSpan, Document, Section, Token}

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

// TODO I don't like the last two arguments of this constructor: -akm
// The index should be in terms of the Span, not the Section
// I'm not comfortable with mentionType as a String.  Too error prone.  I think it should be a Label.
case class Mention(span: TokenSpan, headTokenIndex: Int = -1, mentionType: String = null) {
  def document: Document = span.document
  def section: Section = span.section
  /** The Token position in its Section. */
  def start: Int = span.start
  /** The number of Tokens in the mention's span. */
  def length: Int = span.length
  def sentence: Sentence = span.sentence
  def headToken: Token = document.asSection.tokens(headTokenIndex) // This is buggy.  If the Document has multiple Sections, this won't work. -akm
}
