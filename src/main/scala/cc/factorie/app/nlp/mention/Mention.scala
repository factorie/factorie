package cc.factorie.app.nlp.mention

import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.{Sentence, TokenSpan, Document}

/**
 * Created with IntelliJ IDEA.
 * User: belanger
 * Date: 5/29/13
 * Time: 2:01 PM
 * To change this template use File | Settings | File Templates.
 */

class MentionList extends ArrayBuffer[Mention]

object Mention{
  def apply(doc: Document, start: Int, length: Int, headTokenIndex: Int) =  new Mention(new TokenSpan(doc,start,length),headTokenIndex)
}

case class Mention(span: TokenSpan, headTokenIndex: Int = -1, mentionType: String = null){
  def document: Document = span.document
  def start: Int = span.start
  def length: Int = span.length
  def sentence: Sentence = span.sentence
  def headToken = document.tokens(headTokenIndex)
}
