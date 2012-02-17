/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.nlp
import cc.factorie._
import scala.collection.mutable.ArrayBuffer

/** Value is the sequence of tokens */
// Consider Document(strValue:String, name:String = "")
class Document(val name:String, strValue:String = "") extends ChainWithSpansVar[Document,TokenSpan,Token] with Attr {
    
  // One of the following two is always null, the other non-null
  private var _string: String = strValue
  private var _stringbuf: StringBuffer = null
  def appendString(s:String): Int = {
    if (_stringbuf eq null) _stringbuf = new StringBuffer(_string)
    val result = _stringbuf.length
    _stringbuf.append(s)
    _string = null
    result
  }
  def string: String = {
    if (_string eq null) _string = _stringbuf.toString
    _stringbuf = null
    _string
  }
  //def value: String = string
  def stringLength: Int = if (_string ne null) _string.length else _stringbuf.length
  override def +=(s:TokenSpan): Unit = s match {
    case s:Sentence => {
      if (_sentences.length == 0 || _sentences.last.end < s.start) _sentences += s
      else throw new Error("Sentences must be added in order and not overlap.")
      s._chain = this // not already done in += be cause += is not on ChainWithSpans
    }
    case s:TokenSpan => super.+=(s)
  }
  override def -=(s:TokenSpan): Unit = s match {
    case s:Sentence => _sentences -= s
    case s:TokenSpan => super.-=(s)
  }
  
  def tokens: IndexedSeq[ElementType] = this
  private var _sentences = new ArrayBuffer[Sentence]
  def sentences: Seq[Sentence] = _sentences
  // potentially very slow for large documents.
  def sentenceContaining(token: Token): Sentence = sentences.find(_.contains(token)).getOrElse(null)

  def sgmlString: String = {
    val buf = new StringBuffer
    for (token <- this) {
      if (token.isSentenceStart) buf.append("<sentence>")
      token.startsSpans.foreach(span => buf.append("<"+span.name+">"))
      print(token.string)
      token.endsSpans.foreach(span => buf.append("</"+span.name+">"))
      if (token.isSentenceEnd) buf.append("</sentence>")
      buf.append(" ")
    }
    buf.toString
  }
  
  def this(c:DocumentCubbie) = {
	this(c.name.value, c.string.value)
    val tokenStarts = c.tokenStarts.value
    val tokenLengths = c.tokenLengths.value
    for (i <- 0 until tokenStarts.length)
      this += new Token(tokenStarts(i), tokenLengths(i))
    val sentenceStarts = c.sentenceStarts.value
    val sentenceLengths = c.sentenceLengths.value
    for (i <- 0 until sentenceStarts.length)
      this += new Sentence(this, sentenceStarts(i), sentenceLengths(i))(null)
    for (spanCubbie <- c.spans.value) this += new TokenSpan(this, spanCubbie)
  }
  
  def cubbieName = "Document"
  def toCubbie: DocumentCubbie = { val c = new DocumentCubbie(); this.intoCubbie(c); c }
  def intoCubbie(c:DocumentCubbie): Unit = {
    c.name := name
    c.string := string
    c.tokenStarts := tokens.map(_.stringStart)
    c.tokenLengths := tokens.map(_.stringLength)
    c.sentenceStarts := sentences.map(_.start)
    c.sentenceLengths := sentences.map(_.length)
    c.spans := spans.map(_.toCubbie)
  }
  
}


class DocumentCubbie extends Cubbie {
  val name = StringSlot("name")
  val string = StringSlot("string")
  val tokenStarts = IntListSlot("tokenStarts")
  val tokenLengths = IntListSlot("tokenLengths")
  val sentenceStarts = IntListSlot("sentenceStarts")
  val sentenceLengths = IntListSlot("sentenceLengths")
  val spans = CubbieListSlot("spans", ()=>new TokenSpanCubbie)
}





