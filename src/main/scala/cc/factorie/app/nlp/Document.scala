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
    
}


class DocumentCubbie[TC<:TokenCubbie,TSC<:TokenSpanCubbie](val tc:()=>TC, val tsc:()=>TSC) extends Cubbie {
  val name = StringSlot("name")
  val string = StringSlot("string")  
  val tokens = CubbieListSlot("tokens", tc)
  val spans = CubbieListSlot("spans", tsc)
  def storeDocument(doc:Document): this.type = {
    name := doc.name
    string := doc.string
    if (doc.tokens.length > 0) tokens := doc.tokens.map(t => tokens.constructor().storeToken(t))
    if (doc.spans.length > 0) spans := doc.spans.map(s => spans.constructor().storeTokenSpan(s))
    this
  }
  def fetchDocument: Document = {
    val doc = new Document(name.value, string.value)
    if (tokens.value ne null) tokens.value.foreach(tc => doc += tc.fetchToken)
    if (spans.value ne null) spans.value.foreach(sc => doc += sc.fetchTokenSpan(doc))
    doc
  }
}

/*
@deprecated("Old draft")
class DocumentCubbieOld extends Cubbie {
  val name = StringSlot("name")
  val string = StringSlot("string")  
  val tokens = CubbieListSlot("tokens", ()=>new TokenCubbie)
  val spans = CubbieListSlot("spans", ()=>new TokenSpanCubbie)
  def storeDocument(doc:Document): this.type = {
    name := doc.name
    string := doc.string
    if (doc.tokens.length > 0) tokens := doc.tokens.map(t => tokens.constructor().storeToken(t))
    this
  }
  def fetchDocument: Document = {
    val doc = new Document(name.value, string.value)
    if (tokens ne null) tokens.value.foreach(tc => doc += tc.fetchToken)
    doc
  }
}

@deprecated("Old draft")
trait DocumentTokenNerLabelsOld extends DocumentCubbie {
  self =>
  def newTokenNerLabel(t:Token, value:String): cc.factorie.app.nlp.ner.ChainNerLabel
  //override def newTokenCubbieFunction = () => new TokenCubbie with TokenNerLabelCubbie
  //override val tokens = CubbieListSlot("tokens", () => new TokenCubbie with TokenNerLabelCubbie { def newTokenNerLabel(t:Token, v:String) = self.newTokenNerLabel(t, v) })
  override def storeDocument(doc:Document): this.type = {
    super.storeDocument(doc)
    doc.tokens.zip(tokens.value).foreach({case (token,tcubbie) => tcubbie._rawPut("ner", token.nerLabel.categoryValue)})
    this
  }
  override def fetchDocument: Document = {
    val doc = super.fetchDocument
    //doc.tokens.zip(tokens.value).foreach({case (token,tcubbie) => token.attr += tcubbie._rawPut("ner", token.nerLabel.categoryValue)})
    doc
  }
}
*/


