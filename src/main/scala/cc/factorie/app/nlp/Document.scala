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
// Consider Document(strValue:String, val name:String = "")
// TODO: Must all Documents have a name?  Alexandre thinks not.
class Document(val name:String, strValue:String = "") extends ChainWithSpansVar[Document,TokenSpan,Token] with Attr {
    
  // One of the following two is always null, the other non-null
  private var _string: String = strValue
  private var _stringbuf: StringBuffer = null
  /** Append the string 's' to this Document.
      @return the length of the Document's string before string 's' was appended. */
  def appendString(s:String): Int = {
    if (_stringbuf eq null) _stringbuf = new StringBuffer(_string)
    val result = _stringbuf.length
    _stringbuf.append(s)
    _string = null
    result
  }
  def string: String = {
    this.synchronized {
      if (_string eq null) _string = _stringbuf.toString
      _stringbuf = null
    }
    _string
  }
  def stringLength: Int = if (_string ne null) _string.length else _stringbuf.length
  
  /** Just a clearly-named alias for Chain.links. */
  def tokens: IndexedSeq[Token] = links
  
  // Managing Sentences
  private var _sentences = new ArrayBuffer[Sentence]
  def sentences: Seq[Sentence] = _sentences
  // potentially very slow for large documents. // TODO Why isn't this simply using token.sentence??  Or even better, just remove this method. -akm
  def sentenceContaining(token: Token): Sentence = sentences.find(_.contains(token)).getOrElse(null)

  // Managing Spans, keeping Sentence-spans separate from all other TokenSpans
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
  
  // Keeping records of which DocumentAnnotators have been run on this document, producing which annotations
  val documentProcessors = new DocumentProcessorMap // TODO Remove this
  //val documentAnnotators = new DocumentAnnotatorMap
  
  /** Return a String containing the token strings in the document, with sentence and span boundaries indicated with SGML. */
  def sgmlString: String = {
    val buf = new StringBuffer
    for (token <- tokens) {
      if (token.isSentenceStart) buf.append("<sentence>")
      token.startsSpans.foreach(span => buf.append("<"+span.name+">"))
      buf.append(token.string)
      token.endsSpans.foreach(span => buf.append("</"+span.name+">"))
      if (token.isSentenceEnd) buf.append("</sentence>")
      buf.append(" ")
    }
    buf.toString
  }
  
  /** Return a String containing the token strings in the document, with one-word-per-line 
      and various tab-separated attributes appended on each line. */
  // TODO Remove this default argument -akm
  def owplString(attributes:Iterable[(Token)=>Any] = List((t:Token) => t.posLabel.categoryValue)): String = {
    val buf = new StringBuffer
    for (token <- tokens) {
      if (token.isSentenceStart) buf.append("\n")
      buf.append(token.string); buf.append("\t")
      //buf.append(token.lemmaString); buf.append("\t") // This can now be printed as an attribute
      //buf.append(token.stringStart); buf.append("\t")
      //buf.append(token.stringEnd)
      for (af <- attributes) {
        buf.append("\t")
        af(token) match {
          case cv:CategoricalVar[_,String] => buf.append(cv.categoryValue.toString)
          case null => {}
          case v:Any => buf.append(v.toString)
        }
      }
      buf.append("\n")
    }
    buf.toString
  }
}

/** A Cubbie for serializing a Document, with separate slots for the Tokens, Sentences, and TokenSpans. */
class DocumentCubbie[TC<:TokenCubbie,SC<:SentenceCubbie,TSC<:TokenSpanCubbie](val tc:()=>TC, val sc:()=>SC, val tsc:()=>TSC) extends Cubbie with AttrCubbieSlots {
  val name = StringSlot("name")
  val string = StringSlot("string")  
  val tokens = CubbieListSlot("tokens", tc)
  val sentences = CubbieListSlot("sentences", sc)
  val spans = CubbieListSlot("spans", tsc)
  def storeDocument(doc:Document): this.type = {
    name := doc.name
    string := doc.string
    if (doc.length > 0) tokens := doc.tokens.map(t => tokens.constructor().storeToken(t))
//    if (doc.spans.length > 0) spans := doc.spans.map(s => spans.constructor().store(s))
    if (doc.sentences.length > 0) sentences := doc.sentences.map(s => sentences.constructor().storeSentence(s))
    storeAttr(doc)
    this
  }
  def fetchDocument: Document = {
    val doc = new Document(name.value, string.value)
    if (tokens.value ne null) tokens.value.foreach(tc => doc += tc.fetchToken)
    //if (spans.value ne null) spans.value.foreach(sc => doc += sc.fetch(doc))
    if (sentences.value ne null) sentences.value.foreach(sc =>  sc.fetchSentence(doc))
    fetchAttr(doc)
    doc
  }
}

// TODO Consider moving this to file util/Attr.scala
trait AttrCubbieSlots extends Cubbie {
  val storeHooks = new cc.factorie.util.Hooks1[Attr]
  val fetchHooks = new cc.factorie.util.Hooks1[AnyRef]
  def storeAttr(a:Attr): this.type = { storeHooks(a); this }
  def fetchAttr(a:Attr): Attr = { fetchHooks(a); a }
}

trait DateAttrCubbieSlot extends AttrCubbieSlots {
  val date = DateSlot("date")
  storeHooks += ((a:Attr) => date := a.attr[java.util.Date])
  //fetchHooks += ((a:Attr) => a.attr += date.value)
  fetchHooks += { case a:Attr => a.attr += date.value }
}


