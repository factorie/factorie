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

/** A portion of the string contents of a Document.
    @author Andrew McCallum */
trait DocumentSubstring {
  /** The Document of which this DocumentSubstring is a part. */
  def document: Document
  /** The character offset into the Document.string at which this DocumentSubstring begins. */
  def stringStart: Int
  /** The character offset into the Document.string at which this DocumentSubstring is over. 
      In other words, the last character of the DocumentSubstring is Document.string(this.stringEnd-1). */
  def stringEnd: Int
  /** The substring of the Document encompassed by this DocumentSubstring. */
  def string: String
}

/** A Document holds a String containing the original raw string contents
    of a natural language document to be processed.  The Document also holds
    a sequence of Sections, each of which is delineated by character offsets 
    into the Document's string, and each of which contains a sequence of Tokens, 
    Sentences and other TokenSpans which may be annotated.
    
    Documents may be constructed with their full string contents, or they may
    have their string contents added to by the appendString method.
    
    Documents also have an optional "name" which can be set by Document.setName.
    
    The Document.stringLength method may be a faster alternative to Document.string.length
    when you are in the middle of multiple appendString calls.
    
    The canonical sequence of Sections in the Document is available through 
    the Document.sections method.
    
    By default the canonical sequence of Sections holds a single Section that covers the
    entire string contents of the Document (even as the Document grows).  This canonical sequence
    of Sections may be modified by the user, but this special all-encompassing Section
    instance will always be available as Document.asSection.
    
    Even though Tokens, Sentences and TokenSpans are really stored in the Sections,
    Document has basic convenience methods for obtaining iterable collections of these
    by concatenating them from the canonical sequence of Sections.  If, for example,
    you need the Tokens as a Seq[Token] rather than an Iterable[Token], or you need
    more advanced queries for TokenSpan types, you should use methods on a Section,
    not on the Document.  In this case typical processing looks like:
    "for (section <- document.sections) ...".  
    
    @author Andrew McCallum */
class Document extends DocumentSubstring with Attr {
  def this(stringContents:String) = { this(); _string = stringContents }
  def name: String = this.attr[DocumentName].string
  /** Set DocumentName attr on Document.  If String argument is null, remove DocumentName attr if present. */
  def setName(s:String): this.type = { if (s ne null) this.attr += DocumentName(s) else this.attr.remove[DocumentName]; this }
  
  // One of the following two is always null, the other non-null
  private var _string: String = ""
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

  // For DocumentSubstring
  def document: Document = this
  def stringStart: Int = 0
  def stringEnd: Int = stringLength
  
  // Managing sections.  These are the canonical Sections, but alternative Sections can be attached as Attr's.
  val asSection: Section = new Section { def document: Document = Document.this; def stringStart = 0; def stringEnd = document.stringEnd }
  private var _sections: Seq[Section] = List(asSection)
  def sections: Seq[Section] = _sections //if (_sections.length == 0) return Seq(this) else _sections
  
  // A few iterators that combine the results from the Sections
  //def tokens: Iterator[Token] = for (section <- sections.iterator; token <- section.tokens.iterator) yield token
  def tokens: Iterable[Token] = if (sections.length == 1) sections.head.tokens else new Iterable[Token] { def iterator = for (section <- sections.iterator; token <- section.tokens.iterator) yield token }
  //def sentences: Iterator[Sentence] = for (section <- sections.iterator; sentence <- section.sentences.iterator) yield sentence
  def sentences: Iterable[Sentence] = if (sections.length == 1) sections.head.sentences else new Iterable[Sentence] { def iterator = for (section <- sections.iterator; sentence <- section.sentences.iterator) yield sentence } 
  //def spans: Iterator[TokenSpan] = for (section <- sections.iterator; span <- section.spans.iterator) yield span
  def spans: Iterable[TokenSpan] = if (sections.length == 1) sections.head.spans else new Iterable[TokenSpan] { def iterator = for (section <- sections.iterator; span <- section.spans.iterator) yield span }
  
  /** An efficient way to get the total number of Tokens in the canonical Sections of this Document. */
  def tokenCount: Int = if (sections.length == 0) sections.head.length else sections.foldLeft(0)((result, section) => result + section.length)
  /** An efficient way to get the total number of Sentences in the canonical Sections of this Document. */
  def sentenceCount: Int = if (sections.length == 0) sections.head.sentences.length else sections.foldLeft(0)((result, section) => result + section.sentences.length)
  /** An efficient way to get the total number of Spans (not including Sentences) in the canonical Sections of this Document. */
  def spanCount: Int = if (sections.length == 0) sections.head.spans.length else sections.foldLeft(0)((result, section) => result + section.spans.length)
    
  /** Keeping records of which DocumentAnnotators have been run on this document, producing which annotations.
      The collection of DocumentAnnotators that have been run on this Document.  
      A Map from the annotation class to the DocumentAnnotator that produced it.
      Note that this map records annotations placed not just on the Document itself, but also its constituents,
      such as TokenSpan, Token, Sentence, etc. */
  val annotators = new DocumentAnnotatorMap
  /** Has an annotation of class 'c' been placed somewhere within this Document? */
  def hasAnnotation(c:Class[_]): Boolean = annotators.keys.exists(k => c.isAssignableFrom(k))
  /** Which DocumentAnnotator produced the annotation of class 'c' within this Document.  If  */
  def annotatorFor(c:Class[_]): Option[DocumentAnnotator] = annotators.keys.find(k => c.isAssignableFrom(k)).collect({case k:Class[_] => annotators(k)})
  
  /** Return a String containing the Token strings in the document, with sentence and span boundaries indicated with SGML. */
  def sgmlString: String = {
    val buf = new StringBuffer
    for (section <- sections; token <- section.tokens) {
      if (token.isSentenceStart) buf.append("<sentence>")
      token.startsSpans.foreach(span => buf.append("<"+span.name+">"))
      buf.append(token.string)
      token.endsSpans.foreach(span => buf.append("</"+span.name+">"))
      if (token.isSentenceEnd) buf.append("</sentence>")
      buf.append(" ")
    }
    buf.toString
  }
  
  /** Return a String containing the Token strings in the document, with one-word-per-line 
      and various tab-separated attributes appended on each line. */
  def owplString(attributes:Iterable[(Token)=>Any] = List((t:Token) => t.lemmaString)): String = {
    val buf = new StringBuffer
    for (section <- sections; token <- section.tokens) {
      if (token.isSentenceStart) buf.append("\n")
      buf.append("%d\t%d\t%s\t".format(token.position+1, token.positionInSentence+1, token.string))
      //buf.append(token.stringStart); buf.append("\t")
      //buf.append(token.stringEnd)
      for (af <- attributes) {
        buf.append("\t")
        af(token) match {
          case cv:CategoricalVar[_,String @unchecked] => buf.append(cv.categoryValue.toString)
          case null => {}
          case v:Any => buf.append(v.toString)
        }
      }
      buf.append("\n")
    }
    buf.toString
  }

}

/** Used as an attribute on Document to hold the document's name. */
case class DocumentName(string:String) {
  override def toString: String = string
}


/** A Cubbie for serializing a Document, with separate slots for the Tokens, Sentences, and TokenSpans. 
    Note that it does not yet serialize Sections, and relies on Document.asSection being the only Section. */
class DocumentCubbie[TC<:TokenCubbie,SC<:SentenceCubbie,TSC<:TokenSpanCubbie](val tc:()=>TC, val sc:()=>SC, val tsc:()=>TSC) extends Cubbie with AttrCubbieSlots {
  val name = StringSlot("name")
  val string = StringSlot("string")  
  val tokens = CubbieListSlot("tokens", tc)
  val sentences = CubbieListSlot("sentences", sc)
  val spans = CubbieListSlot("spans", tsc)
  def storeDocument(doc:Document): this.type = {
    name := doc.name
    string := doc.string
    if (doc.asSection.length > 0) tokens := doc.tokens.toSeq.map(t => tokens.constructor().storeToken(t))
//    if (doc.spans.length > 0) spans := doc.spans.map(s => spans.constructor().store(s))
    if (doc.asSection.sentences.length > 0) sentences := doc.sentences.toSeq.map(s => sentences.constructor().storeSentence(s))
    storeAttr(doc)
    this
  }
  def fetchDocument: Document = {
    val doc = new Document(string.value).setName(name.value)
    if (tokens.value ne null) tokens.value.foreach(tc => doc.asSection += tc.fetchToken)
    //if (spans.value ne null) spans.value.foreach(sc => doc += sc.fetch(doc))
    if (sentences.value ne null) sentences.value.foreach(sc =>  sc.fetchSentence(doc.asSection))
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


