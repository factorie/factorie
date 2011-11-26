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
//import cc.factorie.app.tokenseq._
import scala.collection.mutable.ArrayBuffer



// There are two ways to create Tokens and add them to Sentences and/or Documents:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and when Sentence is specified, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength?
class Token(var stringStart:Int, var stringLength:Int) extends StringVar with ChainLink[Token,Document] with Attr with cc.factorie.app.chain.Observation[Token] {
  def this(doc:Document, s:Int, l:Int) = {
    this(s, l)
    doc += this
  }
  def this(sentence:Sentence, s:Int, l:Int) = {
    this(s, l)
    if (sentence.document.sentences.last ne sentence) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = sentence
    sentence.setLength(this.position - sentence.start)(null)
  }
  def this(doc:Document, tokenString:String) = {
    this(doc, doc.stringLength, tokenString.length)
    doc.appendString(tokenString)
    //doc.appendString(" ") // TODO Make this configurable
  }
  def this(s:Sentence, tokenString:String) = {
    this(s.document, tokenString)
    if (s.document.sentences.last ne s) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = s
    s.setLength(this.position - s.start)(null)
  }
  def document: ChainType = chain
  def string = document.string.substring(stringStart, stringStart + stringLength)
  def value: String = string // abstract in StringVar
  
  // Common attributes, will return null if not present
  def posLabel = attr[cc.factorie.app.nlp.pos.PosLabel]
  def nerLabel = attr[cc.factorie.app.nlp.ner.ChainNerLabel]
  
  // Sentence methods
  // Consider not storing _sentence, but instead
  //def sentence: Sentence = document.sentenceContaining(this) // but will it be too slow?
  private var _sentence: Sentence = null
  def sentence = _sentence
  def sentenceHasNext: Boolean = (sentence ne null) && position < sentence.end
  def sentenceHasPrev: Boolean = (sentence ne null) && position > sentence.start
  def sentenceNext: Token = if (sentenceHasNext) next else null
  def sentencePrev: Token = if (sentenceHasPrev) prev else null
  def isInSentence: Boolean = _sentence ne null
  def isSentenceStart: Boolean = (_sentence ne null) && _sentence.start == position
  def isSentenceEnd: Boolean = (_sentence ne null) && _sentence.end == position
  
  // Span methods
  def spans:Seq[TokenSpan] = chain.spansContaining(position) //.toList
  def spansOfClass[A<:TokenSpan](c:Class[A]) = chain.spansOfClassContaining(c, position)
  def spansOfClass[A<:TokenSpan](implicit m:Manifest[A]) = chain.spansOfClassContaining(m.erasure.asInstanceOf[Class[A]], position)
  def startsSpans: Iterable[TokenSpan] = chain.spansStartingAt(position)
  def startsSpansOfClass[A<:TokenSpan](implicit m:Manifest[A]): Iterable[A] = chain.spansOfClassStartingAt(position)
  def endsSpans: Iterable[TokenSpan] = chain.spansEndingAt(position)
  def endsSpansOfClass[A<:TokenSpan](implicit m:Manifest[A]): Iterable[A] = chain.spansOfClassEndingAt(position)
  
  // String feature help:
  def matches(t2:Token): Boolean = string == t2.string
  /** Return true if the first  character of the word is upper case. */
  def isCapitalized = java.lang.Character.isUpperCase(string(0))
  def isPunctuation = string.matches("\\{Punct}")
  def containsLowerCase = string.exists(c => java.lang.Character.isLowerCase(c))
  /* Return true if the word contains only digits. */
  def isDigits = string.matches("\\d+")
  /* Return true if the word contains at least one digit. */
  def containsDigit = string.matches(".*\\d.*")
  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def wordShape(maxRepetitions:Int): String = cc.factorie.app.strings.stringShape(string, maxRepetitions)
  def charNGrams(min:Int, max:Int): Seq[String] = cc.factorie.app.strings.charNGrams(string, min, max)
  override def toString = "Token("+stringStart+"+"+stringLength+":"+string+")"
}

class TokenSpan(doc:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends SpanVariable[TokenSpan,Document,Token](doc, initialStart, initialLength) with Attr {
  def document = chain
  def phrase = if (length == 1) this.head.string else this.map(_.string).mkString(" ")
  // Does this span contain the words of argument span in order?
  def containsStrings(span:TokenSpan): Boolean = {
    for (i <- 0 until length) {
      if (length - i < span.length) return false
      var result = true
      var i2 = i; var j = 0
      while (j < span.length && i2 < this.length && result) {
        if (span(j).string != this(i2)) result = false
        j += 1; i2 += 1
      }
      if (result == true) return true 
    }
    return false
  }
  override def toString = "TokenSpan("+length+":"+this.phrase+")"
  /** A short name for this span */
  def name: String = attr.values.head match {
    case label:LabelVariable[String] => label.categoryValue
    case x => x.toString
  }
}

class Sentence(doc:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends TokenSpan(doc, initialStart, initialLength) {
  def this(doc:Document)(implicit d:DiffList = null) = this(doc, doc.length, 0)
  def tokens: IndexedSeq[Token] = this 
  def tokenAtCharIndex(charOffset:Int): Token = {
    require(charOffset >= first.stringStart && charOffset <= last.stringStart + last.stringLength)
    var i = 0 // TODO Implement as faster binary search
    while (i < this.length && this(i).stringStart <= charOffset) {
      val token = this(i)
      if (token.stringStart <= charOffset && token.stringStart + token.stringLength <= charOffset) return token
      i += 1
    }
    return null
  }
}

/*class Sentence(var stringStart:Int, var stringLength:Int) extends StringVar with ChainInChain[Sentence,Token,Document] {
  def this(doc:Document) = { this(doc.stringLength, 0); doc += this }
  def this(doc:Document, sentenceString:String) = { this(doc.stringLength, sentenceString.length); doc.appendString(sentenceString) }
  def this(doc:Document, stringStart:Int, stringLength:Int) = { this(stringStart, stringLength); doc += this }
  
  def document: ChainType = chain
  def string: String = document.string.substring(stringStart, stringLength)
  def value: String = string
  def tokens: IndexedSeq[ElementType] = this
}*/


/** Value is the sequence of tokens */
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


  /** Given some String features read from raw data, in which the first feature is always the word String itself, add some more features.  */
/*
  def standardFeatureFunction(inFeatures:Seq[String]): Seq[String] = {
    val result = new scala.collection.mutable.ArrayBuffer[String]
    // Assume the first feature is the word
    result += "W="+inFeatures(0)
    result += "SHAPE="+wordShape(inFeatures(0), 2)
    result ++= charNGrams(inFeatures(0), 2, 5)
    result ++= inFeatures.drop(1)
    result
  }
*/

