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

// There are two ways to create Tokens and add them to Sentences and/or Documents:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and when Sentence is specified, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength?
class Token(var stringStart:Int, var stringEnd:Int) extends cc.factorie.app.chain.Observation[Token] with ChainLink[Token,Document] with Attr {
  assert(stringStart <= stringEnd)
  def this(doc:Document, s:Int, e:Int) = {
    this(s, e)
    doc += this
  }
  def this(sentence:Sentence, s:Int, e:Int) = {
    this(s, e) // TODO Rather than TODO below, we should just make this line: this(sentence.document, s, l)
    if (sentence.document.sentences.last ne sentence) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = sentence
    // TODO Don't we also need to do??: doc += this
    sentence.setLength(this.position - sentence.start + 1)(null)
  }
  def this(doc:Document, tokenString:String) = {
    this(doc, doc.stringLength, doc.stringLength + tokenString.length)
    doc.appendString(tokenString)
    //doc.appendString(" ") // TODO Make this configurable
  }
  def this(s:Sentence, tokenString:String) = {
    this(s.document, tokenString)
    if (s.document.sentences.last ne s) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = s
    s.setLength(this.position - s.start + 1)(null)
  }
  def document: ChainType = chain
  def value: String = string // abstract in StringVar
  def docSubstring = document.string.substring(stringStart, stringEnd)
  /** Return the string contents of this Token, either from its attr[TokenString] variable or ,if unset, directly as a substring of the Document */
  def string = { val ts = attr[TokenString]; if (ts ne null) ts.value else docSubstring }
  def stringVar: StringVariable = { val ts = attr[TokenString]; if (ts ne null) ts else { val ts2 = new TokenString(this, docSubstring); attr += ts2; ts2 } }
  def sentencePosition = position - sentence.start
  //def stringLength = throw new Error("Use string.length instead")
  
  // Common attributes, will return null if not present
  def posLabel = attr[cc.factorie.app.nlp.pos.PosLabel]
  def nerLabel = attr[cc.factorie.app.nlp.ner.ChainNerLabel]
  // Parse attributes, will throw exception if parse is not present
  def parse = sentence.attr[cc.factorie.app.nlp.parse.ParseTree]
  def parseParent: Token = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].parent(sentencePosition)
  def parseLabel: cc.factorie.app.nlp.parse.ParseTreeLabel = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].label(sentencePosition)
  def parseChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].children(sentencePosition)
  def parseLeftChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].leftChildren(sentencePosition)
  def parseRightChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].leftChildren(sentencePosition)
  def parseChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].childrenLabeled(sentencePosition, label.intValue)
  def parseLeftChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].leftChildrenLabeled(sentencePosition, label.intValue)
  def parseRightChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].rightChildrenLabeled(sentencePosition, label.intValue)
  
  // Sentence methods
  private var _sentence: Sentence = null // This must be changeable from outside because sometimes Tokenization comes before Sentence segmentation
  def sentence = {
    if (_sentence eq null) _sentence = document.sentenceContaining(this)
    _sentence
  }
  @deprecated("This method should be removed. use 'sentencePosition'.")
  def indexInSentence: Int = sentencePosition
  def sentenceHasNext: Boolean = (sentence ne null) && position < sentence.end
  def sentenceHasPrev: Boolean = (sentence ne null) && position > sentence.start
  def sentenceNext: Token = if (sentenceHasNext) next else null
  def sentencePrev: Token = if (sentenceHasPrev) prev else null
  def isInSentence: Boolean = sentence ne null
  def isSentenceStart: Boolean = (sentence ne null) && sentence.start == position
  def isSentenceEnd: Boolean = (sentence ne null) && sentence.end == position
  

  // Span methods
  def inSpan: Boolean = chain.hasSpanContaining(position) 
  def inSpanOfClass[A<:TokenSpan](c:Class[A]): Boolean = chain.hasSpanOfClassContaining(c, position)
  def inSpanOfClass[A<:TokenSpan](implicit m:Manifest[A]): Boolean = chain.hasSpanOfClassContaining(m.erasure.asInstanceOf[Class[A]], position)
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
  override def toString = "Token("+stringStart+":"+string+")"

}

class TokenString(val token:Token, s:String) extends StringVariable(s) 


// Cubbie storage

class TokenCubbie extends Cubbie {
  val start = IntSlot("start")
  val end = IntSlot("end")
  def postFetchToken(t:Token): Unit = {}
  def fetchToken: Token = {
    val t = new Token(start.value, end.value)
    postFetchToken(t)
    t
  }
  def postStoreToken(t:Token): Unit = {}
  def storeToken(t:Token): this.type = {
    start := t.stringStart
    end := t.stringEnd
    postStoreToken(t)
    this
  }
}

trait TokenStringCubbieSlot extends TokenCubbie {
  val string = StringSlot("string")
  override def postStoreToken(t:Token): Unit = {
    super.postStoreToken(t)
    string := t.string	
  }
  // No postFetchToken necessary because "string" isn't needed for Token initialization
}

trait TokenNerLabelCubbie extends TokenCubbie {
  val ner = StringSlot("ner")
  def newTokenNerLabel(t:Token, s:String): cc.factorie.app.nlp.ner.ChainNerLabel
  override def storeToken(t:Token): this.type = {
    super.storeToken(t)
    ner := t.nerLabel.categoryValue
    this
  }
  override def fetchToken: Token = {
    val t = super.fetchToken
    t.attr += newTokenNerLabel(t, ner.value)
    t
  }
}

trait TokenPosLabelCubbie extends TokenCubbie {
  val pos = StringSlot("pos")
  def newTokenPosLabel(t:Token, s:String): cc.factorie.app.nlp.pos.PosLabel
  override def storeToken(t:Token): this.type = {
    super.storeToken(t)
    pos:= t.posLabel.categoryValue
    this
  }
  override def fetchToken: Token = {
    val t = super.fetchToken
    t.attr += newTokenPosLabel(t, pos.value)
    t
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

