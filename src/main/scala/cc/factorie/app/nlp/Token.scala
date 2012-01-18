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
import cc.factorie.er._

// There are two ways to create Tokens and add them to Sentences and/or Documents:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and when Sentence is specified, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength?
class Token(var stringStart:Int, var stringLength:Int) extends StringVar with cc.factorie.app.chain.Observation[Token] with ChainLink[Token,Document] with Attr with Entity[Token] {
  def this(doc:Document, s:Int, l:Int) = {
    this(s, l)
    doc += this
  }
  def this(sentence:Sentence, s:Int, l:Int) = {
    this(s, l) // TODO Rather than TODO below, we should just make this line: this(sentence.document, s, l)
    if (sentence.document.sentences.last ne sentence) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = sentence
    // TODO Don't we also need to do??: doc += this
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
  def stringEnd = stringStart + stringLength
  def sentencePosition = position - sentence.start
  def value: String = string // abstract in StringVar
  
  // Common attributes, will return null if not present
  def posLabel = attr[cc.factorie.app.nlp.pos.PosLabel]
  def nerLabel = attr[cc.factorie.app.nlp.ner.ChainNerLabel]
  // Parse attributes, will throw exception if parse is not present
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
    if (_sentence == null) _sentence = document.sentenceContaining(this)
    _sentence
  }
  def indexInSentence: Int = sentence.zipWithIndex.find(_._1 == this).get._2
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
  override def toString = "Token("+stringStart+"+"+stringLength+":"+string+")"

  // factorie.er interface
  type GetterType <: TokenGetter
  class GetterClass extends TokenGetter
}


/** Implementation of the entity-relationship language we can use with Token objects. */
class TokenGetter extends EntityGetter[Token] {
  type T = Token
  //type S = Chain[S,Token]
  def newTokenGetter = new TokenGetter
  /** Go from a token to the next token. */
  def next = initManyToMany[T](newTokenGetter,
                               (token:T) => if (!token.hasNext) Nil else List(token.next), 
                               (token:T) => if (!token.hasPrev) Nil else List(token.prev))
  /** Go from a token to the previous token. */
  def prev = initManyToMany[T](newTokenGetter,
                               (token:T) => if (!token.hasPrev) Nil else List(token.prev), 
                               (token:T) => if (!token.hasNext) Nil else List(token.next))
  /** Go from a token to the collection of the next 'n' tokens. */
  def next(n:Int) = initManyToMany[T](newTokenGetter,
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret },
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret })
  /** Go from a token to the collection of the previous 'n' tokens. */
  def prev(n:Int) = initManyToMany[T](newTokenGetter,
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret },
                                      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret })
  /** All the other tokens in the Sentence. */
  def sentenceTokens = initManyToMany[T](newTokenGetter,
                                         (token:T) => token.chain, 
                                         (token:T) => token.chain)
  //def nerLabel = initOneToOne[ChainNerLabel]                                       
  /** Return a BooleanObservation with value true if the word of this Token is equal to 'w'.  
   Intended for use in tests in er.Formula, not as a feature itself.  
   If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
  def isWord(w:String) = getOneToOne[BooleanObservationWithGetter](
    // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
    token => if (token.string == w) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
    bool => throw new Error("Constant bool shouldn't change"))
  /** Return a BooleanObservation with value true if the word of this Token is capitalized.  
   Intended for use in tests in er.Formula, not as a feature itself.  
   If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
  def isCapitalized = getOneToOne[BooleanObservationWithGetter](
    // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
    token => if (java.lang.Character.isUpperCase(token.string.head)) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
    bool => throw new Error("Constant bool shouldn't change"))
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

