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
import cc.factorie.util.{Cubbie, Attr}
import cc.factorie.variable.{StringVariable, ChainLink, CategoricalValue}

// There are two ways to create Tokens and add them to Sentences and/or Documents:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and when Sentence is specified, Sentence length is automatically extended)

/** A word in a document, covering a substring of the Document.
    A Token is also a ChainLink in a Chain sequence; thus Tokens have "next" and "prev" methods returning neighboring Tokens.
    Token constructors that include a Section automatically add the Token to the Section (which is the Chain).
    Token constructors that include a Sentence automatically add the Token to the Sentence and its Section.
    Token constructors that include a tokenString automatically append the tokenString to the Document's string. */
class Token(val stringStart:Int, val stringEnd:Int) extends cc.factorie.app.chain.Observation[Token] with ChainLink[Token,Section] with DocumentSubstring with Attr {
  assert(stringStart <= stringEnd)
  /** Create a Token and also append it to the list of Tokens in the Section.
      There must not already be Tokens in the document with higher stringStart indices.
      Note that the start and end indices are character offsets into the Document string, not the Section string. */
  def this(sec:Section, s:Int, e:Int) = {
    this(s, e)
    assert(sec ne null)
    assert(sec.document ne null)
    assert(sec.document.annotators ne null)
    // if (!sec.document.annotators.contains(classOf[Token])) sec.document.annotators(classOf[Token]) = null // Is this really necessary?  Can we remove it for efficiency? -akm
    sec += this
  }
  /** Token constructions that defaults to placing it in the special Section that encompasses the whole Document. */
  def this(doc:Document, s:Int, e:Int) = this(doc.asSection, s, e)
  def this(sentence:Sentence, s:Int, e:Int) = {
    this(sentence.section, s, e)
    if (sentence.section.sentences.last ne sentence) throw new Error("Can only append Token to the last Sentence of the Document.")
    if (!sentence.document.annotators.contains(classOf[Token])) sentence.document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
    _sentence = sentence
    sentence.setLength(this.position - sentence.start + 1)(null)
  }
  def this(doc:Document, tokenString:String) = {
    this(doc.asSection, doc.stringLength, doc.stringLength + tokenString.length)
    doc.appendString(tokenString)
  }
  def this(s:Sentence, tokenString:String) = {
    this(s.document, tokenString)
    if (s.document.asSection.sentences.last ne s) throw new Error("Can only append of the last sentence of the Document.")
    _sentence = s
    s.setLength(this.position - s.start + 1)(null)
  }
  /** Just an alias for the "chain" method. */
  def section: Section = chain
  /** The Document containing this Token's Section. */
  def document: Document = chain.document
  /** Return the substring of the original Document string covered by the character indices stringStart to stringEnd.
      This may be different than the String returned by this.string if the TokenString attribute has been set. 
      (Such substitutions are useful for de-hyphenation, downcasing, and other such modifications. */
  def docSubstring = document.string.substring(stringStart, stringEnd)
  /** Return the string contents of this Token, either from its attr[TokenString] variable or, if unset, directly as a substring of the Document */
  def string: String = { val ts = attr[TokenString]; if (ts ne null) ts.value else docSubstring }
  /** Return the Token's string contents as a StringVariable.  Repeated calls will return the same Variable (assuming that the attr[TokenString] is not changed). */
  def stringVar: TokenString = { val ts = attr[TokenString]; if (ts ne null) ts else { val ts2 = new TokenString(this, docSubstring); attr += ts2; ts2 } }
  /** Return the string contents of this Token, either from its specified attr[C], or if unset, directly as a substring of the Document. */
  def normalizedString[C<:TokenString](attrClass:Class[C]): String = { val ts = attr(attrClass); if (ts ne null) ts.value else docSubstring }
  /** Return the lemma of the string contents of the Token, either from its attr[TokenLemma] variable or,if unset, from token.string.  */
  def lemmaString: String = { val tl = attr[cc.factorie.app.nlp.lemma.TokenLemma]; if (tl ne null) tl.value else string }
  /** Return the 0-start index of this token in its sentence.  If not part of a sentence, return -1. */
  def positionInSection: Int = position
  // TODO The ClearSegmenter should set Token._sentence, so the "sentence" method doesn't have to search for it. -akm
  def positionInSentence = if (sentence eq null) -1 else position - sentence.start // TODO Consider throwing an Error here? -akm

  // Common attributes, will return null if not present
  def posTag = attr[cc.factorie.app.nlp.pos.PennPosTag]
  def nerTag = attr[cc.factorie.app.nlp.ner.NerTag]
  def lemma = attr[cc.factorie.app.nlp.lemma.TokenLemma]
  // Parse attributes, will throw exception if parse is not present
  def parse = sentence.attr[cc.factorie.app.nlp.parse.ParseTree]
  def parseParent: Token = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].parent(positionInSentence)
  def parseParentIndex: Int = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].parentIndex(positionInSentence)
  def parseLabel: cc.factorie.app.nlp.parse.ParseTreeLabel = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].label(positionInSentence)
  def parseChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].children(positionInSentence)
  def parseLeftChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].leftChildren(positionInSentence)
  def parseRightChildren: Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].rightChildren(positionInSentence)
  def parseChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].childrenLabeled(positionInSentence, label.intValue)
  def parseLeftChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].leftChildrenLabeled(positionInSentence, label.intValue)
  def parseRightChildrenLabeled(label:CategoricalValue[String]): Seq[Token] = sentence.attr[cc.factorie.app.nlp.parse.ParseTree].rightChildrenLabeled(positionInSentence, label.intValue)
  
  // Sentence methods
  private[nlp] var _sentence: Sentence = null // This must be changeable from outside because sometimes Tokenization comes before Sentence segmentation
  def sentence: Sentence = {
    if (_sentence eq null) _sentence = section.sentences.find(_.contains(this)).getOrElse(null) // TODO Make this search more efficient
    _sentence
  }
  def sentenceHasNext: Boolean = (sentence ne null) && position+1 < sentence.end
  def sentenceHasPrev: Boolean = (sentence ne null) && position > sentence.start
  def sentenceNext: Token = if (sentenceHasNext) next else null
  def sentencePrev: Token = if (sentenceHasPrev) prev else null
  def isInSentence: Boolean = sentence ne null
  def isSentenceStart: Boolean = (sentence ne null) && sentence.start == position
  def isSentenceEnd: Boolean = (sentence ne null) && sentence.end-1 == position
  
  // Span methods.  Don't delete these yet.  Still small chance may have a canonical "SpanList" in Section.
//  def inSpan: Boolean = chain.hasSpanContaining(position) 
//  def inSpanOfClass[A<:TokenSpan](c:Class[A]): Boolean = chain.hasSpanOfClassContaining(c, position)
//  def inSpanOfClass[A<:TokenSpan](implicit m:Manifest[A]): Boolean = chain.hasSpanOfClassContaining(m.erasure.asInstanceOf[Class[A]], position)
//  def spans:Seq[TokenSpan] = chain.spansContaining(position) //.toList
//  def spansOfClass[A<:TokenSpan](c:Class[A]) = chain.spansOfClassContaining(c, position)
//  def spansOfClass[A<:TokenSpan](implicit m:Manifest[A]) = chain.spansOfClassContaining(m.erasure.asInstanceOf[Class[A]], position)
//  def startsSpans: Iterable[TokenSpan] = chain.spansStartingAt(position)
//  def startsSpansOfClass[A<:TokenSpan](implicit m:Manifest[A]): Iterable[A] = chain.spansOfClassStartingAt(position)
//  def endsSpans: Iterable[TokenSpan] = chain.spansEndingAt(position)
//  def endsSpansOfClass[A<:TokenSpan](implicit m:Manifest[A]): Iterable[A] = chain.spansOfClassEndingAt(position)
  
  // String feature help:
  def matches(t2:Token): Boolean = string == t2.string // TODO Consider renaming "stringMatches"
  /** Return true if the first character of the word is upper case. */
  def isCapitalized: Boolean = java.lang.Character.isUpperCase(string(0))
  def isPunctuation: Boolean = string.matches("\\p{Punct}+")
  /** Return true if any character of the word is lower case. */
  def containsLowerCase: Boolean = string.exists(c => java.lang.Character.isLowerCase(c))
  /** Return true if any character of the word is upper case. */
  def containsUpperCase: Boolean = string.exists(c => java.lang.Character.isUpperCase(c))
  /* Return true if the word contains only digits. */
  def isDigits: Boolean = string.matches("\\d+")
  /** Return true if the word contains at least one digit. */
  def containsDigit: Boolean = string.matches(".*\\d.*")
  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def wordShape(maxRepetitions:Int = 2): String = cc.factorie.app.strings.stringShape(string, maxRepetitions)
  /** Return all the word's character subsequences of lengths between min and max. */
  def charNGrams(min:Int, max:Int): Seq[String] = cc.factorie.app.strings.charNGrams(string, min, max)
  /** Return true if the character immediately preceding the start of this token is a whitespace character (such as space, newline, tab, etc) */
  def hasPrecedingWhitespace: Boolean = stringStart == 0 || java.lang.Character.isWhitespace(document.string(stringStart-1))
  /** Return true if the character immediately following the end of this token is a whitespace character (such as space, newline, tab, etc) */
  def hasFollowingWhitespace: Boolean = stringEnd == document.stringLength || java.lang.Character.isWhitespace(document.string(stringEnd))
  /** Return true if the character immediately preceding the start of this token is a newline.  The beginning of the document counts as a newline. */
  def followsNewline: Boolean = stringStart == 0 || document.string(stringStart-1) == '\n'
  /** Return true if the character immediately following the end of this token is a newline.  The end of the document counts as a newline. */
  def precedesNewline: Boolean = stringEnd == document.stringLength || document.string(stringEnd) == '\n'
  /** Returns a string representation of this Token object, including the prefix "Token(" and its starting character offset.
      If instead you want the string contents of the token use the method "string". */
  override def toString = "Token("+stringStart+":"+string+")"

}

/** Used as an attribute of Token when the token.string should return something 
    different than the document.string.substring at the Token's start and end positions. 
    For example, de-hyphenation may change "probab\n-ly" to "probably". */
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

trait TokenIobConllNerTagCubbie extends TokenCubbie {
  val ner = StringSlot("ner")
  def newTokenNerLabel(t:Token, s:String): cc.factorie.app.nlp.ner.IobConllNerTag
  override def storeToken(t:Token): this.type = {
    super.storeToken(t)
    ner := t.nerTag.categoryValue
    this
  }
  override def fetchToken: Token = {
    val t = super.fetchToken
    t.attr += newTokenNerLabel(t, ner.value)
    t
  }
}

trait TokenPennPosTagCubbie extends TokenCubbie {
  val pos = StringSlot("pos")
  def newTokenPosLabel(t:Token, s:String): cc.factorie.app.nlp.pos.PennPosTag
  override def storeToken(t:Token): this.type = {
    super.storeToken(t)
    pos:= t.posTag.categoryValue
    this
  }
  override def fetchToken: Token = {
    val t = super.fetchToken
    t.attr += newTokenPosLabel(t, pos.value)
    t
  }
}
