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
import cc.factorie.app.tokenseq._
import scala.collection.mutable.ArrayBuffer


trait SpanType[+S<:AnyRef] {
  type SpanType = S
}

trait Span[This<:Span[This,C,E],C<:ChainWithSpans[C,This,E],E<:InChain[E,C]] extends IndexedSeq[E] with ThisType[This] with ElementType[E] with ChainType[C] {
  this: This =>
  protected var _start = 0
  protected var _length = 0
  var _chain: C = null.asInstanceOf[C] //ChainWithSpans[C,This,E] = null // Set automatically in ChainWithSpans.+= and -=
  /** True if this span is currently present in a ChainWithSpans.  Used by Diff objects to handle deleted spans. */
  def present = _chain ne null
  def start: Int = _start
  def length: Int = _length
  def chain: C = _chain
  def end = start + length - 1
  override def iterator = new Iterator[E] {
    var i = start
    def hasNext = i < start + _length
    def next: ElementType = { i += 1; Span.this.chain.apply(i - 1) }
  }
  def apply(i: Int) = chain(i + start)
  def isAtStart = start == 0
  def overlaps(that: Span[_,_<:AnyRef,_<:AnyRef]) = {
    assert(this.chain eq that.chain)
    (that.start <= this.start && that.end >= this.start) ||
    (this.start <= that.start && this.end >= that.start)
  }
  def isAtEnd = start + length == chain.length
  def hasSuccessor(i: Int) = (start + length - 1 + i) < chain.length
  def hasPredecessor(i: Int) = (start - i) >= 0
  def successor(i: Int): E = if (hasSuccessor(i)) chain(start + length - 1 + i) else null.asInstanceOf[E]
  def predecessor(i: Int): E = if (hasPredecessor(i)) chain(start - i) else null.asInstanceOf[E]
  def prevWindow(n:Int): Seq[E] = for (i <- math.max(0,start-n) until start) yield chain(i)
  def nextWindow(n:Int): Seq[E] = for (i <- end+1 until math.min(seq.length-1,end+n)) yield chain(i)
  def window(n:Int): Seq[E] = for (i <- math.max(0,start-n) to math.min(seq.length-1,end+n)) yield chain(i)
  def windowWithoutSelf(n:Int): Seq[E] = for (i <- math.max(0,start-n) to math.min(seq.length-1,end+n); if (i < start || i > end)) yield chain(i)
  // Support for next/prev of elements within a span
  @inline private def checkInSpan(elt:E): Unit = { require(elt.chain eq chain); require(elt.position >= start && elt.position <= end) } 
  def hasNext(elt:E): Boolean = { checkInSpan(elt); elt.position < end }
  def hasPrev(elt:E): Boolean = { checkInSpan(elt); elt.position > start }
  def next(elt:E): E = if (hasNext(elt)) elt.next else null.asInstanceOf[E]
  def prev(elt:E): E = if (hasPrev(elt)) elt.prev else null.asInstanceOf[E]
}

trait SpanVar[This<:SpanVar[This,C,E],C<:ChainWithSpansVar[C,This,E],E<:InChain[E,C]] extends Span[This,C,E] with IndexedSeqVar[E] with VarAndValueGenericDomain[SpanVar[This,C,E],IndexedSeq[E]] {
  this: This =>
  def value: IndexedSeq[ElementType] = this
  /** If true, this SpanVariable will be scored by a difflist, even if it is in its deleted non-"present" state. */
  def diffIfNotPresent = false
  def preChange(implicit d:DiffList): Unit = {}
  def postChange(implicit d:DiffList): Unit = {}
  def delete(implicit d: DiffList): Unit = { preChange; chain.removeSpan(this)(d); postChange }
  def setLength(l: Int)(implicit d: DiffList): Unit = if (l != length) { preChange; new SetLength(_length, l); postChange }
  def trimStart(n: Int)(implicit d: DiffList): Unit = if (n >= length) this.delete else if (n > 0) { preChange; new TrimStart(n); postChange }
  def trimEnd(n: Int)(implicit d: DiffList): Unit = if (n >= length) this.delete else if (n > 0) { preChange; new TrimEnd(n); postChange }
  def prepend(n: Int)(implicit d: DiffList): Unit = if (n > 0) { preChange; new Prepend(n); postChange }
  def append(n: Int)(implicit d: DiffList): Unit = if (n > 0) { preChange; new Append(n); postChange }
  def canPrepend(n: Int) = _start >= n
  def canAppend(n: Int) = _start + _length + n <= chain.length
  /** This should be called in the constructor */
  // Why do I need this and also the AddSpan Diff??
  /*case class NewSpan(implicit d: DiffList) extends Diff {
    // NewSpan cannot be an AutoDiff because of initialization ordering, done will end up false. 
    // TODO But I should get rid of 'done' and just use 'present' instead.
    //println("NewSpanVariable d.length="+d.length)
    var done = false
    if (d != null) d += this
    redo
    def variable = {if (done || diffIfNotPresent) SpanVar.this else null}
    def redo = {assert(!done); done = true; assert(present) }
    def undo = {assert(done); done = false; assert(!present) }
    override def toString = "NewSpan("+SpanVar.this+")"
  }*/
  /*@deprecated("Remove") case class DeleteSpanVariable(implicit d: DiffList) extends Diff {
    // cannot be AutoDiff for same reasons as NewSpanVariable
    var done = false
    if (d != null) d += this
    redo
    def variable: SpanInChainVar[T] = if (done && !diffIfNotPresent) null else SpanInChainVar.this
    def redo = { assert(!done); done = true; assert(!present) }
    def undo = { assert(done); done = false; assert(present) }
    override def toString = "DeleteSpanVariable("+SpanInChainVar.this+")"
  }*/
  case class SetStart(oldStart: Int, newStart: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = _start = newStart
    def undo = _start = oldStart
  }
  case class SetLength(oldLength: Int, newLength: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = _length = newLength
    def undo = _length = oldLength
  }
  case class TrimStart(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = {assert(n < _length); _start += n; _length -= n}
    def undo = {assert(_start - n >= 0); _start -= n; _length += n}
    override def toString = "TrimStart("+n+","+SpanVar.this+")"
  }
  case class TrimEnd(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = {assert(n < _length); _length -= n}
    def undo = _length += n
    override def toString = "TrimEnd("+n+","+SpanVar.this+")"
  }
  case class Prepend(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = {assert(canPrepend(n)); _start -= n; _length += n}
    def undo = {_start += n; _length -= n}
  }
  case class Append(n: Int)(implicit d: DiffList) extends AutoDiff {
    //if (!canAppend(n)) { println("Append n="+n+" start="+variable.start+" length="+variable.length+" parent.length="+variable.parent.length) }
    def variable = if (present || diffIfNotPresent) SpanVar.this else null
    def redo = {assert(canAppend(n)); _length += n}
    def undo = _length -= n
    //override def toString = "Append("+n+","+(how do I reliably get the appended token)+")"
  }  
}

class SpanVariable[This<:SpanVar[This,C,E],C<:ChainWithSpansVar[C,This,E],E<:InChain[E,C]](theChain:C, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends SpanVar[This,C,E] {
  this: This =>
  _start = initialStart
  _length = initialLength
  _chain = theChain
  _chain.addSpan(this)(d)
  //if (d ne null) NewSpan // Add NewSpan diff to the DiffList
}

trait ChainWithSpans[This<:ChainWithSpans[This,S,E],S<:Span[S,This,E],E<:InChain[E,This]] extends Chain[This,E] with SpanType[S] {
  this: This =>
  //type Span = ThisType#SpanType
  private val _spans = new scala.collection.mutable.ListBuffer[Span[S,This,E]];
  def spans: Seq[S] = _spans.asInstanceOf[Seq[S]]
  def +=(s:S): Unit = { _spans.prepend(s); s._chain = this }
  def -=(s:S): Unit = { _spans -= s; s._chain = null.asInstanceOf[This] }
  def orderedSpans: Seq[S] = spans.toList.sortWith((s1,s2) => s1.start < s2.start) // TODO Make this more efficient by avoiding toList
  def spansContaining(position: Int): Seq[S] = spans.filter(s => s.start <= position && position < (s.start + s.length))
  def spansStartingAt(position: Int): Seq[S] = spans.filter(s => s.start == position)
  def spansEndingAt(position: Int): Seq[S] = spans.filter(s => s.start + s.length - 1 == position)
}

trait ChainWithSpansVar[This<:ChainWithSpansVar[This,S,E],S<:SpanVar[S,This,E],E<:InChain[E,This]] extends ChainVar[This,E] with ChainWithSpans[This,S,E] with IndexedSeqVar[E] with VarAndElementType[ChainWithSpansVar[This,S,E],E]{
  this: This =>
  /** Add the span to the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from the SpanVariable constructor. */
  def addSpan(s:S)(implicit d:DiffList): Unit = {
    //require(s.seq == this, "VariableSeqWithSpans.addSpan: span.seq="+s.seq+" != this="+this)
    AddSpanVariable(s)
  }
  /** Remove the span from the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from SpanVariable.delete. */
  def removeSpan(s:S)(implicit d:DiffList): Unit = {
    require(s.chain == this)
    RemoveSpanVariable(s)
  }
  def clearSpans(implicit d:DiffList): Unit = {
    // Make a copy of the collection of spans so its iterator doesn't get confused as we delete them
    spans.toList.foreach(removeSpan(_)(d))
  }
  case class AddSpanVariable(span:S)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: S = if (done) span else null.asInstanceOf[S]
    def redo = { ChainWithSpansVar.this.+=(span); assert(!done); done = true }
    def undo = { ChainWithSpansVar.this.-=(span); assert(done); done = false }
    override def toString = "AddSpanVariable("+span+")"
  }
  case class RemoveSpanVariable(span:S)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: S = if (done) null.asInstanceOf[S] else span
    def redo = { ChainWithSpansVar.this.-=(span); assert(!done); done = true }
    def undo = { ChainWithSpansVar.this.+=(span); assert(done); done = false }
    override def toString = "RemoveSpanVariable("+span+")"
  }
}

// There are two ways to create Tokens and add them to Sentences and/or Documents:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and when Sentence is specified, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength?
class Token(var stringStart:Int, var stringLength:Int) extends StringVar with InChain[Token,Document] with Attr with cc.factorie.app.chain.Observation[Token] {
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
  def nerLabel = attr[cc.factorie.app.nlp.ner.NerLabel]
  
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
  def spans:Seq[TokenSpan] = chain.spansContaining(position).toList
  def startsSpans: Iterable[TokenSpan] = chain.spansStartingAt(position)
  def endsSpans: Iterable[TokenSpan] = chain.spansEndingAt(position)
  
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
}

class TokenSpan(doc:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends SpanVariable[TokenSpan,Document,Token](doc, initialStart, initialLength) with Attr {
  def document = chain
  def phrase = if (length == 1) this.head.toString else this.mkString(" ")
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
  override def toString = "TokenSpan("+length+","+attr[cc.factorie.app.nlp.ner.NerLabel].categoryValue+":"+this.phrase+")"
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
      s._chain = this
    }
    case s:TokenSpan => super.+=(s)
  }
  override def -=(s:TokenSpan): Unit = s match {
    case s:Sentence => _sentences -= s
    case s:TokenSpan => super.+=(s)
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

