package cc.factorie.app.nlp
import cc.factorie._
import cc.factorie.app.tokenseq._
import scala.collection.mutable.ArrayBuffer

/** Provides member "attr" which is a map from class to an attribute value (instance of that class).
    For example: object foo extends Attr; foo.attr += "bar"; require(foo.attr[String] == "bar"); foo.attr -= "bar" */
trait Attr {
  object attr extends scala.collection.mutable.ListMap[Class[_],AnyRef] {
    def apply[C<:AnyRef]()(implicit m: Manifest[C]): C = this(m.erasure).asInstanceOf[C]
    def get[C<:AnyRef](implicit m: Manifest[C]): Option[C] = this.get(m.erasure).asInstanceOf[Option[C]]
    def +=[C<:AnyRef](value:C): Unit = this(value.getClass) = value
    def -=[C<:AnyRef](value:C): Unit = super.-=(value.getClass)
    def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = super.-=(m.erasure)
  }
}

trait ThisType[+This<:AnyRef] {
  //this: This =>
  type ThisType = This
}

/*trait ElementType[+E<:AnyRef] {
  type ElementType = E
}*/

trait ChainType[+C<:AnyRef] {
  type ChainType = C
}

/** An element or "link" of a Chain, having methods "next", "prev", etc. */
trait InChain[+This<:InChain[This,C],+C<:Chain[C,This]] extends ThisType[This] with ChainType[C] {
  this: This =>
  var _position: Int = -1
  var _chain: scala.collection.IndexedSeq[AnyRef] = null
  def chain: ChainType = _chain.asInstanceOf[ChainType]
  def position: Int = _position
  
  def hasNext = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else _chain != null && _position + 1 < _chain.length
  def next: This = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (_position + 1 < _chain.length) chain(_position + 1) else null.asInstanceOf[This]
  def hasPrev = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else _chain != null && _position > 0
  def prev: This = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (_position > 0) chain(_position - 1) else null.asInstanceOf[This]
  def next(n:Int): This = { 
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set")
    val i = _position + n
    if (i >= 0 && i < _chain.length) chain(i) else null.asInstanceOf[This]
  }
  def prev(n:Int): This = {
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") 
    val i = _position - n
    if (i >= 0 && i < _chain.length) chain(i) else null.asInstanceOf[This]
  }
  def seqAfter = chain.drop(_position+1)
  def seqBefore = chain.take(_position)
  def prevWindow(n:Int): Seq[This] = for (i <- math.max(_position-n, 0) to math.max(_position-1,0)) yield chain(i)
  def nextWindow(n:Int): Seq[This] = for (i <- math.min(_position+1, _chain.length-1) to math.min(_position+n, _chain.length-1)) yield chain(i)
  def window(n:Int): Seq[This] = for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1)) yield chain(i)
  def windowWithoutSelf(n:Int): Seq[This] = for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1); if (i != _position)) yield chain(i)
  def between(other:InChain[_,_]): Seq[This] = {
    assert (other.chain == chain)
    if (other.position > _position)
      for (i <- _position until other.position) yield chain(i)
    else
      for (i <- other.position until _position) yield chain(i)
  } 
  def firstInSeq = chain(0)
  /*class InSubChain[+This2<:InSubChain[This,C2],+C2<:Chain[C2,This2]](val subchain:C) extends InChain[This2,C2] {
    this: This2 =>
    def elt: This = InChain.this
  }*/
}

/** A chain of elements, each of which has methods "next", "prev", etc */
trait Chain[+This<:Chain[This,E],+E<:InChain[E,This]] extends IndexedSeqEqualsEq[E] with ThisType[This] with ElementType[E] {
  //this: This =>
  private val _sequence = new ArrayBuffer[AnyRef]
  def apply(i:Int): ElementType = _sequence(i).asInstanceOf[ElementType]
  def length = _sequence.length
  // TODO Try to make type of 'e' stronger without getting into contravariance trouble 
  def +=(e:InChain[_,_]): Unit = {
    e._position = _sequence.length
    e._chain = this
    _sequence += e.asInstanceOf[InChain[ElementType,This]]
  }
}

/** A Chain that is also a Variable, with value Seq[ElementType] */
trait ChainVar[+This<:Chain[This,E],+E<:InChain[E,This]] extends Chain[This,E] with IndexedSeqVar[E]

/** A Chain which itself is also an element of an outer Chain */
trait ChainInChain[+This<:ChainInChain[This,E,S],+E<:InChain[E,This],+S<:Chain[S,This]] extends InChain[This,S] with Chain[This,E] {
  this: This =>
}

trait SpanType[+S<:AnyRef] {
  type SpanType = S
}

//trait Span[This<:Span[This,C,E],+C<:Chain[C,E],+E<:InChain[E,C]]

trait SpanInChain[This<:SpanInChain[This,C,E],+C<:ChainWithSpans[C,This,E],+E<:InChain[E,C]] extends IndexedSeq[E] with ThisType[This] with ElementType[E] with ChainType[C] {
  type Element = ThisType#ElementType
  protected var _start = 0
  protected var _length = 0
  var _chain: ChainWithSpans[_,This,_] = null // Set automatically in ChainWithSpans.+= and -=
  /** True if this span is currently present in a ChainWithSpans.  Used by Diff objects to handle deleted spans. */
  def present = _chain ne null
  def start: Int = _start
  def length: Int = _length
  def chain: ChainType = _chain.asInstanceOf[ChainType]
  def end = start + length - 1
  override def iterator = new Iterator[E] {
    var i = start
    def hasNext = i < start + length
    def next: ElementType = { i += 1; SpanInChain.this.chain.apply(i - 1) }
  }
  def apply(i: Int) = chain(i + start)
  def isAtStart = start == 0
  def overlaps(that: SpanInChain[_,_<:AnyRef,_<:AnyRef]) = {
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
  //def hasNext(elt:Element): Boolean = { require(elt.chain eq chain); elt.position < end }
  //def hasPrev(elt:Element): Boolean = { require(elt.chain eq chain); elt.position > start }
  //def next(elt:ElementType): E = if (hasNext(elt)) elt.next else null.asInstanceOf[E]
  //def prev(elt:ElementType): E = if (hasPrev(elt)) elt.prev else null.asInstanceOf[E]
}

trait SpanInChainVar[This<:SpanInChainVar[This,C,E],+C<:ChainWithSpansVar[C,This,E],+E<:InChain[E,C]] extends SpanInChain[This,C,E] with IndexedSeqVar[E] {
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
  case class NewSpan(implicit d: DiffList) extends Diff {
    // NewSpanVariable cannot be an AutoDiff because of initialization ordering, done will end up false. 
    // TODO But I should get rid of 'done' and just use 'present' instead.
    //println("NewSpanVariable d.length="+d.length)
    var done = false
    if (d != null) d += this
    redo
    def variable = {if (done || diffIfNotPresent) SpanInChainVar.this else null}
    def redo = {assert(!done); done = true; assert(present) }
    def undo = {assert(done); done = false; assert(!present) }
    override def toString = "NewSpan("+SpanInChainVar.this+")"
  }
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
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = _start = newStart
    def undo = _start = oldStart
  }
  case class SetLength(oldLength: Int, newLength: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = _length = newLength
    def undo = _length = oldLength
  }
  case class TrimStart(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = {assert(n < _length); _start += n; _length -= n}
    def undo = {assert(_start - n >= 0); _start -= n; _length += n}
    override def toString = "TrimStart("+n+","+SpanInChainVar.this+")"
  }
  case class TrimEnd(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = {assert(n < _length); _length -= n}
    def undo = _length += n
    override def toString = "TrimEnd("+n+","+SpanInChainVar.this+")"
  }
  case class Prepend(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = {assert(canPrepend(n)); _start -= n; _length += n}
    def undo = {_start += n; _length -= n}
  }
  case class Append(n: Int)(implicit d: DiffList) extends AutoDiff {
    //if (!canAppend(n)) { println("Append n="+n+" start="+variable.start+" length="+variable.length+" parent.length="+variable.parent.length) }
    def variable = if (present || diffIfNotPresent) SpanInChainVar.this else null
    def redo = {assert(canAppend(n)); _length += n}
    def undo = _length -= n
    //override def toString = "Append("+n+","+(how do I reliably get the appended token)+")"
  }  
}

trait ChainWithSpans[+This<:ChainWithSpans[This,S,E],S<:SpanInChain[S,This,E],+E<:InChain[E,This]] extends Chain[This,E] with SpanType[S] {
  this: This =>
  type Span = ThisType#SpanType
  private val _spans = new scala.collection.mutable.ListBuffer[SpanInChain[Span,_,_]];
  def spans: Seq[Span] = _spans.asInstanceOf[Seq[S]]
  def +=(s:Span): Unit = { _spans.prepend(s); s._chain = this }
  def -=(s:Span): Unit = { _spans -= s; s._chain = null.asInstanceOf[This] }
  def orderedSpans: Seq[S] = spans.toList.sortWith((s1,s2) => s1.start < s2.start) // TODO Make this more efficient by avoiding toList
  def spansContaining(position: Int): Seq[S] = spans.filter(s => s.start <= position && position < (s.start + s.length))
  def spansStartingAt(position: Int): Seq[S] = spans.filter(s => s.start == position)
  def spansEndingAt(position: Int): Seq[S] = spans.filter(s => s.start + s.length - 1 == position)
}

trait ChainWithSpansVar[+This<:ChainWithSpansVar[This,S,E],S<:SpanInChainVar[S,This,E],+E<:InChain[E,This]] extends ChainWithSpans[This,S,E] with IndexedSeqVar[E] with VarAndElementType[ChainWithSpansVar[This,S,E],E]{
  this: This =>
  //type Span = VariableType#SpanType
  def value: IndexedSeq[E] = this.toIndexedSeq // TODO Consider making a copy of the sequence so that the returned value stays constant even this changes later? 
  /** Add the span to the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from the SpanVariable constructor. */
  def addSpan(s:Span)(implicit d:DiffList): Unit = {
    //require(s.seq == this, "VariableSeqWithSpans.addSpan: span.seq="+s.seq+" != this="+this)
    AddSpanVariable(s)
  }
  /** Remove the span from the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from SpanVariable.delete. */
  def removeSpan(s:Span)(implicit d:DiffList): Unit = {
    require(s.chain == this)
    RemoveSpanVariable(s)
  }
  def clearSpans: Unit = {
    // Make a copy of the collection of spans so its iterator doesn't get confused as we delete them
    spans.toList.foreach(removeSpan(_)(null))
  }
  case class AddSpanVariable(span:Span)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: Span = if (done) span else null.asInstanceOf[S]
    def redo = { ChainWithSpansVar.this.+=(span); assert(!done); done = true }
    def undo = { ChainWithSpansVar.this.-=(span); assert(done); done = false }
    override def toString = "AddSpanVariable("+span+")"
  }
  case class RemoveSpanVariable(span:Span)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: Span = if (done) null.asInstanceOf[S] else span
    def redo = { ChainWithSpansVar.this.-=(span); assert(!done); done = true }
    def undo = { ChainWithSpansVar.this.+=(span); assert(done); done = false }
    override def toString = "RemoveSpanVariable("+span+")"
  }
}

// There are two ways to create/add Tokens/Sentences:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and for Tokens, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength
class Token(var stringStart:Int, var stringLength:Int) extends StringVar with InChain[Token,Document] with Attr {
  def this(doc:Document, s:Int, l:Int) = {
    this(s, l)
    doc += this
  }
  def this(doc:Document, tokenString:String) = {
    this(doc, doc.stringLength, tokenString.length)
    doc.appendString(tokenString)
    //doc.appendString(" ") // TODO Make this configurable
  }
  def this(s:Sentence, tokenString:String) = {
    this(s.document, s.document.stringLength, tokenString.length)
    if (s.document.sentences.last ne s) throw new Error("Can only append of the last sentence of the Document.")
    s.document.appendString(tokenString)
    //doc.appendString(" ") // TODO Make this configurable
    s.setLength(this.position - s.start)(null)
  }
  def document: ChainType = chain
  def string = document.string.substring(stringStart, stringStart + stringLength)
  def value: String = string // abstract in StringVar
  private var _sentence: Sentence = null
  def sentence = _sentence
  //def sentenceNext: ThisType 
}

class TokenSpan(val document:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends SpanInChainVar[TokenSpan,Document,Token] {
  _start = initialStart
  _length = initialLength
  document.addSpan(this)(d)
  NewSpan // Add NewSpan diff to the DiffList
  _start = initialStart
  _length = initialLength
  //def apply(i:Int): ElementType = throw new Error // document(initialStart+i)
  def phrase = if (length == 1) this.head.toString else this.mkString(" ")
}

class Sentence(doc:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends TokenSpan(doc, initialStart, initialLength) {
  def this(doc:Document)(implicit d:DiffList = null) = this(doc, doc.tokens.length, 0)
  def tokens: IndexedSeq[ElementType] = this 
  def tokenAt(charOffset:Int): ElementType = {
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
class Document(val name:String, strValue:String = "") extends ChainWithSpansVar[Document,TokenSpan,Token] {
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
  override def +=(s:Span): Unit = s match {
    case s:Sentence => {
      if (_sentences.length == 0 || _sentences.last.end >= s.start) _sentences += s
      else throw new Error("Sentences must be added in order.")
      s._chain = this
    }
    case s:Span => super.+=(s)
  }
  override def -=(s:Span): Unit = s match {
    case s:Sentence => _sentences -= s
    case s:Span => super.+=(s)
  }
  
  def tokens: IndexedSeq[ElementType] = this
  private var _sentences = new ArrayBuffer[Sentence]
  def sentences: Seq[Sentence] = _sentences
}

object Document {
  /** Create a new document using a tokenizing regex.  Currently all tokens in document contained in one sentence. */
  def apply(name:String, contents:String): Document = {
    val doc = new Document(name, contents)
    val tokenIterator = cc.factorie.app.strings.nonWhitespaceClassesSegmenter/*.regex.findAllIn*/(contents)
    for (tokenString <- tokenIterator)
      doc += new Token(tokenIterator.start, tokenIterator.end - tokenIterator.start)
    doc
  }
}

