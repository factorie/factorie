package cc.factorie.app.nlp
import cc.factorie._
import cc.factorie.app.tokenseq._
import scala.collection.mutable.ArrayBuffer

trait Attr {
  object attr extends scala.collection.mutable.ListMap[Class[_],AnyRef] {
    def apply[C<:AnyRef]()(implicit m: Manifest[C]): C = this(m.erasure).asInstanceOf[C]
    def get[C<:AnyRef](implicit m: Manifest[C]): Option[C] = this.get(m.erasure).asInstanceOf[Option[C]]
    def +=[C<:AnyRef](value:C): Unit = this(value.getClass) = value
    def -=[C<:AnyRef](value:C): Unit = super.-=(value.getClass)
    def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = super.-=(m.erasure)
  }
}

trait ThisType[+This] {
  //this: This =>
  type ThisType = This
}

trait ElementType[+E] {
  type ElementType = E
}

trait ChainType[+C] {
  type ChainType = C
}

trait InChain[+This<:InChain[This,C],+C<:Chain[C,This]] extends ThisType[This] with ChainType[C] {
  //this: This =>
  //type ThisType = This
  var _position: Int = -1
  var _chain: scala.collection.IndexedSeq[AnyRef] = null
  def chain: ChainType = _chain.asInstanceOf[ChainType]
  def position: Int = _position
  def next: This = chain(_position+1)
  def prev: This = chain(_position-1)
  def hasPrev: Boolean = _position > 0
  def hasNext: Boolean = _position < _chain.length - 1
}

trait Chain[+This<:Chain[This,A],+A<:InChain[A,This]] extends scala.collection.IndexedSeq[A] with ThisType[This] with ElementType[A] {
  //this: This =>
  //type ElementType = A
  type _Element = ThisType#ElementType
  type _This = ThisType#ThisType
  private val _sequence = new ArrayBuffer[AnyRef]
  //def sequence: IndexedSeq[A] = _sequence.asInstanceOf[IndexedSeq[A]]
  def apply(i:Int): A = _sequence(i).asInstanceOf[A]
  def length = _sequence.length
  // TODO Try to make type of 'e' stronger without getting into contravariance trouble 
  def +=(e:InChain[_,_]): Unit = {
    e._position = _sequence.length
    e._chain = this
    _sequence += e.asInstanceOf[InChain[A,This]]
  }
}

trait ChainInChain[+This<:ChainInChain[This,E,S],+E<:InChain[E,This],+S<:Chain[S,This]] extends InChain[This,S] with Chain[This,E] {
  this: This =>
}

//trait ChainWithSpans[+This<:ChainWithSpans[This,S,E],+S,+E<:InChain[E,This]] extends Chain[This,E]

// There are two ways to create/add Tokens/Sentences:
// Without String arguments, in which case the string is assumed to already be in the Document
// With String arguments, in which case the string is appended to the Document (and for Tokens, Sentence length is automatically extended)

// TODO Consider stringEnd instead of stringLength
class Token(var stringStart:Int, var stringLength:Int) extends StringVar with InChain[Token,Sentence] with Attr {
  def this(sentence:Sentence, s:Int, l:Int) = { this(s, l); sentence += this }
  def this(sentence:Sentence, tokenString:String) = {
    this(sentence.stringLength, tokenString.length)
    sentence.document.appendString(tokenString)
    //sentence.document.appendString(" ") // TODO Make this configurable
    sentence.stringLength += tokenString.length + 1
  }
  def sentence: ChainType = chain
  def string = sentence.document.string.substring(stringStart, stringStart + stringLength)
  def value: String = string // abstract in StringVar
}

class TokenSpan(val sentence:Sentence, initialStart:Int, initialLength:Int) extends ElementType[Token] with ChainType[Sentence] with IndexedSeq[Token] {
  private var _start = initialStart
  private var _length = initialLength
  def start = _start
  def length = _length
  def apply(i:Int): ElementType = sentence(initialStart+i)
  def phrase = if (length == 1) this.head.toString else this.mkString(" ")
}

class Sentence(var stringStart:Int, var stringLength:Int) extends StringVar with ChainInChain[Sentence,Token,Document] {
  def this(doc:Document) = { this(doc.stringLength, 0); doc += this }
  def this(doc:Document, sentenceString:String) = { this(doc.stringLength, sentenceString.length); doc.appendString(sentenceString) }
  def this(doc:Document, stringStart:Int, stringLength:Int) = { this(stringStart, stringLength); doc += this }
  def document: ChainType = chain
  def string: String = document.string.substring(stringStart, stringLength)
  def value: String = string
  def tokens: IndexedSeq[ElementType] = this
  def tokenAt(charOffset:Int): ElementType = {
    require(charOffset >= stringStart && charOffset <= stringStart + stringLength)
    var i = 0 // TODO Implement as faster binary search
    while (i < this.length && this(i).stringStart <= charOffset) {
      val token = this(i)
      if (token.stringStart <= charOffset && token.stringStart + token.stringLength <= charOffset) return token
      i += 1
    }
    return null
  }
}

// TODO Consider putting Tokens directly in Document, and still implementing the ability to get the Tokens in a sentence separately

class Document(val name:String, strValue:String = "") extends StringVar with Chain[Document,Sentence] {
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
  def value: String = string
  def stringLength: Int = if (_string ne null) _string.length else _stringbuf.length
  def sentences: IndexedSeq[ElementType] = this
  def tokens: IndexedSeq[ElementType#ElementType] = this.flatMap(_.tokens)
}

object Document {
  /** Create a new document using a tokenizing regex.  Currently all tokens in document contained in one sentence. */
  def apply(name:String, contents:String): Document = {
    val doc = new Document(name, contents)
    val sentence = new Sentence(doc, 0, contents.length) // TODO Implement sentence segmentation
    val tokenIterator = cc.factorie.app.strings.nonWhitespaceClassesSegmenter/*.regex.findAllIn*/(contents)
    for (tokenString <- tokenIterator)
      sentence += new Token(tokenIterator.start, tokenIterator.end - tokenIterator.start)
    doc
  }
}

