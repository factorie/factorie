package cc.factorie.app.nlp
import cc.factorie._
import cc.factorie.app.tokenseq._
import scala.collection.mutable.ArrayBuffer

// TODO Rename AttrClasses
class ClassMap {
  private val map = new scala.collection.mutable.ListMap[Class[_],AnyRef]
  def apply[C<:AnyRef]()(implicit m: Manifest[C]): C = map(m.erasure).asInstanceOf[C]
  def attr[C<:AnyRef](implicit m: Manifest[C]): C = map(m.erasure).asInstanceOf[C]
  def addAttr[C<:AnyRef](c:C)(implicit m: Manifest[C]): Unit = map(m.erasure) = c
  def get[C<:AnyRef](implicit m: Manifest[C]): Option[C] = map.get(m.erasure).asInstanceOf[Option[C]]
  def +=[C<:AnyRef](value:C): Unit = map(value.getClass) = value
  def -=[C<:AnyRef](value:C): Unit = map -= value.getClass
  def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = map -= m.erasure
}

trait Attr {
  object attr extends scala.collection.mutable.ListMap[Class[_],AnyRef] {
    def apply[C<:AnyRef]()(implicit m: Manifest[C]): C = this(m.erasure).asInstanceOf[C]
    def get[C<:AnyRef](implicit m: Manifest[C]): Option[C] = this.get(m.erasure).asInstanceOf[Option[C]]
    def +=[C<:AnyRef](value:C): Unit = this(value.getClass) = value
    def -=[C<:AnyRef](value:C): Unit = super.-=(value.getClass)
    def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = super.-=(m.erasure)
  }
}

object Attr {
  val c = 1.getClass
}

trait ThisType[+This] {
  //this: This =>
  type ThisType = This
}

trait ElementType[+E] {
  type ElementType = E
}

trait InChain[+This<:InChain[This,S],+S<:Chain[S,This]] extends ThisType[This] {
  //this: This =>
  //type ThisType = This
  type ChainType = S
  var _position: Int = -1
  var _chain: scala.collection.IndexedSeq[AnyRef] = null
  def chain: S = _chain.asInstanceOf[S]
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
  def +=(a:InChain[_,_]): Unit = {
    a._position = _sequence.length
    a._chain = this
    _sequence += a.asInstanceOf[InChain[A,This]]
  }
}

trait ChainInChain[+This<:ChainInChain[This,E,S],+E<:InChain[E,This],+S<:Chain[S,This]] extends InChain[This,S] with Chain[This,E] {
  this: This =>
}

//trait ChainWithSpans[+This<:ChainWithSpans[This,S,E],+S,+E<:InChain[E,This]] extends Chain[This,E]

class Token(var stringStart:Int, var stringLength:Int) extends StringVar with InChain[Token,Sentence] with Attr {
  def this(sentence:Sentence, s:Int, l:Int) = { this(s, l); sentence += this }
  def this(sentence:Sentence, tokenString:String) = {
    this(sentence.stringLength, tokenString.length)
    sentence.document.appendString(tokenString)
    sentence.document.appendString(" ") // TODO Make this configurable
    sentence.stringLength += tokenString.length + 1
  }
  def sentence: ChainType = chain
  def string = sentence.document.string.substring(stringStart, stringStart + stringLength)
  def value: String = string // abstract in StringVar
}

class Sentence(var stringStart:Int, var stringLength:Int) extends StringVar with ChainInChain[Sentence,Token,Document] {
  def this(doc:Document) = { this(doc.stringLength, 0); doc += this }
  def this(doc:Document, sentenceString:String) = { this(doc.stringLength, sentenceString.length); doc.appendString(sentenceString) }
  type TokenType <: Token
  def document: ChainType = chain
  def value: String = document.string.substring(stringStart, stringLength)
  def tokens: IndexedSeq[ElementType] = this
}

class Document(val name:String, strValue:String = "") extends StringVar with Chain[Document,Sentence] {
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
}

object Document {
  def apply(name:String, contents:String): Document = {
    val doc = new Document(name, contents)
    val sentence = new Sentence(0, contents.length)
    doc += sentence
    val tokenIterator = "[A-Za-z]+".r.findAllIn(contents)
    for (tokenString <- tokenIterator)
      sentence += new Token(tokenIterator.start, tokenIterator.end - tokenIterator.start)
    doc += (sentence)
    doc
  }
}

object Test {
  object PosDomain extends CategoricalDomain[String]
  //class Token1(s:Int, l:Int) extends Token(s, l) with PosToken with InChain[Token1,Sentence1] { def posDomain = PosDomain }
  //class Sentence1(s:Int, l:Int) extends Sentence(s, l) with ChainInChain[Sentence1,Token1,Document1]
  //class Document1(n:String, s:String) extends Document(n,s) with Chain[Document1,Sentence1]
  def main(args:Array[String]): Unit = {
    
  }

}