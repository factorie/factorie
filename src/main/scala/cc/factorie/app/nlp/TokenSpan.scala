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

class TokenSpan(doc:Document, initialStart:Int, initialLength:Int)(implicit d:DiffList = null) extends SpanVariable[TokenSpan,Document,Token](doc, initialStart, initialLength) with Attr {
  def document = chain // Just a convenient alias
  def sentence = this(0).sentence
  def phrase: String = if (length == 1) this.head.string else doc.string.substring(head.stringStart, last.stringEnd)
  def string: String = phrase
  /** Returns true if this span contain the words of argument span in order. */
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
  override def toString = "TokenSpan("+start+":"+this.phrase+")"
  /** A short name for this span */
  def name: String = attr.values.head match {
    case label:LabelVariable[String] => label.categoryValue
    case x => x.toString
  }
  
}


// Cubbie storage

/*class TokenSpanCubbie(storing:TokenSpan) extends Cubbie(storing) {
  if (storing ne null) {
    start := storing.start
    length := storing.length
  }
  val start = IntSlot("start")
  val length = IntSlot("length")
  def newObject(doc:Document, start:Int, length:Int): TokenSpan = new TokenSpan(doc, start, length)(null)
  def fetch(doc:Document): TokenSpan = {
    val ts = newObject(doc, start.value, length.value)
    ts
  }
  // A version that gets the Document from the cubbie.doc RefSlot
  def fetch(/*implicit cr:CubbieRefs*/): TokenSpan = {
    throw new Error("Not yet implemented")
    val ts = newObject(null, start.value, length.value)
    ts
  }
}*/

abstract class ObjectCubbie extends Cubbie {
  type A <: AnyRef
  val storeHooks = new cc.factorie.util.Hooks1[A]
  val fetchHooks = new cc.factorie.util.Hooks1[A]
  final def store4(a:A): this.type = {
	storeHooks(a)
	this
  }
  final def fetch(a:A): Unit = {
    fetchHooks(a)
  }
  protected def store1(o:A): Unit = {}
  final def store(o:AnyRef): this.type = {
    store1(o.asInstanceOf[A])
    this
  }
  //def frob: Unit = store1(null)
}

class TokenSpanCubbie extends ObjectCubbie {
  type A <: TokenSpan
  val start = IntSlot("start")
  val length = IntSlot("length")
  override protected def store1(a:A): Unit = {
    super.store1(a)
	start := a.start
	length := a.length
  }  
  def store2(o:AnyRef): this.type = {
    //super.store(o)
    o match {
      case ts:TokenSpan => {
        start := ts.start
        length := ts.length
      }
    }
    this
  }

}

object TestTokenSpanCubbie {
  //val t = new TokenSpan(new Document("foo"), 0, 2)
  //val c = new TokenSpanCubbie().store(t)
}

//class TokenSpanCubbie extends Cubbie {
//  val start = IntSlot("start")
//  val length = IntSlot("length")
//  def newObject(doc:Document, start:Int, length:Int): TokenSpan = new TokenSpan(doc, start, length)(null)
//  def store(ts:TokenSpan): this.type = {
//    start := ts.start
//    length:= ts.length
//    this
//  }
//  final def fetch(doc:Document): TokenSpan = {
//    val ts = newObject(doc, start.value, length.value)
//    init(ts)
//    ts
//  }
//  def init(ts:TokenSpan): Unit = {}
//  // A version that gets the Document from the cubbie.doc RefSlot
//  def fetch(/*implicit cr:CubbieRefs*/): TokenSpan = {
//    throw new Error("Not yet implemented")
//    val ts = newObject(null, start.value, length.value)
//    ts
//  }
//}

trait TokenSpanWithDocRefCubbie[DC<:DocumentCubbie[_,_,_]] extends TokenSpanCubbie {
  def newDocumentCubbie: DC
  val doc = RefSlot("doc", ()=>newDocumentCubbie)
  def store3(o:AnyRef): this.type = {
    super.store(o)
    doc := o.asInstanceOf[TokenSpan].document.name
    this
  }
}

/* ACS: Commented out non-compiling code when integrating mongo-cubbie w/factorie 
trait TokenSpanNerLabelCubbieSlot extends TokenSpanCubbie {
  def newTokenSpanNerLabel(ts:TokenSpan, s:String): cc.factorie.app.nlp.ner.NerLabel
  val ner = StringSlot("ner")
  if (storing ne null) storing match {
    case ts:TokenSpan => ner := ts.attr[cc.factorie.app.nlp.ner.NerLabel].categoryValue
  }
  /*override def storeTokenSpan(ts:TokenSpan): this.type = {
    super.storeTokenSpan(ts)
    ner := ts.attr[cc.factorie.app.nlp.ner.NerLabel].categoryValue
    this
  }*/
  /*override def fetchTokenSpan: TokenSpan = {
    val ts = newTokenSpan
    ts.attr += newTokenSpanNerLabel(ts, ner.value)
    ts
  }*/
}
*/
