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
  
  def toCubbie: TokenSpanCubbie = {
    val c = new TokenSpanCubbie
    c.start := start
    c.length := length
    attr.intoCubbie(c)
    c
  }
  def this(doc:Document, c:TokenSpanCubbie) = {
    this(doc, c.start.value, c.length.value)(null)
    // TODO Do something with Attr
  }
}

class TokenSpanCubbie extends Cubbie {
  val doc = RefSlot("doc", ()=>new DocumentCubbie)
  val start = IntSlot("start")
  val length = IntSlot("length")
  // TODO put the Attr in here also
}
