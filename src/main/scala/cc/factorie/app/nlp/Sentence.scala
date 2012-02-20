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

  def contains(t: Token) = {
    t.stringStart >= initialStart && t.stringEnd <= (initialStart+initialLength)
  }

  // Parse attributes
  def parse = attr[cc.factorie.app.nlp.parse.ParseTree]
  def parseRootChild: Token = attr[cc.factorie.app.nlp.parse.ParseTree].rootChild

}

// Cubbie storage

class SentenceCubbie extends TokenSpanCubbie {
  override def newObject(doc:Document, start:Int, length:Int): Sentence = new Sentence(doc, start, length)
  def storeSentence(s:Sentence): this.type = {
    storeTokenSpan(s)
  }
  def fetchSentence(doc:Document): Sentence = {
    val s = newObject(doc, start.value, length.value)
    s
  }
}

// To save the sentence with its parse tree use "new SentenceCubbie with SentenceParseTreeCubbie"
trait SentenceParseCubbie extends SentenceCubbie {
  val parse = CubbieSlot("parse", () => new cc.factorie.app.nlp.parse.ParseTreeCubbie)
  override def storeSentence(s:Sentence): this.type = {
    super.storeSentence(s)
    parse := parse.constructor().storeParseTree(s.parse)
    this
  }
  override def fetchSentence(doc:Document): Sentence = {
    val s = super.fetchSentence(doc)
    s.attr += parse.value.fetchParseTree(s)
    s
  }
}