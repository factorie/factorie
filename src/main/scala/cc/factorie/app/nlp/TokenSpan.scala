/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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
import cc.factorie.util.{Cubbie, Attr}
import cc.factorie.variable._

/** A sub-sequence of Tokens within a Section (which is in turn part of a Document). */
class TokenSpan(theSection:Section, initialStart:Int, initialLength:Int) extends SpanVariable[Section,Token](theSection, initialStart, initialLength) with Attr {
  /** The Document Section of which this TokenSpan is a subsequence. */
  final def section = chain  // Just a convenient alias
  /** The Document to which this TokenSpan belongs. */
  final def document = chain.document
  /** The indexed sequence of tokens contained in this TokenSpan. */
  final def tokens = this.toIndexedSeq //value
  /** The Sentence to which the first Token in this TokenSpan belongs. */
  def sentence = tokens(0).sentence
  // TODO Implement something like this? def containsSentenceIndex(i:Int): Boolean // Does this TokenSpan contain the token in the ith position of the sentence containing this TokenSpan.
  
  @deprecated("Use 'string' instead.") def phrase: String = string
  /** Return the substring of the Document covered by this TokenSpan.
      If this is a multi-Token TokenSpan, this will include all original characters in the Document, including those skipped by tokenization. */
  def documentString: String = document.string.substring(tokens.head.stringStart, tokens.last.stringEnd) // TODO Handle Token.attr[TokenString] changes
  /** Return a String representation of this TokenSpan, concatenating each Token.string, separated by the given separator. */
  def tokensString(separator:String): String = if (length == 1) tokens(0).string else tokens.map(_.string).mkString(separator)
  /** Return a String representation of this TokenSpan, concatenating each Token.string, separated by a space.
      This nicely avoids newlines, HTML or other junk that might be in the phrase.documentString. */
  def string: String = tokensString(" ")
  /** Returns true if this span contain the words of argument span in order. */
  def containsStrings(span:TokenSpan): Boolean = {
    for (i <- 0 until length) {
      if (length - i < span.length) return false
      var result = true
      var i2 = i; var j = 0
      while (j < span.length && i2 < this.length && result) {
        if (span.tokens(j).string != tokens(i2).string) result = false
        j += 1; i2 += 1
      }
      if (result) return true
    }
    false
  }
  override def toString = "TokenSpan("+start+","+end+":"+this.phrase+")"
  // TODO This seems unsafe.  Can we delete it? -akm
  /** A short name for this span */
  @deprecated("This method will be deleted in the near future.")
  def name: String = attr.values.head match {
    case label:LabeledCategoricalVariable[String @unchecked] => label.categoryValue
    case x => x.toString
  }

  /**
   * Returns the character offsets of this TokenSpan into the raw text of its original document.
   */
  def characterOffsets:(Int, Int) = this.apply(0).stringStart -> this.apply(length).stringEnd
}
trait TokenSpanCollection[S<:TokenSpan] extends SpanVarCollection[S, Section, Token]


/** An immutable collection of TokenSpans, with various methods to returns filtered sub-sets of spans based on position and class. */
class TokenSpanList[S<:TokenSpan](spans:Iterable[S]) extends SpanVarList[S, Section, Token](spans) with TokenSpanCollection[S]

/** A mutable collection of TokenSpans, with various methods to returns filtered sub-sets of spans based on position and class. */
class TokenSpanBuffer[S<:TokenSpan] extends SpanVarBuffer[S, Section, Token] with TokenSpanCollection[S]

object TokenSpan {

  //TODO this doesn't seem to be used anywhere, can it be deleted? -KS
  //TODO If this is used, it could be incorporated into the TriePhraseLexcion 
  //     using the AhoCorasick findMention method - craigacp
  def fromLexicon(lexicon:cc.factorie.app.nlp.lexicon.PhraseLexicon, document:Document): Int = {
    var spanCount = 0
    for (section <- document.sections; token <- section.tokens) {
      val len = lexicon.startsAt(token)
      if (len > 0) {
        throw new Error("Not yet implemented.")  // To what SpanList should these tokens be added? -akm
        val span = new TokenSpan(section, token.position, len)
        span.attr += lexicon
        spanCount += 1
      }
    }
    spanCount
  }
}


// Cubbie storage

class TokenSpanCubbie extends Cubbie {
  val start = IntSlot("start")
  val length = IntSlot("length")
  def storeTokenSpan(ts:TokenSpan): this.type = {
  	start := ts.start
  	length := ts.length
  	finishStoreTokenSpan(ts)
  	this
  }
  def finishStoreTokenSpan(ts:TokenSpan): Unit = {}
  def fetchTokenSpan(section:Section): TokenSpan = {
    val ts = new TokenSpan(section, start.value, length.value)
    finishFetchTokenSpan(ts)
    ts
  }
  def finishFetchTokenSpan(ts:TokenSpan): Unit = {}
}

trait TokenSpanWithPhraseCubbie extends TokenSpanCubbie {
  val phrase = StringSlot("phrase")
  override def finishStoreTokenSpan(ts:TokenSpan): Unit = {
    super.finishStoreTokenSpan(ts)
    phrase := ts.phrase
  }
}

trait TokenSpanWithDocRefCubbie[DC<:DocumentCubbie[_,_,_]] extends TokenSpanCubbie {
  def newDocumentCubbie: DC
  val doc = RefSlot("doc", ()=>newDocumentCubbie)
  override def finishStoreTokenSpan(ts:TokenSpan): Unit = {
    super.finishStoreTokenSpan(ts)
    doc := ts.document.name
  }
  def fetchTokenSpan(/* implicit cr:CubbieRefs */): TokenSpan = {
    throw new Error("Not yet implemented")
    val ts = new TokenSpan(null, start.value, length.value)
    finishFetchTokenSpan(ts)
    ts
  }
}

trait TokenSpanNerLabelCubbieSlot extends TokenSpanCubbie {
  def newTokenSpanNerLabel(ts:TokenSpan, s:String): cc.factorie.app.nlp.ner.NerSpanLabel
  val ner = StringSlot("ner")
  override def finishStoreTokenSpan(ts:TokenSpan): Unit = {
    super.finishStoreTokenSpan(ts)
    ner := ts.attr[cc.factorie.app.nlp.ner.NerSpanLabel].categoryValue
  }
  override def finishFetchTokenSpan(ts:TokenSpan): Unit = {
    super.finishFetchTokenSpan(ts)
    ts.attr += newTokenSpanNerLabel(ts, ner.value)
  }
}
