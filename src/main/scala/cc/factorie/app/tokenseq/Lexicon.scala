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

package cc.factorie.app.tokenseq
import cc.factorie._
import scala.collection.mutable.{ArrayBuffer,HashMap}

/** A list of words or phrases, with methods to easily check whether a TokenInSeq is in the list.
    @author Andrew McCallum */
class Lexicon(val caseSensitive:Boolean) {
  import scala.io.Source
  import java.io.File
  /** Populate lexicon from file, with one entry per line, consisting of space-separated tokens. */
  def this(filename:String) = { this(false); this.++=(Source.fromFile(new File(filename))(scala.io.Codec.UTF8)) }
  var lexer = cc.factorie.app.tokenseq.nonWhitespaceClasses // TODO Make this a choice
  private class LexiconToken(val word:String) extends TokenInSeq[LexiconToken] {
    var next: LexiconToken = null
    var prev: LexiconToken = null
    def hasNext = next != null
    def hasPrev = prev != null
    def firstInSeq = if (prev == null) this else prev.firstInSeq
    def lengthToEnd: Int = if (next == null) 1 else 1 + next.lengthToEnd
    def length = firstInSeq.lengthToEnd
    def seq: Seq[LexiconToken] = {
      val result = new ArrayBuffer[LexiconToken];
      var t = firstInSeq; result += t
      while (t.hasNext) { t = t.next; result += t }
      result
    }
  }
  private def newLexiconTokens(words:Seq[String]): Seq[LexiconToken] = {
    val result = new ArrayBuffer[LexiconToken]
    var t: LexiconToken = null
    for (word <- words) {
      val t2 = new LexiconToken(word)
      t2.prev = t
      if (t != null) t.next = t2
      t = t2
      result += t2
    }
    result
  }
  private val contents = new HashMap[String,List[LexiconToken]];
  private def _key(s:String) = if (caseSensitive) s else s.toLowerCase
  private def +=(t:LexiconToken): Unit = {
    val key = _key(t.word)
    val old: List[LexiconToken] = contents.getOrElse(key, Nil)
    contents(key) = t :: old
  }
  private def addAll(ts:Seq[LexiconToken]): Unit = {
    //println("Lexicon adding "+ts.map(_.word))
    ts.foreach(t => this += t)
  }
  def +=(w:String): Unit = this.+=(new LexiconToken(w))
  def ++=(ws:Seq[String]): Unit = this.addAll(newLexiconTokens(if (caseSensitive) ws else ws.map(_.toLowerCase)))
  def ++=(source:Source): Unit = for (line <- source.getLines()) { this.++=(lexer.findAllIn(line).toList); /*println("TokenSeqs.Lexicon adding "+line)*/ }
  /** Is 'query' in the lexicon, accounting for lexicon phrases and the context of 'query' */
  def contains[T<:TokenInSeq[T]](query:T): Boolean = {
    //println("contains "+query.word+" "+query.hasPrev+" "+query)
    val key = _key(query.word)
    val entries = contents.getOrElse(key, Nil)
    for (entry <- entries) {
      var te = entry
      var tq = query
      var result = true
      // Go the beginning of this lexicon entry
      while (te.hasPrev && result) {
        if (!tq.hasPrev) return false
        te = te.prev; tq = tq.prev
      }
      //println("  Trying "+query.word+" "+entry.seq.map(_.word).toList)
      // Check for match all the way to the end of this lexicon entry
      do {
        if ((!caseSensitive && te.word != tq.word.toLowerCase) || (caseSensitive && te.word != tq.word)) result = false
        te = te.next; tq = tq.next
      } while (te != null && tq != null && result == true)   
      if (result && te == null) {
        //print(" contains length="+entry.length+"  "+entry.seq.map(_.word).toList)
        return true
      }
    }
    false
  }
  /** Is 'query' in the lexicon, ignoring context. */
  def containsSingle[T<:TokenInSeq[T]](query:T): Boolean = contents.contains(_key(query.word))
}


