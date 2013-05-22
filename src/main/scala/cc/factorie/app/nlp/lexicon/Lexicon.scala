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

package cc.factorie.app.nlp.lexicon
import cc.factorie._
import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer,NoopLemmatizer}
import scala.collection.mutable.{ArrayBuffer,HashMap}
import scala.io.Source
import java.io.File
import cc.factorie.app.chain.Observation
import scala.io.Codec.charset2codec

/** A list of words or phrases, with methods to easily check whether a Token (or more generally a cc.factorie.app.chain.Observation) is in the list.
    @author Andrew McCallum */
class Lexicon(val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceClassesSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer) {
  /** Populate lexicon from file, with one entry per line, consisting of space-separated tokens. */
  def this(filename:String) = { this(); this.++=(Source.fromFile(new File(filename))(scala.io.Codec.UTF8)) }
  def this(caseSensitive:Boolean) = this(lemmatizer = if (caseSensitive) LowercaseLemmatizer else NoopLemmatizer)
  
  class LexiconToken extends Observation[LexiconToken] {
    def string: String = throw new Error("string unknown; in key only.")
    def next: LexiconToken = null
    def next_=(lt:LexiconToken): Unit = throw new Error
    def prev_=(lt:LexiconToken): Unit = throw new Error
    def prev: LexiconToken = null
    def hasNext = false
    def hasPrev = false
    def position = 0
    def lengthToEnd = 1
  }
  object LexiconToken extends LexiconToken // Used to efficiently represent single-word lexicon entries
  class LexiconPhraseToken(override val string:String) extends LexiconToken {
    override var next: LexiconToken = null
    override var prev: LexiconToken = null
    override def hasNext = next != null
    override def hasPrev = prev != null
    override def position = lengthToEnd
    override def lengthToEnd: Int = if (next == null) 1 else 1 + next.lengthToEnd
  }
  private def newLexiconTokens(words:Seq[String]): Seq[LexiconPhraseToken] = {
    val result = new ArrayBuffer[LexiconPhraseToken]
    var t: LexiconPhraseToken = null
    for (word <- words) {
      val t2 = new LexiconPhraseToken(word)
      t2.prev = t
      if (t != null) t.next = t2
      t = t2
      result += t2
    }
    result
  }
  val contents = new HashMap[String,List[LexiconToken]];
  private def +=(t:LexiconPhraseToken): Unit = {
    val key = lemmatizer.lemmatize(t.string)
    val old: List[LexiconToken] = contents.getOrElse(key, Nil)
    contents(key) = t :: old
  }
  /** Add a new lexicon entry consisting of one or more words.  The Lexicon's tokenizer will be used to split the string, if possible. */
  def +=(phrase:String): Unit = {
    val words: Seq[String] = tokenizer(phrase).toSeq
    if (words.length == 1) {
      val word = words.head
      val key = lemmatizer.lemmatize(word)
      val old: List[LexiconToken] = contents.getOrElse(key, Nil)
      contents(key) = LexiconToken :: old
    } else {
      this += newLexiconTokens(words.map(lemmatizer.lemmatize(_)))
    }
  }
  private def +=(ts:Seq[LexiconPhraseToken]): Unit = {
    //println("Lexicon adding "+ts.map(_.word))
    ts.foreach(t => this += t)
  }
  /** Add a new lexicon entry consisting of a multi-string phrase. */
  //def +=(ws:Seq[String]): Unit = this.+=(newLexiconTokens(ws.map(lemmatizer.lemmatize(_))))
  def ++=(source:Source): Unit = for (line <- source.getLines()) { this.+=(line); /*println("TokenSeqs.Lexicon adding "+line)*/ }
  def ++=(phrases:String): Unit = ++=(Source.fromString(phrases))
  def ++=(file:File, enc:String = "UTF-8"): Unit = ++=(Source.fromFile(file, enc))
  def phrases: Seq[String] = {
    def phrase(entry:LexiconToken): String = if (entry.hasNext) entry.string + " " + phrase(entry.next) else entry.string 
    val result = new ArrayBuffer[String]
    for (key <- contents.keys; entry <- contents(key)) {
      if (entry eq LexiconToken) result += key
      else result += phrase(entry.chainHead)
    }
    result.distinct
  }
  /** Do any of the Lexicon entries contain the given word string. */
  def containsWord(word:String): Boolean = contents.contains(lemmatizer.lemmatize(word))
  def containsWords(words: Seq[String]): Boolean = contains(newLexiconTokens(words.map(lemmatizer.lemmatize(_))).head.asInstanceOf[Observation[LexiconToken]])
  def contains(untokenizedString:String): Boolean = { val words = tokenizer(untokenizedString).toSeq; if (words.length == 1) containsWord(words.head) else containsWords(words) }
  /** Is 'query' in the lexicon, accounting for lexicon phrases and the context of 'query' */
  def contains[T<:Observation[T]](query:T): Boolean = {
    //println("contains "+query.word+" "+query.hasPrev+" "+query)
    val key = lemmatizer.lemmatize(query.string)
    val entries = contents.getOrElse(key, Nil)
    for (entry <- entries) {
      if (entry eq LexiconToken) return true // The lexicon entry is a single word, indicated just by the presence (keyString, object LexiconToken) 
      var te: LexiconToken = entry
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
        if (te.string != lemmatizer.lemmatize(tq.string)) result = false
        //if ((!caseSensitive && te.string != tq.string.toLowerCase) || (caseSensitive && te.string != tq.string)) result = false
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
  def containsSingle[T<:Observation[T]](query:T): Boolean = contents.contains(lemmatizer.lemmatize(query.string))
  /** Return length of match, or -1 if no match. */
  def startsAt[T<:Observation[T]](query:T): Int = {
    val key = lemmatizer.lemmatize(query.string)
    val entries = contents.getOrElse(key, Nil)
    for (entry <- entries.filter(_.hasPrev == false).sortBy(entry => -entry.lengthToEnd)) { // Sort so that we look for long entries first
      var te = entry
      var tq = query
      var len = 0
      var found = true
      // Query must be at the the beginning of this lexicon entry
      // Check for match all the way to the end of this lexicon entry
      do {
        if (te.string != lemmatizer.lemmatize(tq.string)) found = false
        //if ((!caseSensitive && te.string != tq.string.toLowerCase) || (caseSensitive && te.string != tq.string)) found = false
        len += 1
        te = te.next; tq = tq.next
      } while (te != null && tq != null && found == true)   
      if (found && te == null) {
        //print(" contains length="+entry.length+"  "+entry.seq.map(_.word).toList)
        return len
      }
    }
    -1
  }
}

