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
import cc.factorie.app.nlp.TokenSpan
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer,NoopLemmatizer}
import scala.collection.mutable.{ArrayBuffer,HashMap}
import scala.io.Source
import java.io.File
import cc.factorie.app.chain.Observation
import scala.io.Codec.charset2codec

/** The general interface to a lexicon.  Both WordLexicon and PhraseLexicon are subclasses.
    @author Andrew McCallum */
trait Lexicon {
  /** An identifier for this lexicon, suitable for adding as a category to a FeatureVectorVariable[String]. */
  def name: String
  // For pre-processing of lexicon and query strings
  /** The string segmenter that breaks a lexicon entries and queries into (potentially) multi-word phrases. */
  def tokenizer:StringSegmenter
  /** The string lemmatizer that simplifies lexicon entries and queries before searching for a match.
      For example, a common lemmatizer is one that lowercases all strings. */
  def lemmatizer:Lemmatizer
  /** Is this single word in the lexicon.  The input String will not be processed by tokenizer, but will be processed by the lemmatizer. */
  def containsLemmatizedWord(word:String): Boolean
  // For querying the lexicon
  /** Is this single word in the lexicon.  The input String will not be processed by tokenizer, but will be processed by the lemmatizer. */
  def containsWord(word:String): Boolean = containsLemmatizedWord(lemmatizer.lemmatize(word))
  /** Is the pre-tokenized sequence of words in the lexicon.  Each of the input words is processed by the lemmatizer. */
  def containsLemmatizedWords(words: Seq[String]): Boolean
  def containsWords(words: Seq[String]): Boolean = containsLemmatizedWords(words.map(lemmatizer.lemmatize(_)))
  /** Is this Token (or more generally Observation) a member of a phrase in the lexicon (including single-word phrases).
      For example if query.string is "New" and query.next.string is "York" and the two-word phrase "New York" is in the lexicon, 
      then this method will return true.  But if query.next.string is "shoes" (and "New shoes" is not in the lexicon) this method will return false. */
  def contains[T<:Observation[T]](query:T): Boolean
  def contains[T<:Observation[T]](query:Seq[T]): Boolean
  def contains(span:TokenSpan): Boolean = contains(span.value)
  /** Is the input String in the lexicon.  The input is tokenized and lemmatized; 
      if the tokenizer indicates that it is a multi-word phrase, it will be processed by containsWords, otherwise containsWord. */
  def contains(untokenizedString:String): Boolean = { val words = tokenizer(untokenizedString).map(lemmatizer.lemmatize(_)).toSeq; if (words.length == 1) containsWord(words.head) else containsWords(words) }
}

trait MutableLexicon extends Lexicon {
  // For populating the lexicon
  /** Tokenize and lemmatize the input String and add it as a single entry to the Lexicon */
  def +=(phrase:String): Unit
  /** All a lines from the input Source to this lexicon.  Source is assumed to contain multiple newline-separated lexicon entries */
  def ++=(source:Source): this.type = { for (line <- source.getLines()) { val phrase = line.trim; if (phrase.length > 0) MutableLexicon.this.+=(phrase) }; source.close(); this }
  /** All a lines from the input String to this lexicon.  String contains multiple newline-separated lexicon entries */
  def ++=(phrases:String): this.type = ++=(Source.fromString(phrases))
  /** All a lines from the input File to this lexicon.  File contains multiple newline-separated lexicon entries */
  def ++=(file:File, enc:String = "UTF-8"): this.type = ++=(Source.fromFile(file, enc))
}

/** A union of multiple lexicons.  Answer "contains" queries with true, if any of the member Lexicons contain the query.
    @author Andrew McCallum */
class UnionLexicon(val name: String, val members:Lexicon*) extends Lexicon {
  def tokenizer: StringSegmenter = members.head.tokenizer
  def lemmatizer: Lemmatizer = members.head.lemmatizer
  def containsLemmatizedWord(word:String): Boolean = members.exists(_.containsLemmatizedWord(word))
  def containsLemmatizedWords(words: Seq[String]): Boolean = members.exists(_.containsLemmatizedWords(words))
  def contains[T<:Observation[T]](query:T): Boolean = members.exists(_.contains(query))
  def contains[T<:Observation[T]](query:Seq[T]): Boolean = members.exists(_.contains(query))
}

/** Support for constructing Lexicons, which automatically will determine if a WordLexicon will suffice or a PhraseLexicon is required.
    @author Andrew McCallum */
object Lexicon {
  def fromSource(name:String, source:Source, tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, lemmatizer:Lemmatizer = LowercaseLemmatizer): Lexicon = {
    var result: MutableLexicon = new WordLexicon(name, tokenizer, lemmatizer)
    try { result ++= source } catch { case e:MultiWordException => {
      result = new PhraseLexicon(name, tokenizer, lemmatizer)
      result ++= source.reset
      source.close()
    } }
    result
  }
  def fromFilename(filename:String, tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, lemmatizer:Lemmatizer = LowercaseLemmatizer): Lexicon = 
    fromSource(filename, Source.fromFile(new File(filename))(scala.io.Codec.UTF8))
  def fromResource(resourceFilename:String, tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, lemmatizer:Lemmatizer = LowercaseLemmatizer): Lexicon =
    fromSource(resourceFilename, io.Source.fromInputStream(getClass.getResourceAsStream(resourceFilename)))
}

/** A Lexicon that can only hold single-word lexicon entries, but which is efficient for this case.
    with methods to check whether a String or Token (or more generally a cc.factorie.app.chain.Observation) is in the list.
    @author Andrew McCallum */
class WordLexicon(val name:String, val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer) extends MutableLexicon {
  val contents = new scala.collection.mutable.HashSet[String]
  def +=(phrase:String): Unit = {
    val words: Seq[String] = tokenizer(phrase).toSeq
    if (words.length == 1) contents += lemmatizer.lemmatize(words.head) else throw new MultiWordException("Cannot add multi-word phrase to WordLexicon: "+phrase+" => "+words+" by segmenter "+tokenizer.getClass)
  }
  final def containsLemmatizedWord(word:String): Boolean = contents.contains(word)
  def contains[T<:Observation[T]](query:T): Boolean = containsWord(query.string)
  def containsLemmatizedWords(words: Seq[String]): Boolean = if (words.length == 1) containsLemmatizedWord(words.head) else false
  def contains[T<:Observation[T]](query:Seq[T]): Boolean = if (query.length == 1) containsWord(query.head.string) else false
}

/** An exception thrown when someone tries to add a multi-word phrase to a WordLexicon. */
class MultiWordException(msg:String) extends Exception(msg)

/** A list of words or phrases, with methods to check whether a String, Seq[String], or Token (or more generally a cc.factorie.app.chain.Observation) is in the list.
    @author Andrew McCallum */
class PhraseLexicon(val name:String, val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer) extends MutableLexicon {
  // The next two constructors are there just to support legacy usage, and should ultimately be removed.
  /** Populate lexicon from file, with one entry per line, consisting of space-separated tokens. */
  def this(file:File) = { this(file.toString, cc.factorie.app.strings.nonWhitespaceSegmenter, LowercaseLemmatizer); this.++=(Source.fromFile(file)(scala.io.Codec.UTF8)) }
  //def this(caseSensitive:Boolean) = this(lemmatizer = if (caseSensitive) LowercaseLemmatizer else NoopLemmatizer)
  
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
  val contents = new HashMap[String,List[LexiconToken]]
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
      PhraseLexicon.this += newLexiconTokens(words.map(lemmatizer.lemmatize(_)))
    }
  }
  private def +=(ts:Seq[LexiconPhraseToken]): Unit = {
    //println("Lexicon adding "+ts.map(_.word))
    ts.foreach(t => PhraseLexicon.this += t)
  }
  /** Add a new lexicon entry consisting of a multi-string phrase. */
  //def +=(ws:Seq[String]): Unit = this.+=(newLexiconTokens(ws.map(lemmatizer.lemmatize(_))))
  //def ++=(source:Source): Unit = for (line <- source.getLines()) yield { PhraseLexicon.this.+=(line); /*println("TokenSeqs.Lexicon adding "+line)*/ }
  /** String contains multiple newline-separated lexicon entries */
  //def ++=(phrases:String): Unit = ++=(Source.fromString(phrases))
  //def ++=(file:File, enc:String = "UTF-8"): Unit = ++=(Source.fromFile(file, enc))
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
  def containsLemmatizedWord(word:String): Boolean = contents.contains(word)
  def containsLemmatizedWords(words: Seq[String]): Boolean = newLexiconTokens(words).nonEmpty && contains(newLexiconTokens(words).head.asInstanceOf[Observation[LexiconToken]])
  def contains[T<:Observation[T]](query:Seq[T]): Boolean = {
    val queryToken = query.head
    val entries = contents.getOrElse(lemmatizer.lemmatize(queryToken.string), Nil)
    for (entry <- entries) {
      if (entry eq LexiconToken) return true // The lexicon entry is a single word, indicated just by the presence (keyString, object LexiconToken) 
      var te: LexiconToken = entry
      var tq = queryToken
      var result = true
      // Check for match all the way to the end of this lexicon entry
      do {
        if (te.string != lemmatizer.lemmatize(tq.string)) result = false
        //if ((!caseSensitive && te.string != tq.string.toLowerCase) || (caseSensitive && te.string != tq.string)) result = false
        te = te.next; tq = tq.next
      } while (te != null && tq != null && result)
      if (result && te == null) {
        //print(" contains length="+entry.length+"  "+entry.seq.map(_.word).toList)
        return true
      }
    }
    false
  }

  /** Is 'query' in the lexicon, accounting for lexicon phrases and the context of 'query' */
  def contains[T<:Observation[T]](query:T): Boolean = {
    //println("contains "+query.word+" "+query.hasPrev+" "+query)
    val entries = contents.getOrElse(lemmatizer.lemmatize(query.string), Nil)
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
      } while (te != null && tq != null && result)
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
      } while (te != null && tq != null && found)
      if (found && te == null) {
        //print(" contains length="+entry.length+"  "+entry.seq.map(_.word).toList)
        return len
      }
    }
    -1
  }
}

