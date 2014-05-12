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
package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.Some

/**
 * User: apassos
 * Date: 8/19/13
 * Time: 12:58 PM
 */

/**
 * A sequence of sections which are tokenized as phrases.
 */
class PhraseSectionList extends ArrayBuffer[Section]

class PhraseTrie {
  class TrieKey
  case class StringKey(s: String) extends TrieKey
  object EndKey extends TrieKey
  val map = new mutable.HashMap[TrieKey, PhraseTrie]
  def add(phrase: Seq[String]) {
    if (phrase.length > 0) {
      val child = map.getOrElseUpdate(StringKey(phrase.head), new PhraseTrie)
      child.add(phrase.tail)
    } else {
      map(EndKey) = new PhraseTrie
    }
  }

  def canEnd = if (map.contains(EndKey)) 0 else -1

  def findLongestPhraseLength(tokens: Seq[Token], position: Int): Int = {
    math.max(canEnd, map.get(StringKey(tokens(position).string)) match {
      case None => -1
      case Some(trie) =>
        if (position + 1 < tokens.length) {
          val len = trie.findLongestPhraseLength(tokens, position+1)
          if (len >= 0) len + 1
          else -1
        } else -1
    })
  }
}

object PhraseTokenizerModes extends scala.Enumeration {
  type PhraseTokenizerMode = Value
  val REPLACE_SECTIONS, ADD_TO_SECTIONS, ADD_SEPARATELY = Value
}

/**
 * A tokenizer which will merge existing tokens if they are from one of the phrases given.
 *
 * Efficiently uses a trie-like data structure to simulate the finite automaton for
 * tokenization. The behavior is that if there is a long and a short phrase with the same prefix
 * the longer one will be picked greedily.
 *
 * This version gets all attributes from the last token in the phrase.
 *
 * @param phrases The set of phrases to be picked.
 * @param mode The mode. If ADD_SEPARATELY the new sections are only added to the attribute.
 *             If ADD_TO_SECTIONS the new sections are added to the document.
 *             IF REPLACE_SECTIONS the existing sections in the document are replaced.
 */
class PhraseTokenizer(phrases: Iterable[Seq[String]], val mode: PhraseTokenizerModes.PhraseTokenizerMode = PhraseTokenizerModes.ADD_SEPARATELY) extends DocumentAnnotator {
  val trie = new PhraseTrie
  phrases.foreach(trie.add)
  def prereqAttrs = Seq(classOf[Token])
  def postAttrs = Seq(classOf[PhraseSectionList])
  def tokenAnnotationString(token: Token) = null

  def process(document: Document): Document = {
    val newSections = new PhraseSectionList
    document.attr += newSections
    for (section <- document.sections) {
      val newSection = new BasicSection(section.document, section.stringStart, section.stringEnd)
      newSections += newSection
      val tokens = section.tokens
      var i = 0
      while (i < tokens.length) {
        trie.findLongestPhraseLength(tokens, i) match {
          case -1 => val t = new Token(newSection, tokens(i).stringStart, tokens(i).stringEnd)
            tokens(i).attr.values.foreach(t.attr.+=)
            i += 1
          case 0  => throw new Error(s"Found a single-token phrase in the dictionary, should not happen. Offending phrase: ${tokens(i).string}")
          case n =>
            val t = new Token(newSection, tokens(i).stringStart, tokens(i+n-1).stringEnd)
            tokens(i+n-1).attr.values.foreach(t.attr.+=)
            i += n
        }
      }
    }
    mode match {
      case PhraseTokenizerModes.ADD_TO_SECTIONS => newSections.foreach(document.+=)
      case PhraseTokenizerModes.REPLACE_SECTIONS =>
        document.clearSections()
        newSections.foreach(document.+=)
      case PhraseTokenizerModes.ADD_SEPARATELY =>
    }
    document
  }
}
