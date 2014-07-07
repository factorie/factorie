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

package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp._
import cc.factorie.util.Attr
import cc.factorie.app.nlp.pos.PennPosDomain
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain

/** A Phrase is a TokenSpan that has a head token.
    If offsetToHeadToken is unspecified, then it will be set automatically using HeadTokenOffset.apply. */
class Phrase(section:Section, start:Int, length:Int, offsetToHeadToken: Int) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  
  assert(offsetToHeadToken == -1 || offsetToHeadToken >= 0 && offsetToHeadToken < length, "Offset from beginning of span, headTokenOffset="+offsetToHeadToken+", but span only has length "+length)
  val headTokenOffset = if (offsetToHeadToken == -1) HeadTokenOffset(this) else offsetToHeadToken
  
  def headToken: Token = this.apply(headTokenOffset)
  
  def isPronoun = { val i = headToken.posTag.intValue; i == PennPosDomain.prpIndex || i == PennPosDomain.prpdIndex || i == PennPosDomain.wpIndex || i == PennPosDomain.wpdIndex }
  def isProperNoun = { val i = headToken.posTag.intValue; i == PennPosDomain.nnpIndex || i == PennPosDomain.nnpsIndex }
  def isNoun = headToken.posTag.categoryValue(0) == 'N'
  def isPossessive = headToken.posTag.intValue == PennPosDomain.posIndex 
  def isAppositionOf(other:Phrase) : Boolean = (headToken.parseLabel.intValue == ParseTreeLabelDomain.appos) && (headToken.parseParent == other.headToken)

  def gender = this.attr[Gender]
  def number = this.attr[Number]
  def nounPhraseType = this.attr[NounPhraseType]
  
}

/** A collection of Phrases.  Typically used as an attribute of a Section or a Document. */
class PhraseList(spans:Iterable[Phrase]) extends TokenSpanList[Phrase](spans)

/** A collection of Phrases that are noun phrases.  Typically used as an attribute of a Section or a Document. */
class NounPhraseList(phrases:Iterable[Phrase]) extends PhraseList(phrases)

/** A collection of VerbPhrases.  Typically used as an attribute of a Section or a Document. */
class VerbPhraseList(phrases:Iterable[Phrase]) extends PhraseList(phrases)


/** A heuristic for selecting the head of a phrase.
    If a parse is available, use it to find the head; if a preposition is found, select the word before it; otherwise simply select the last token. */
object HeadTokenOffset {
  def apply(phrase:Phrase): Int = {
    if (phrase.length == 1) return 0
    val span = phrase.value
    val sentence = phrase.sentence
    // If there is a parse, then traverse up the tree until just before we exit the Span
    val parse = sentence.parse
    if (parse ne null) {
      var headSentenceIndex = span.end-1 - sentence.start
      var parentSentenceIndex = parse.parentIndex(headSentenceIndex)
      while (span.contains(parentSentenceIndex + sentence.start)) {
        headSentenceIndex = parentSentenceIndex
        parentSentenceIndex = parse.parentIndex(parentSentenceIndex)
      }
      //Sometimes phrases are broken, consisting of more than one subgraph in the parse tree; check if parent of exit is not again part of mention
      if(parentSentenceIndex >= 0) {
        parentSentenceIndex = parse.parentIndex(parentSentenceIndex)
        while (span.contains(parentSentenceIndex + sentence.start)) {
          headSentenceIndex = parentSentenceIndex
          parentSentenceIndex = parse.parentIndex(parentSentenceIndex)
        }
      }
      return headSentenceIndex + sentence.start - span.start
    } else {
      // If there is a preposition, select the word just before the first preposition
      val prepositionIndex = span.indexWhere(cc.factorie.app.nlp.lexicon.Preposition.contains(_))
      if (prepositionIndex >= 1) return prepositionIndex - 1
      // If there is noun, return the last noun
      val lastNounIndex = span.lastIndexWhere(_.posTag.isNoun)
      if (lastNounIndex > 0) return lastNounIndex
      // Otherwise simply select the last word of the span
      else return span.length-1

    }
  }
}
