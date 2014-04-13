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

package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.Attr
import cc.factorie.variable.{CategoricalVariable, CategoricalDomain}
import cc.factorie.app.nlp.pos.PennPosDomain
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain

/** A Phrase is a TokenSpan that has a head token. */
class Phrase(section:Section, start:Int, length:Int, val headTokenOffset: Int = -1) extends TokenSpan(section, start, length) with Attr {
  def this(span:TokenSpan, headTokenIndex:Int = -1) = this(span.section, span.start, span.length, headTokenIndex)
  assert(headTokenOffset == -1 || headTokenOffset >= 0 && headTokenOffset < length, "Offset from beginning of span, headTokenOffset="+headTokenOffset+", but span only has length "+length)
  def headToken: Token = { require(headTokenOffset >= 0); this.apply(headTokenOffset) }
  
  def isPronoun = { val i = headToken.posTag.intValue; i == PennPosDomain.prpIndex || i == PennPosDomain.prpdIndex || i == PennPosDomain.wpIndex || i == PennPosDomain.wpdIndex }
  def isProperNoun = { val i = headToken.posTag.intValue; i == PennPosDomain.nnpIndex || i == PennPosDomain.nnpsIndex }
  def isNoun = headToken.posTag.categoryValue(0) == 'N'
  def isPossessive = headToken.posTag.intValue == PennPosDomain.posIndex 
  def isAppositionOf(other:Phrase) : Boolean = (headToken.parseLabel.intValue == ParseTreeLabelDomain.appos) && (headToken.parseParent == other.headToken)

  
}

/** A collection of Phrases.  Typically used as an attribute of a Section or a Document. */
class PhraseList(spans:Iterable[Phrase]) extends TokenSpanList[Phrase](spans)

///** A simple subclass of Chunk reserved for verb phrases. */
//class VerbPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length)
///** A collection of VerbPhrases.  Typically used as an attribute of a Section or a Document. */
//class VerbPhraseList(spans:Iterable[VerbPhrase]) extends TokenSpanList[VerbPhrase](spans)

///** A simple subclass of Chunk reserved for noun phrases.
//    A Mention (used in coreference) inherits from this; (or rather should after FACTORIE NLP is further cleaned up). */
//class NounPhrase(section:Section, start:Int, length:Int, headTokenOffset: Int = -1) extends Phrase(section, start, length) {
//  def this(span:TokenSpan, headTokenOffset:Int) = this(span.section, span.start, span.length, headTokenOffset)
//}

/** A collection of Phrases that are noun phrases.  Typically used as an attribute of a Section or a Document. */
class NounPhraseList(phrases:Iterable[Phrase]) extends PhraseList(phrases)

