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

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.coref.WithinDocCoref
import cc.factorie.app.nlp.morph.BasicMorphologicalAnalyzer
import cc.factorie.variable.{EnumDomain, CategoricalVariable}
import scala.reflect.ClassTag

object NumberDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  SINGULAR,        // one of something
  PLURAL = Value   // multiple of something
  freeze()
}

class Number extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = NumberDomain
}
class PhraseNumber(val phrase:Phrase, value:Int) extends Number(value) {
  def this(phrase:Phrase, value:String) = this(phrase, NumberDomain.index(value))
}

/** Cheap number predictor based on rules and lexicons.  Really this should use a real morphological analyzer. */
class NounPhraseNumberLabeler[A<:AnyRef](documentAttrToPhrases:(A)=>Iterable[Phrase])(implicit docAttrClass:ClassTag[A]) extends DocumentAnnotator {
  val singularPronoun = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluralPronoun = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singularDeterminer = Set("a", "an", "this")
  val pluralDeterminer = Set("those", "these", "some")
  def isProper(pos:String): Boolean = pos.startsWith("NNP")
  def isNoun(pos:String): Boolean = pos(0) == 'N'
  def isPossessive(pos:String): Boolean = pos == "POS"
  def process(document:Document): Document = {
    for (phrase <- documentAttrToPhrases(document.attr[A])) process(phrase)
    document
  }
  def process(phrase:Phrase): Unit = {
    import NumberDomain._
    val number = new PhraseNumber(phrase, UNKNOWN)
    phrase.attr += number
    if (phrase.length > 0) {
      val firstWord = phrase(0).string.toLowerCase
      val headPos = phrase.headToken.attr[PennPosTag].categoryValue
      if (singularPronoun.contains(firstWord) || singularDeterminer.contains(firstWord)) number := SINGULAR
      else if (pluralPronoun.contains(firstWord) || pluralDeterminer.contains(firstWord)) number := PLURAL
      else if (isProper(headPos) && phrase.exists(token => token.string.toLowerCase == "and")) number := PLURAL
      else if (isNoun(headPos) || isPossessive(headPos)) {
        val headWord = phrase.headToken.string.toLowerCase
        if (BasicMorphologicalAnalyzer.isPlural(headWord)) number := PLURAL
        else if (headPos.startsWith("N")) { if (headPos.endsWith("S")) number := PLURAL else number := SINGULAR }
        else number := SINGULAR
      }
    }
  }
  override def tokenAnnotationString(token:Token): String = { val phrases = documentAttrToPhrases(token.document.attr[A]).filter(_.contains(token)); phrases.map(_.attr[Number].categoryValue).mkString(",") }
  override def phraseAnnotationString(phrase:Phrase): String = { val t = phrase.attr[Number]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[PennPosTag], classOf[NounPhraseList])
  def postAttrs: Iterable[Class[_]] = List(classOf[PhraseNumber])
}

//class NounPhraseNumberLabeler extends PhraseNumberLabeler[NounPhraseList](phrases => phrases)
object NounPhraseNumberLabeler extends NounPhraseNumberLabeler[NounPhraseList](phrases => phrases)

//class MentionPhraseNumberLabeler extends PhraseNumberLabeler[WithinDocCoref](_.mentions.map(_.phrase))
object MentionPhraseNumberLabeler extends NounPhraseNumberLabeler[WithinDocCoref](_.mentions.map(_.phrase))

// No reason to have this.   Label should always go on Phrase, not mention. -akm
//object MentionNumberLabeler extends NumberLabeler[Mention,MentionList]
