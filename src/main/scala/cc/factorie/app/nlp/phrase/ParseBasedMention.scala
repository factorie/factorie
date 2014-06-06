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

import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.ner._
import scala.Some
import cc.factorie.app.nlp.coref.{ConllProperNounPhraseFinder, MentionList, Mention}


class ParseBasedMentionList(spans:Iterable[Mention]) extends MentionList(spans)

object ParseBasedPhraseFinder extends ParseBasedPhraseFinder(false)

object ParseAndNerBasedPhraseFinder extends ParseBasedPhraseFinder(true)

class ParseBasedPhraseFinder(val useNER: Boolean) extends DocumentAnnotator {
  def prereqAttrs: Iterable[Class[_]] = if (!useNER) List(classOf[parse.ParseTree]) else List(classOf[parse.ParseTree])++ConllProperNounPhraseFinder.prereqAttrs
  def postAttrs: Iterable[Class[_]] = List(classOf[PhraseList])

  def process(doc: Document): Document = {
    // Filter Mentions that have no MentionType and that are longer than 5 words -akm
    doc.attr += new PhraseList(dedup(getPhrases(doc)).filter(phrase => phrase.attr[NounPhraseType] ne null).toSeq)
    doc
  }

  def getPhrases(doc:Document): Seq[Phrase] = {
    var docPhrases = new ArrayBuffer[Phrase]

    //if NER has already been done, then convert the NER tags to NER spans
    //Note that this doesn't change the postAttrs for the annotator, since it may not necessarily add spans
    if(useNER) docPhrases ++= ConllProperNounPhraseFinder(doc)

    // NAM = proper noun, NOM = common noun, PRO = pronoun
    docPhrases ++= personalPronounSpans(doc)           map(  phrase => {phrase.attr += new NounPhraseType(phrase,"PRO");phrase})
    docPhrases ++= nounPhraseSpans(doc, isCommonNoun)  map(  phrase => {phrase.attr += new NounPhraseType(phrase,"NOM");phrase})
    docPhrases ++= nounPhraseSpans(doc, isProperNoun)  map(  phrase => {phrase.attr += new NounPhraseType(phrase,"NAM");phrase})
    docPhrases ++= NNPSpans(doc)                       map(  phrase => {phrase.attr += new NounPhraseType(phrase,"NAM");phrase})

  }

  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")
  private final val COMMON_NOUNS      = Seq("NN" , "NNS")
  private final val PROPER_NOUNS      = Seq("NNP", "NNPS")
  private final val ALL_NOUNS         = Seq("NN","NNS","NNP","NNPS","PRP","PRP$")

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isCommonNoun     (t: Token) = COMMON_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isProperNoun     (t: Token) = PROPER_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isNoun           (t: Token) = ALL_NOUNS.contains(t.posTag.categoryValue.toUpperCase)

  def predictMentionType(t: Token): Option[String] =
    if(isPersonalPronoun(t)) Some("PRO")
    else if(isCommonNoun(t)) Some("NOM")
    else if(isProperNoun(t)) Some("NAM")
    else None

  var FILTER_APPOS = true /* This flag is so that appositive filtering can be turned off.
                            If the mentions that we are extracting do not include the appositives as part of a mention
                            we want to make sure that we are extracting the appositives separately
                            default behavior is that we do filter the appositives.   */

  private def nerSpans(doc: Document): Seq[Phrase] = {
    (for (span <- doc.attr[ConllNerSpanBuffer]) yield
      new Phrase(span.section, span.start, span.length, -1)
      ).toSeq
  }

  private def NNPSpans(doc : Document) : Seq[Phrase] = {
    val spans = ArrayBuffer[ArrayBuffer[Token]]()
    spans += ArrayBuffer[Token]()
    for(section <- doc.sections; sentence <- section.sentences; token <- sentence.tokens) {
      if(spans.last.nonEmpty && spans.last.last.next != token) spans += ArrayBuffer[Token]()
      if(isProperNoun(token)) spans.last += token
    }
    if(spans.nonEmpty && spans.last.isEmpty) spans.remove(spans.length-1)
    (for(span <- spans) yield
      new Phrase(span.head.section, span.head.positionInSection, span.last.positionInSection-span.head.positionInSection+1, span.last.positionInSection-span.head.positionInSection)).toSeq
  }

  // [Assumes personal pronouns are single tokens.]
  private def personalPronounSpans(doc: Document): Seq[Phrase] = {
    //val coref = doc.getCoref
    (for (section <- doc.sections; s <- section.sentences;
           (t,i) <- s.tokens.zipWithIndex if isPersonalPronoun(t)) yield
        new Phrase(section, s.start + i, 1,0)
      ).toSeq
  }

  private def getHeadTokenIdx(m: Mention): Int = {
   val tokenIdxInSection =  getHead(
      m.phrase.head.sentence.parse,
      m.phrase.start until (m.phrase.start + m.phrase.length) //these are section-level offsets
    )
    val tokenIdxInSpan = tokenIdxInSection - m.phrase.start
    assert(tokenIdxInSpan >= 0 && tokenIdxInSpan <= m.phrase.length)
    tokenIdxInSpan
  }

  //this expects as input indices in the **document section** not the sentence
  //note that this never returns the root as the head, it always returns a pointer to an actual token in the sentence
  //it will either return the root of a parse tree span, or a token that is a child of the root
  def getHead(parse: ParseTree, subtree: Seq[Int]): Int = {
    val sentenceLevelIndices = subtree.map(i => i - parse.sentence.start)
    var curr = sentenceLevelIndices.head
    val leftBoundary = sentenceLevelIndices.head
    val rightBoundary = sentenceLevelIndices.last
    while(parse.parentIndex(curr) > 0 && containedInInterval(leftBoundary,rightBoundary,parse.parentIndex(curr))){
      curr = parse.parentIndex(curr)
    }
    curr + parse.sentence.start  //this shifts it back to have section-level indices
  }

  private def containedInInterval(left: Int, right: Int, testIndex: Int): Boolean = {
    testIndex >= left && testIndex <= right
  }

  final val copularVerbs = collection.immutable.HashSet[String]() ++ Seq("is","are","was","'m")

  final val allowedChildLabels = Set("amod", "det", "nn", "num", "hmod", "hyph", "possessive", "poss", "predet", "nmod", "dep")
  final val disallowedChildLabels = Set("conj", "punct", "prep", "cc", "appos", "npadvmod", "advmod", "quantmod", "partmod", "rcmod", "dobj", "nsubj", "infmod", "ccomp", "advcl", "aux", "intj", "neg", "preconj", "prt", "meta", "parataxis", "complm", "mark")

  private def nounPhraseSpans(doc: Document, nounFilter: Token => Boolean): Seq[Phrase] =  {
    val phrases = ArrayBuffer[Phrase]()
    for (section <- doc.sections; s <- section.sentences; (t, si) <- s.tokens.zipWithIndex if nounFilter(t);
         label = s.parse.label(t.positionInSentence).categoryValue
         if label != "nn" && label != "hmod")  {
      val children = s.parse.children(t.positionInSentence)
      children.foreach(c => {
        val cat = s.parse.label(c.positionInSentence).categoryValue
        if (!(allowedChildLabels.contains(cat) || disallowedChildLabels.contains(cat))) {
          println("BAD LABEL: " + cat)
          // println(doc.owplString(DepParser1))
        }
      })
      val goodChildren = children.filter{c =>
        val parseNode = s.parse.label(c.positionInSentence)
        allowedChildLabels.contains(parseNode.categoryValue) || (parseNode.categoryValue == "prep" && c.string.toLowerCase == "of")
      }
      val tokens = Seq(t) ++ goodChildren.map(c => s.parse.subtree(c.positionInSentence)).flatten
      val sTokens = tokens.sortBy(_.positionInSection)
      val start = sTokens.head.positionInSection
      val end = sTokens.last.positionInSection
      phrases += new Phrase(section, start, end-start+1, sTokens.zipWithIndex.filter(i => i._1 eq t).head._2)
    }
    phrases
  }

  private def dedup(phrases: Seq[Phrase]): Seq[Phrase] = {
    def dedupOverlappingMentions(phrases: Seq[Phrase]): Phrase = {
      if(phrases.length == 1){
        return phrases.head
      }else{
        phrases.find( _.attr[NounPhraseType].categoryValue == "NAM").getOrElse(phrases.head)
      }

    }
    phrases
      .groupBy(phrase => (phrase.section, phrase.start, phrase.length))
      .values.map(phraseSet => dedupOverlappingMentions(phraseSet)).toSeq
      .sortBy(phrase => (phrase.tokens.head.stringStart, phrase.length))
  }

   override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.phrase.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.phrase.attr[NounPhraseType].categoryValue+":"+m.phrase.indexOf(token)).mkString(","); case _ => "_" }
}


