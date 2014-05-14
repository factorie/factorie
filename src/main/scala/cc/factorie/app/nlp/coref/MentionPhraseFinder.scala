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
package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos.PennPosDomain
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.ner.BilouConllNerTag
import cc.factorie.app.nlp.pos.PennPosTag
import scala.Option.option2Iterable

/** Trait for objects that return a list of Phrases given a Document 
    whose annotations includes those classes listed in prereqAttrs.
    This is not a DocumentAnnotator because it does not add its results to the Document.attr; 
    invocations to its apply method simple return a collection of Phrases.
    
    This design was chosen because these phrases are often used for coreference
    in which there are many coreference-specific choices of what mentions are filtered
    or included, and we didn't want to pollute the Document.attr with a tremendous number
    of postAttrs that are specific to individual coreference solutions.
    
    If you really want a DocumentAnnotator that saves its results, it is easy to
    create one uses a PhraseFinder.
     
    @author Andrew McCallum
    */
trait MentionPhraseFinder {
  def prereqAttrs: Seq[Class[_]]
  //def phrasePostAttrs: Seq[Class[_]] // TODO Should we have something like this?
  def apply(document:Document): Iterable[Phrase]
}


/** Apply returns a list of pronoun phrases, given PennPosTags.
    @author Andrew McCallum */
object PronounFinder extends MentionPhraseFinder {
  def prereqAttrs = Seq(classOf[PennPosTag])
  def apply(document:Document): Iterable[Phrase] = { 
    val phrases = document.tokens.filter(_.posTag.isPersonalPronoun).map(t => new Phrase(t.section, start=t.positionInSection, length=1,offsetToHeadToken = -1))
    for (phrase <- phrases) phrase.attr += new NounPhraseType(phrase, "PRO")
    phrases
  }
}

/** Apply returns a list of proper noun phrases, given BilouConllNerTags.
    @author Andrew McCallum */
object ConllProperNounPhraseFinder extends MentionPhraseFinder {
  def prereqAttrs = Seq(classOf[BilouConllNerTag])
  def apply(doc:Document): Seq[Phrase] = {
    val result = new ArrayBuffer[Phrase]
    for (section <- doc.sections; token <- section.tokens) {
      if (token.attr[BilouConllNerTag].categoryValue != "O") {
        val attr = token.attr[BilouConllNerTag].categoryValue.split("-")
        if (attr(0) == "U") {
          val phrase = new Phrase(section, token.positionInSection, length=1,offsetToHeadToken = -1)
          phrase.attr += new ConllPhraseEntityType(phrase, attr(1))
          DeterministicNounPhraseTypeLabeler.process(phrase)
          result += phrase
        } else if (attr(0) == "B") {
          if (token.hasNext) {
            var lookFor = token.next
            while (lookFor.hasNext && lookFor.attr[BilouConllNerTag].categoryValue.matches("(I|L)-" + attr(1))) lookFor = lookFor.next
            // TODO Be more clever in determining the headTokenOffset
            val phrase = new Phrase(section, token.positionInSection, length=lookFor.positionInSection - token.positionInSection,offsetToHeadToken = -1)
            phrase.attr += new ConllPhraseEntityType(phrase, attr(1))
            DeterministicNounPhraseTypeLabeler.process(phrase)
            result += phrase
          } else {
            val phrase = new Phrase(section, token.positionInSection, length=1,offsetToHeadToken = -1)
            phrase.attr += new ConllPhraseEntityType(phrase, attr(1))
            DeterministicNounPhraseTypeLabeler.process(phrase)
            result += phrase
          }
        }
      }
    }
    result
  }
}

/** Apply returns a list of acronym noun phrases.
    @author Andrew McCallum */
object AcronymNounPhraseFinder extends MentionPhraseFinder {
  def prereqAttrs = Seq(classOf[Token])
  def apply(doc:Document): Seq[Phrase] = {
    val result = new ArrayBuffer[Phrase]
    for (section <- doc.sections; token <- section.tokens) {
      // Matches middle word of "Yesterday IBM announced" but not "OBAMA WINS ELECTION"
      if ( token.string.length > 2 && !token.containsLowerCase && Character.isUpperCase(token.string(0)) && (token.getNext ++ token.getPrev).exists(_.containsLowerCase)) {
        val phrase = new Phrase(section, token.positionInSection, length=1,offsetToHeadToken = -1)
        phrase.attr += new ConllPhraseEntityType(phrase, "ORG")
        phrase.attr += new NounPhraseType(phrase, "NAM")
        result += phrase
      }
    }
    result
  }
}

/** Apply returns a list of NNP-indicated proper noun phrases, given PennPosTags.
    @author Andrew McCallum */
object NnpPosNounPhraseFinder extends MentionPhraseFinder {
  def prereqAttrs = Seq(classOf[PennPosTag])
  def apply(doc:Document): Seq[Phrase] = {
    val result = new ArrayBuffer[Phrase]
    var start = 0
    for (section <- doc.sections) {
      val tokens = section.tokens
      while (start < tokens.length) {
        val token = tokens(start)
        var end = start
        while (end < tokens.length && tokens(end).posTag.intValue == PennPosDomain.nnpIndex) end += 1
        if (end != start && tokens(end-1).posTag.intValue == PennPosDomain.nnpIndex) {
          val phrase = new Phrase(section, token.positionInSection, length=end-start,offsetToHeadToken = -1)
          phrase.attr += new NounPhraseType(phrase, "NAM")
          NounPhraseEntityTypeLabeler.process(phrase)
        }
        start = math.max(start+1, end)
      }
    }
    result
  }
}




