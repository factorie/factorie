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
import scala.collection.mutable.ListBuffer
import cc.factorie.app.nlp.load.{ChunkTag, BILOUNestedChunkTag, BILOUChunkTag}

/** User: cellier
 *  Date: 10/28/13
 *  Time: 11:24 PM
 */

/** Object to retrieve two layers of Nested BILOU Tags*/
object NestedNPChunkPhraseFinder extends NPChunkPhraseFinder[BILOUNestedChunkTag]{
  //Splits tag value and calls to retrieve NPs for the inner tags and outer tags
  override def getMentionSpans(document: Document): Seq[TokenSpan] ={
    val mentionSpans = ListBuffer[TokenSpan]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t.attr[BILOUNestedChunkTag].categoryValue.split(":").map(layer => t -> layer)).map(layer => (layer(0),layer(1)))
      val (innerTags,outerTags) = chunkTags.unzip
      //splitting up the tags into the inner and outer tags and grabbing noun span separately seemed like the safest option
      //but might not be the fastest
      mentionSpans ++= getNPChunkSpans(s,innerTags)
      mentionSpans ++= getNPChunkSpans(s,outerTags)
    }
    mentionSpans.seq
  }
}
//Default for MentionFinder is BILOU Notation over BIO since BILOU performed best for NP mention finding
object NPChunkMentionFinder extends NPChunkPhraseFinder[BILOUChunkTag]

class NPChunkPhraseFinder[L<:ChunkTag](implicit m: Manifest[L]) extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], m.runtimeClass)
  def postAttrs = Seq(classOf[PhraseList])
  override def tokenAnnotationString(token:Token): String = token.document.attr[PhraseList].filter(phrase => phrase.contains(token)) match { case phraseSeq:Seq[Phrase] if phraseSeq.length > 0 => phraseSeq.map(phrase => phrase.attr[NounPhraseType].categoryValue+":"+ phrase.attr[OntonotesPhraseEntityType].categoryValue +":" +phrase.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  def process(document: Document) = {
    val phrases = getChunkPhrases(document)
    document.attr += new PhraseList(phrases.sortBy(phrase => (phrase.head.stringStart, phrase.length)))
    document
  }

  def getChunkPhrases(document: Document): Seq[Phrase] = {
    getMentionSpans(document).map(span => new Phrase(span))//Get the head from the phrase's heuristic labeler
  }

  def getMentionSpans(document: Document): Seq[TokenSpan] ={
    val mentionSpans = ListBuffer[TokenSpan]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t-> t.attr[BILOUChunkTag].categoryValue)
      mentionSpans ++= getNPChunkSpans(s,chunkTags)
    }
    mentionSpans.seq
  }

  def getNPChunkSpans(s: Sentence,chunkTags: IndexedSeq[(Token, String)]):Seq[TokenSpan]={
    val spans = ListBuffer[TokenSpan]()
    chunkTags.map{case (t,chunk) =>
      if (chunk != "O") {
        if(chunk == "U-NP") spans += new TokenSpan(s.section, t.positionInSection, 1)
        else if(chunk == "B-NP"){
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.sentence == t.sentence && chunkTags(chunkTags.map(_._1.string).indexOf(lookFor.string))._2.matches("(I|L)-NP")) lookFor = lookFor.next
            spans += new TokenSpan(s.section, t.positionInSection, lookFor.positionInSection - t.positionInSection)
          } else  spans += new TokenSpan(s.section, t.positionInSection, 1)
        }
      }
    }
    spans.toSeq
  }
}



