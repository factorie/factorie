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
package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase.{Phrase,OntonotesPhraseEntityType,NounPhraseType}
import cc.factorie.app.nlp.coref.{Mention,MentionList,WithinDocCoref}
import scala.collection.mutable.ListBuffer
import cc.factorie.app.nlp.load.{ChunkTag, BILOUNestedChunkTag, BILOUChunkTag}

/**
 * User: cellier
 * Date: 10/28/13
 * Time: 11:24 PM
 */


/*
 * Object to retrieve two layers of Nested BILOU Tags
 */
object NestedNPChunkMentionFinder extends NPChunkMentionFinder[BILOUNestedChunkTag]{
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
object NPChunkMentionFinder extends NPChunkMentionFinder[BILOUChunkTag]

class NPChunkMentionFinder[L<:ChunkTag](implicit m: Manifest[L]) extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], m.runtimeClass)
  def postAttrs = Seq(classOf[MentionList], classOf[OntonotesPhraseEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.phrase.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[NounPhraseType].categoryValue+":"+ m.phrase.attr[OntonotesPhraseEntityType].categoryValue +":" +m.phrase.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  def process(document: Document) = {
    val mentions = addChunkMentions(document)
    document.attr += new MentionList(mentions.sortBy(m => (m.phrase.head.stringStart, m.phrase.length)))
    document
  }

  //Sets mention entity type to empty in case an entity type labeler is not run on the mentions retrieved
  def addChunkMentions(document: Document): Seq[Mention] = {
    val coref = document.getCoref
    getMentionSpans(document).map{labelSpan =>
      val s = labelSpan
      val p = new Phrase(s, s.length-1)
      val m = coref.addMention(p)
      p.attr += new OntonotesPhraseEntityType(p,"") // TODO Why the empty string here?? -akm
      m
    }
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



