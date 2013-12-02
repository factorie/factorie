package cc.factorie.app.nlp.mention

import java.io.{File, FileOutputStream, FileInputStream}
import cc.factorie.app.nlp._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import cc.factorie.app.nlp.phrase.{ChunkerOpts, CRFChunker}
import cc.factorie.app.nlp.load.{BILOU2LayerChunkTag, BILOUChunkTag}
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import cc.factorie.app.nlp.coref.ForwardCorefTrainer.opts
import cc.factorie.app.nlp.mention.Mention
import scala.collection.mutable


/**
 * User: cellier
 * Date: 10/28/13
 * Time: 11:24 PM
 * Finite State machine implementation to grab NP spans
 */

class ChunkBasedMentionList extends MentionList


class MultiLayerNPChunkMentionFinder extends NPChunkMentionFinder{
  override def getMentionSpans(document: Document): Seq[(String,TokenSpan)] ={
    val mentionSpans = ArrayBuffer[(String,TokenSpan)]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t.attr[BILOU2LayerChunkTag].categoryValue.split(":").map(layer => t -> layer)).map(layer => (layer(0),layer(1)))
      val (innerTags,outerTags) = chunkTags.unzip
      mentionSpans ++= getNPChunkSpans(s,innerTags)
      mentionSpans ++= getNPChunkSpans(s,outerTags)
    }
    mentionSpans.seq
  }
}

class NPChunkMentionFinder extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence],classOf[BILOU2LayerChunkTag])
  def postAttrs = Seq(classOf[MentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  def process(document: Document) = {
    val mentions = addChunkMentions(document)
    document.attr += new MentionList() ++= mentions.sortBy(m => (m.head.stringStart, m.length))
    document
  }
  //Todo: convert to multiple encoding capable
  def addChunkMentions(document: Document): Seq[Mention] = {
    getMentionSpans(document).map{labelSpan =>
      val s = labelSpan._2
      val m = new Mention(s, s.length-1)
      m.attr += new MentionType(m, "NAM")
      m
    }
  }

  def getMentionSpans(document: Document): Seq[(String,TokenSpan)] ={
    val mentionSpans = ArrayBuffer[(String,TokenSpan)]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t-> t.attr[BILOUChunkTag].categoryValue)
      mentionSpans ++= getNPChunkSpans(s,chunkTags)
    }
    mentionSpans.seq
  }

  def getNPChunkSpans(s: Sentence,chunkTags: IndexedSeq[(Token, String)]):Seq[(String,TokenSpan)]={
    val spans = ArrayBuffer[(String,TokenSpan)]()
    chunkTags.map{case (t,chunk) =>
      if (chunk != "O") {
        if(chunk == "U-NP") spans += (chunk -> new TokenSpan(s.section, t.positionInSection, 1))
        else if(chunk == "B-NP"){
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.sentence == t.sentence && chunkTags(chunkTags.map(_._1.string).indexOf(lookFor.string))._2.matches("(I|L)-NP")) lookFor = lookFor.next
            spans += (chunk -> new TokenSpan(s.section, t.positionInSection, lookFor.positionInSection - t.positionInSection))
          } else  spans += (chunk -> new TokenSpan(s.section, t.positionInSection, 1))
        }
      }
    }
    spans.toSeq
  }

}



