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

import cc.factorie.app.nlp.wordnet.WordNet
import scala.collection.mutable
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PennPosTag
import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase.{ParseBasedPhraseFinder, NounPhraseEntityTypeLabeler, OntonotesPhraseEntityType, ParseAndNerBasedPhraseFinder}

/** Used for training with predicted mentions.
 * If the predicted mention is equal to or within some specified alignment width in options we add the true spans entity label if any
 * Otherwise we add the mention to the ground truth coref as a ground truth singleton.*/
object MentionAlignment {
  def makeLabeledData(documents:Seq[Document], outfile: String, useEntityTypes: Boolean, options: CorefOptions, map: DocumentAnnotatorMap): (Seq[Document]) = {

    //remove the gold POS annotation
    if(!options.useGoldBoundaries) documents.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))

    val shifts =  ArrayBuffer[Int]()
    shifts += 0
    for(i <- 1 to options.mentionAlignmentShiftWidth){
      shifts +=  i
      shifts += -1*i
    }

    //align gold mentions to detected mentions in order to get labels for detected mentions
    val alignmentInfo =  documents.par.map(d => alignMentions(d,WordNet,useEntityTypes, options, shifts))

    //do some analysis of the accuracy of this alignment
    val numCorrect = alignmentInfo.map(_.numcorrect).sum.toDouble
    val numGT = alignmentInfo.map(_.numGT).sum.toDouble
    val numDetected = alignmentInfo.map(_.numDetected).sum.toDouble
    println("precision = " + numCorrect + " / " + numDetected + " = " + numCorrect/numDetected)
    println("recall = " + numCorrect + " / " + numGT + " = " + numCorrect/numGT)


    documents
  }

  def findMentions(doc: Document,options:CorefOptions,annotatorMap: DocumentAnnotatorMap = null) {
    if(options.useGoldBoundaries){
      doc.getTargetCoref.mentions.foreach(m => doc.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType])
    }else if(!options.useNERMentions){
      ParseAndNerBasedPhraseFinder.FILTER_APPOS = true
      val map = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      DocumentAnnotatorPipeline(map, prereqs=Nil, ParseBasedPhraseFinder.prereqAttrs.toSeq).process(doc)
      ParseBasedPhraseFinder.getPhrases(doc).foreach(doc.coref.addMention)
    }else {
      val defaultMap = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      val preReqs = ConllProperNounPhraseFinder.prereqAttrs ++ PronounFinder.prereqAttrs ++AcronymNounPhraseFinder.prereqAttrs
      DocumentAnnotatorPipeline.apply(map=defaultMap.toMap, prereqs=Nil, preReqs).process(doc)
      (ConllProperNounPhraseFinder(doc) ++ PronounFinder(doc) ++ AcronymNounPhraseFinder(doc)).foreach(doc.getCoref.addMention)
    }
    DocumentAnnotatorPipeline.apply(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap, prereqs=Nil, ForwardCoref.prereqAttrs).process(doc)
  }

  case class PrecRecReport(numcorrect: Int, numGT: Int, numDetected: Int)

  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
  //the alignment is based on an **exact match** between the mention boundaries
  def alignMentions(gtDoc: Document, wn: WordNet, useEntityTypes: Boolean, options: CorefOptions, shifts: Seq[Int],annotatorMap:DocumentAnnotatorMap = null): (PrecRecReport) = {
    val groundTruthMentions = gtDoc.targetCoref.entities.filter(!_.isSingleton).flatMap(e => e.children).toSeq
    val relevantGTMentions = groundTruthMentions.size

    //Set predicted mentions on the coref attribute of the document
    if(gtDoc.coref.mentions.isEmpty) findMentions(gtDoc,options)
    val detectedMentions = gtDoc.getCoref.mentions.toSeq

    val gtSpanHash = mutable.HashMap[(Int,Int),Mention]()
    gtSpanHash ++= groundTruthMentions.map(m => ((m.phrase.start, m.phrase.length), m))
    val gtHeadHash = mutable.HashMap[Int,Mention]()
    gtHeadHash ++= groundTruthMentions.map(m => (getHeadTokenInDoc(m),m))

    val gtAligned = mutable.HashMap[Mention,Boolean]()
    gtAligned ++= groundTruthMentions.map(m => (m,false))

    var exactMatches = 0
    var relevantExactMatches = 0
    var unAlignedEntityCount = 0
    val debug = false

    //here, we create a bunch of new entity objects, that differ from the entities that the ground truth mentions point to
    //however, we index them by the same uIDs that the ground mentions use
    val entityHash = groundTruthMentions.groupBy(m => m.entity).toMap
    val falsePositives1 = ArrayBuffer[Mention]()
    detectedMentions.foreach(m => {
      val alignment = checkContainment(gtSpanHash,gtHeadHash,m, options, shifts)
      if(alignment.isDefined){
        val gtMention = alignment.get
        val entity = gtMention.entity
        //If aligned gold mention was a gold entity
        if(entity != null) {
          if(entityHash(gtMention.entity).length > 1 && !gtAligned(gtMention)) relevantExactMatches += 1
          gtAligned(gtMention) = true
          if(debug) println("aligned: " + gtMention.string +":" + gtMention.phrase.start   + "  " + m.phrase.string + ":" + m.phrase.start +" " + gtMention.entity.uniqueId)

          exactMatches += 1
          if(options.useEntityType) m.phrase.attr += gtMention.phrase.attr[OntonotesPhraseEntityType]
          else NounPhraseEntityTypeLabeler.process(m.phrase)
          val newEntity = gtDoc.coref.entityFromUniqueId(gtMention.entity.uniqueId)
          newEntity += m
        }
        //If the aligned gold mention was a loaded singleton, use any annotation information if wanted
        else{
          if(options.useEntityType) m.phrase.attr += gtMention.phrase.attr[OntonotesPhraseEntityType]
          else NounPhraseEntityTypeLabeler.process(m.phrase)
          val newEntity = gtDoc.coref.entityFromUniqueId(gtDoc.name + "-" + gtDoc.targetCoref.entities.size+unAlignedEntityCount)
          newEntity += m
          unAlignedEntityCount += 1
          falsePositives1 += m
        }
        //Make the close alignment our new ground truth for training
      }else{
          if(debug) println("not aligned: "  +  m.string + ":" + m.phrase.start)
          //Add our mention which was unaligned to the target coref as a singleton for training
          m.phrase.attr += new OntonotesPhraseEntityType(m.phrase,"O")
          val newEntity = gtDoc.coref.entityFromUniqueId(gtDoc.name + "-" + gtDoc.targetCoref.entities.size+unAlignedEntityCount)
          newEntity += m
          unAlignedEntityCount += 1
          falsePositives1 += m
      }
    })

    val countUnAligned = gtAligned.count(!_._2)
    val newCoref = new WithinDocCoref(gtDoc)
    newCoref.target = gtDoc.coref
    //So we don't have to perform mention finding twice
    gtDoc.coref.mentions.foreach(m => newCoref.addMention(m.phrase))
    gtDoc.attr += newCoref

    new PrecRecReport(relevantGTMentions-countUnAligned,relevantGTMentions,detectedMentions.length)
  }

  def getHeadTokenInDoc(m: Mention): Int = m.phrase.start + m.phrase.headTokenOffset

  def checkContainment(startLengthHash: mutable.HashMap[(Int,Int),Mention], headHash: mutable.HashMap[Int,Mention] ,m: Mention, options: CorefOptions, shifts: Seq[Int]): Option[Mention] = {
    val start = m.phrase.start
    val length = m.phrase.length
    val headTokIdxInDoc = m.phrase.headTokenOffset + m.phrase.start
    val startIdx = start
    val endIdx = start + length

    for (startShift <- shifts; endShift <- shifts; if startIdx + startShift <= endIdx + endShift) {
      val newStart = startIdx + startShift
      val newEnd = endIdx + endShift
      val key = (newStart, newEnd - newStart)
      if(startLengthHash.contains(key))
        return Some(startLengthHash(key))
    }

    //next, back off to aligning it based on the head token
    if(headHash.contains(headTokIdxInDoc))
      return Some(headHash(headTokIdxInDoc))
    None
  }
}


