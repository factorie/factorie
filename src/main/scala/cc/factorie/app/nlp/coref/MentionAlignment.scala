package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import scala.collection.mutable
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosTag}
import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase.{NounPhraseEntityTypeLabeler, Phrase, OntonotesPhraseEntityType}
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain
import cc.factorie.app.nlp.load.{EntityKey, LoadConll2011}
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.ParseAndNerBasedPhraseFinder

/** Used for training with predicted mentions.
 * If the predicted mention is equal to or within some specified alignment width in options we add the true spans entity label if any
 * Otherwise we add the mention to the ground truth coref as a ground truth singleton.
 * @author apassos
 * Date: 6/27/13
 * Time: 12:51 PM
 * @note Updated Cellier April 2014
 */

object MentionAlignment {
  def makeLabeledData(documents:Seq[Document], outfile: String, useEntityTypes: Boolean, options: CorefOptions, map: DocumentAnnotatorMap): (Seq[Document]) = {
    documents.foreach(d=>assert(d.targetCoref.mentions.forall(_.entity != null),"ERRRRORRR"))

    if(!options.useGoldBoundaries) documents.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))  //remove the gold POS annotation
    if(!options.useEntityType) documents.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))
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
    println("precision = " + numCorrect/numDetected)
    println("recall = " + numCorrect/numGT)

    documents
  }

  def findMentions(doc: Document,options:CorefOptions,annotatorMap: DocumentAnnotatorMap = null) {
    if(options.useGoldBoundaries){

      doc.getTargetCoref.mentions.foreach(m => doc.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType])
    }else if(!options.useNERMentions){
      ParseAndNerBasedPhraseFinder.FILTER_APPOS = true
      val map = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      DocumentAnnotatorPipeline(map,ParseAndNerBasedPhraseFinder.prereqAttrs.toSeq).process(doc)
      ParseAndNerBasedPhraseFinder.getPhrases(doc).foreach(doc.coref.addMention)
    }else {
      val map = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      val prereqs = ConllProperNounPhraseFinder.prereqAttrs ++ PronounFinder.prereqAttrs ++ AcronymNounPhraseFinder.prereqAttrs
      DocumentAnnotatorPipeline(map,prereqs).process(doc)
      (ConllProperNounPhraseFinder(doc) ++ PronounFinder(doc) ++ AcronymNounPhraseFinder(doc)).foreach(doc.getCoref.addMention)
    }
  }

  case class PrecRecReport(numcorrect: Int,numGT: Int, numDetected: Int)

  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
  //the alignment is based on an **exact match** between the mention boundaries
  def alignMentions(gtDoc: Document, wn: WordNet, useEntityTypes: Boolean, options: CorefOptions, shifts: Seq[Int],annotatorMap:DocumentAnnotatorMap = null): (PrecRecReport) = {
    val groundTruthMentions = gtDoc.getTargetCoref.mentions.toSeq
    //Set predicted mentions on the coref attribute of the document
    if(gtDoc.coref.mentions.isEmpty) findMentions(gtDoc,options)
    val detectedMentions = gtDoc.getCoref.mentions.toSeq

    val gtSpanHash = mutable.HashMap[(Int,Int),Mention]()
    gtSpanHash ++= groundTruthMentions.map(m => ((m.phrase.start, m.phrase.length), m))
    val gtHeadHash = mutable.HashMap[Int,Mention]()
    gtHeadHash ++= groundTruthMentions.map(m => (getHeadTokenInDoc(m),m))

    val gtAligned = mutable.HashMap[Mention,Boolean]()
    gtAligned ++= groundTruthMentions.map(m => (m,false))
    assert(gtDoc.targetCoref.mentions.forall(_.entity != null),"ERRRRORRR")
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
          //Todo:If we didn't get a correct alignment do we want to make the close alignment the new ground truth?
          if(gtMention.phrase.value != m.phrase.value){
            gtDoc.getTargetCoref.addMention(m.phrase,gtMention.entity)
            gtDoc.getTargetCoref.deleteMention(gtMention)
          }
          m.attr +=  new EntityKey(gtMention.entity.uniqueId)
          if(entityHash(gtMention.entity).length > 1) relevantExactMatches += 1
          exactMatches += 1
          if(useEntityTypes) m.phrase.attr += gtMention.phrase.attr
          else
            NounPhraseEntityTypeLabeler.process(m.phrase)
          gtAligned(gtMention) = true
          if(debug) println("aligned: " + gtMention.string +":" + gtMention.phrase.start   + "  " + m.phrase.string + ":" + m.phrase.start)
      }else{
          if(debug) println("not aligned: "  +  m.string + ":" + m.phrase.start)
          val entityUID = m.phrase.document.name + unAlignedEntityCount
          //NounPhraseEntityTypeLabeler.process(m.phrase)
          //Add our mention which was unaligned to the target coref as a singleton for training
          gtDoc.getTargetCoref.addMention(m.phrase)
          //Make a ground truth singleton entity for training?
          //val newEntity = gtDoc.getTargetCoref.entityFromUniqueId(entityUID) // was: new Entity(entityUID)
          //NounPhraseEntityTypeLabeler.process(m.phrase)
          unAlignedEntityCount += 1
          falsePositives1 += m
      }
    })

    val unAlignedGTMentions = gtAligned.filter(kv => !kv._2).map(_._1)
    //unAlignedGTMentions.foreach(mention => gtDoc.coref.addMention(mention.phrase, mention.entity))

    val relevantGTMentions = groundTruthMentions.count(m => entityHash(m.entity).length > 1)
    new PrecRecReport(relevantExactMatches,relevantGTMentions,detectedMentions.length)
  }

  def getHeadTokenInDoc(m: Mention): Int = {
    m.phrase.start + m.phrase.headTokenOffset
  }

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


object MentionFilter{
  var debug = false
  /**
   * Given a coref holder with predicted mentions on it, perform some standard filtering and return new phrases corresponding to the new set of mentions.
   * Note this does not change the document's coref at all and simply returns the phrases.
   * It would be the responsibility of the caller to erase current mentions and add the new phrases as mentions
   * @param coref Coref class already annotated with mentions
   * @return Seq[Parse] new list of phrases suggested by the filter to be the new list of mentions */
  def filterMentions(coref: WithinDocCoref): Seq[Phrase] = {
    if(coref.mentions.isEmpty){
      // Just a warning and not an assert because there are some documents which have no ground truth mentions
      println("Warning, tried to filter document without any mentions: " + coref.document.name)
      return Seq[Phrase]()
    }
    val mentionExtents = (0 until coref.document.sentenceCount).map(i => new mutable.HashSet[Phrase])
    val sentMents = coref.mentions.toSeq.groupBy(_.phrase.sentence)
    for (sent <- sentMents.keysIterator) {
      // Extract NE spans: filter out O, QUANTITY, CARDINAL, CHUNK
      // Throw out NE types which aren't mentions
      val predMentions = sentMents(sent).map(_.phrase).filterNot(filterNE).map(expandNEsToIncludePossessive)
      mentionExtents(sent.indexInSection) ++= predMentions
      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
      val posOfInterest = Seq("NP","PRP", "PRP$")
      for (label <- posOfInterest) {
        mentionExtents(sent.indexInSection) ++= coref.mentions.toSeq.filter(m=>posOfInterest.contains(m.phrase.head.posTag)).map(_.phrase).filter(phrase => filterSpanIfInNE(phrase,predMentions))
      }
    }
    // Now take maximal mentions with the same heads
    val filteredPredMentionsSorted = (0 until coref.document.sentenceCount).map(i => new ArrayBuffer[Phrase])
    for (sentIdx <- 0 until mentionExtents.size) {
      val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headToken)
      // Look from smallest head first
      for (head <- protoMentionsByHead.keys.toSeq) {
        // Find the biggest span containing this head
        var currentBiggest: Phrase = null
        for (ment <- protoMentionsByHead(head)) {
          // Overlapping but neither is contained in the other
          if (currentBiggest != null && ((ment.start < currentBiggest.start && ment.end < currentBiggest.end) || (ment.start > currentBiggest.start && ment.end > currentBiggest.end))) {
            println("WARNING: mentions with the same head but neither contains the other")
            println(currentBiggest.string + "("+currentBiggest.start +","+currentBiggest.end+")" + "    :   " + ment.string+ "("+ment.start +","+ment.end+")")
          }
          // This one is bigger
          if (currentBiggest == null || (ment.start <= currentBiggest.start && ment.end >= currentBiggest.end)) {
            currentBiggest = ment
          }else if (ment.last.posTag.intValue == PennPosDomain.posIndex){
            println("Added: " + ment.string)
            filteredPredMentionsSorted(sentIdx) += ment
          }else println("Removed: " + ment.string)

        }
        filteredPredMentionsSorted(sentIdx) += currentBiggest
        // ENGLISH ONLY: don't remove appositives
        for (ment <- protoMentionsByHead(head)) {
          val isNotBiggest = ment.start != currentBiggest.start || ment.end != currentBiggest.end
          val isAppositiveLike = ment.last != ment.sentence.last && (ment.last.next.string == "," || ment.last.next.posTag.categoryValue == "CC")
          if (isNotBiggest && isAppositiveLike) {
            filteredPredMentionsSorted(sentIdx) += ment
            println("Added: " + ment.string)
          }
        }
      }
    }
    filteredPredMentionsSorted.flatten.toSeq
  }
  def filterNE(phrase:Phrase): Boolean = {
    Vector(OntonotesEntityTypeDomain.O,
      OntonotesEntityTypeDomain.QUANTITY,
      OntonotesEntityTypeDomain.CARDINAL,
      OntonotesEntityTypeDomain.PERCENT).contains(phrase.attr[OntonotesPhraseEntityType].intValue) && phrase.attr[OntonotesPhraseEntityType].exactMatch
  }
  def filterSpanIfInNE(phrase:Phrase,nerMentions:Seq[Phrase]): Boolean = !nerMentions.exists(ment => ment.start <= phrase.start && phrase.end <= ment.end)

  // From Berkeley's Mention Finding: NE's often don't contain trailing 's, so add this manually
  def expandNEsToIncludePossessive(phrase:Phrase): Phrase = {
    val sent = phrase.sentence
    if (phrase.last != sent.last && phrase.last.next.posTag.intValue == PennPosDomain.posIndex) {
      val newPhrase = new Phrase(phrase.section,phrase.start,phrase.length+1,phrase.headTokenOffset);newPhrase.attr += phrase.attr; newPhrase
    } else phrase
  }


  /**
   * Test filter with mentions with gold boundaries
   * @param args Conll 2011 training data and testing data
   */
  def main(args:Array[String]):Unit={
    val trainDocs = LoadConll2011.loadWithParse(args(0),limitNumDocuments = 300)
    trainDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => assert(m.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")))
    val phrases = trainDocs.flatMap(d => filterMentions(d.getTargetCoref))
    val trueDocs = trainDocs
    trueDocs.foreach(d=> OntonotesTransitionBasedParser.process(d))
    trueDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => assert(m.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")))
    val truePhrases = trueDocs.flatMap(d => d.getTargetCoref.mentions).filter(m=>m.entity != null && m.entity.uniqueId.indexOf('*') != -1)

    if(debug){
      println("_________TRUTH___________")
      truePhrases.foreach(p => println(p.string + ": " + p.phrase.headToken.string + "   " + p.entity.uniqueId))
    }

    trueDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => d.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType]))

    val correctMatches = truePhrases.map(_.phrase.value).intersect(phrases.map(_.value)).size
    val spuriousMentions = phrases.map(_.value).diff(truePhrases.map(_.phrase.value)).filter(m => !truePhrases.map(_.phrase.value).contains(m)).groupBy(_.map(_.string).mkString(" ")).toSeq.sortBy(-_._2.size).filter(_._2.size > 5)
    val missingMentions = truePhrases.map(_.phrase.value).diff(phrases.map(_.value)).filter(m => !phrases.map(_.value).contains(m)).groupBy(_.map(_.string).mkString(" ")).toSeq.sortBy(-_._2.size).filter(_._2.size > 5)

    if(debug){
      //Print any extra mentions in order of how often the string was wrongly extracted
      println("_________Extra Mentions________")
      spuriousMentions.foreach{case (menText, menSet) => println(menText + ": " + menSet.size)}
      //Print any missing mentions in order of how often the string was wrongly extracted
      println("____________Missing_________")
      missingMentions.foreach{case (menText, menSet) => println(menText + ": " + menSet.size)}
    }
    //Print general mention accuracy
    println("Missing: " + missingMentions.flatMap(_._2).size)
    println("Correct Matches: " +correctMatches)
    println("Spurious Matches: " + (phrases.size - correctMatches))


    println("Train: "+trainDocs.length+" documents, " + trainDocs.map(d => d.getTargetCoref.mentions.size).sum.toFloat / trainDocs.length + " mentions/doc")

    val testDocs = LoadConll2011.loadWithParse(args(1),limitNumDocuments = 20)
    testDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => d.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType]))
    for(doc<-testDocs;mention<-doc.coref.mentions) if(!mention.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) mention.phrase.attr += new OntonotesPhraseEntityType(mention.phrase,"O")

    println("Test : "+ testDocs.length+" documents, " + testDocs.map(d => d.coref.mentions.size).sum.toFloat / testDocs.length + " mention/doc")

    (trainDocs,testDocs)
  }
}
