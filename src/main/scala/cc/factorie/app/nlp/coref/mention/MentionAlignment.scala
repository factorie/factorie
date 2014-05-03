package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp.wordnet.WordNet
import scala.collection.mutable
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosTag}
import collection.mutable.{ArrayBuffer, HashMap}
import cc.factorie.app.nlp.phrase.{Phrase,OntonotesPhraseEntityType}
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain
import cc.factorie.app.nlp.load.LoadConll2011
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:51 PM
 */

/*



 */

object MentionAlignment{
  def main(args:Array[String]):Unit={
    val allTrainDocs = LoadConll2011.loadWithParse("src/main/resources/conll-train-clean.txt",true)
    val trainDocs = allTrainDocs.take(300)
    trainDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => assert(m.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")))
    val phrases = trainDocs.flatMap(d => filterMentions(d.getTargetCoref))
   // println("+++++++++++++=========================" +
    //  "+++++++++++++")
    phrases.foreach(p => println(p.string + ": " + p.headToken.string))
    //trainDocs.foreach(m => d.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType])

    //val trueTrainDocs = LoadConll2011.loadWithParse("src/main/resources/conll-train-clean.txt",false)
    val trueDocs = trainDocs
    trueDocs.foreach(d=> OntonotesTransitionBasedParser.process(d))
    trueDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => assert(m.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")))
    val truePhrases = trueDocs.flatMap(d => d.getTargetCoref.mentions).filter(m=>m.entity != null && m.entity.uniqueId.indexOf('*') != -1)

    println("_________TRUTH___________")

    //truePhrases.foreach(p => println(p.string + ": " + p.phrase.headToken.string + "   " + p.entity.uniqueId))

    trueDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => d.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType]))
    //for(truePhrase <- truePhrases){
      //if(phrases.map(_.value).contains(truePhrase.phrase.value))
       // println("Match")
      //if (phrases.map(_.value.start).contains(truePhrase.phrase.value.start) ){
      //  println("same start: " + truePhrase.phrase.string)
      //  if(phrases.map(p => p.value.length + p.value.start).contains(truePhrase.phrase.value.length + truePhrase.phrase.value.start)){
      //    println("same ends: " + truePhrase.phrase.string)
      //    //println(truePhrase.phrase.value.chain == predPhrase.value.chain)
     //   }
     // } else println("No Match: " + truePhrase.phrase.string)

   // }
   // truePhrases.foreach{
   //   truePhrase =>
        //truePhrase.phrase.value =
   // }
    var missing = 0
   // truePhrases.map(_.phrase.value).diff(phrases.map(_.value)).foreach{m => if(truePhrases.map(_.phrase.value).contains(m)) println("Missing:"+m.map(_.string).mkString(" ")) else println("Extra:"+m.map(_.string).mkString(" "));missing +=1 }
    val correctMatches = truePhrases.map(_.phrase.value).intersect(phrases.map(_.value)).size
    val spuriousMentions = phrases.map(_.value).diff(truePhrases.map(_.phrase.value)).filter(m => !truePhrases.map(_.phrase.value).contains(m)).groupBy(_.map(_.string).mkString(" ")).toSeq.sortBy(-_._2.size)
    spuriousMentions.foreach{case (menText, menSet) => println(menText + ": " + menSet.size)}

    println("____________Missing_________")
    val extraMentions = truePhrases.map(_.phrase.value).diff(phrases.map(_.value)).filter(m => !phrases.map(_.value).contains(m)).groupBy(_.map(_.string).mkString(" ")).toSeq.sortBy(-_._2.size)
    extraMentions.foreach{case (menText, menSet) => println(menText + ": " + menSet.size)}
    println("Missing: " + extraMentions.flatMap(_._2).size)
    println("Correct Matches: " +correctMatches)
    println("Spurious Matches: " + (phrases.size - correctMatches))


    println("Train: "+trainDocs.length+" documents, " + trainDocs.map(d => d.getTargetCoref.mentions.size).sum.toFloat / trainDocs.length + " mentions/doc")

    //val allTestDocs = LoadConll2011.loadWithParse("src/main/resources/conll-test-clean.txt")
    //val testDocs = allTestDocs.take(1)
    //testDocs.foreach(d=> d.getTargetCoref.mentions.foreach(m => d.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType]))
    //for(doc<-testDocs;mention<-doc.coref.mentions) if(!mention.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) mention.phrase.attr += new OntonotesPhraseEntityType(mention.phrase,"O")

    //println("Test : "+ testDocs.length+" documents, " + testDocs.map(d => d.coref.mentions.size).sum.toFloat / testDocs.length + " mention/doc")

    //(trainDocs,testDocs)
  }
  def filterMentions(coref: WithinDocCoref): Seq[Phrase] = {
    val mentionExtents = (0 until coref.document.sentenceCount).map(i => new mutable.HashSet[Phrase])
    val sentMents = coref.mentions.toSeq.groupBy(_.phrase.sentence)
    for (sent <- sentMents.keysIterator) {
      // Extract NE spans: filter out O, QUANTITY, CARDINAL, CHUNK
      // Throw out NE types which aren't mentions

      val neProtoMentions = sentMents(sent).map(_.phrase).filterNot(filterNE).map(expandNEsToIncludePossessive)
      mentionExtents(sent.indexInSection) ++= neProtoMentions
      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
      val filterSpanIfInNE: Phrase => Boolean = phrase => neProtoMentions.filter(ment => ment.start <= phrase.start && phrase.end <= ment.end).size == 0
      val posOfInterest = Seq("NP","PRP", "PRP$")
      for (label <- posOfInterest) {
        mentionExtents(sent.indexInSection) ++= coref.mentions.toSeq.filter(m=>posOfInterest.contains(m.phrase.head.posTag)).map(_.phrase).filter(filterSpanIfInNE)
      }
    }
    // Now take maximal mentions with the same heads
    val filteredProtoMentionsSorted = (0 until coref.document.sentenceCount).map(i => new ArrayBuffer[Phrase])
    for (sentIdx <- 0 until mentionExtents.size) {
      val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headToken)
      // Look from smallest head first
      for (head <- protoMentionsByHead.keys.toSeq) {
        // Find the biggest span containing this head
        var currentBiggest: Phrase = null
        for (ment <- protoMentionsByHead(head)) {
          // Overlapping but neither is contained in the other
          if (currentBiggest != null && ((ment.start < currentBiggest.start && ment.end < currentBiggest.end) || (ment.start > currentBiggest.start && ment.end > currentBiggest.end))) {
            println("WARNING: mentions with the same head but neither contains the other");
            println(currentBiggest.string + "("+currentBiggest.start +","+currentBiggest.end+")" + "    :   " + ment.string+ "("+ment.start +","+ment.end+")")
           // println(("  " + rawDoc.words(sentIdx).slice(ment.startIdx, ment.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
           // println(("  " + rawDoc.words(sentIdx).slice(currentBiggest.startIdx, currentBiggest.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
          }
          // This one is bigger
          if (currentBiggest == null || (ment.start <= currentBiggest.start && ment.end >= currentBiggest.end)) {
            currentBiggest = ment

          }else if (ment.last.posTag.intValue == PennPosDomain.posIndex){
            println("Added: " + ment.string)
            filteredProtoMentionsSorted(sentIdx) += ment
          }else println("Removed: " + ment.string)

        }
        filteredProtoMentionsSorted(sentIdx) += currentBiggest
        // ENGLISH ONLY: don't remove appositives
        for (ment <- protoMentionsByHead(head)) {
          val isNotBiggest = ment.start != currentBiggest.start || ment.end != currentBiggest.end
          val isAppositiveLike = ment.last != ment.sentence.last && (ment.last.next.string == "," || ment.last.next.posTag.categoryValue == "CC")
          if (isNotBiggest && isAppositiveLike) {
            filteredProtoMentionsSorted(sentIdx) += ment
            println("Added: " + ment.string)
          }
        }
      }
    }
    filteredProtoMentionsSorted.flatten.toSeq//.map(sortProtoMentionsLinear(_))
  }
  def filterNE(phrase:Phrase): Boolean = {
    Vector(OntonotesEntityTypeDomain.QUANTITY,
           OntonotesEntityTypeDomain.CARDINAL,
           OntonotesEntityTypeDomain.PERCENT).contains(phrase.attr[OntonotesPhraseEntityType].intValue) && phrase.attr[OntonotesPhraseEntityType].exactMatch
  }
      // ENGLISH ONLY: Annoyingly, NE's often don't contain trailing 's, so add this manually
  def expandNEsToIncludePossessive(phrase:Phrase): Phrase = {
    val sent = phrase.sentence
    if (phrase.last != sent.last && phrase.last.next.posTag.intValue == PennPosDomain.posIndex) {
      val newPhrase = new Phrase(phrase.section,phrase.start,phrase.length+1,phrase.headTokenOffset);newPhrase.attr += phrase.attr; newPhrase
    } else phrase
  }

}



//
//
object MentionAlignmentCreation {
  def makeLabeledData(f: String, outfile: String ,portion: Double, useEntityTypes: Boolean, options: Coref1Options, map: DocumentAnnotatorMap): (Seq[Document]) = {
//    //first, get the gold data (in the form of factorie Mentions)
    val documentsAll = LoadConll2011.loadWithParse(f)
    val documents = documentsAll.take((documentsAll.length*portion).toInt)

    for(doc <- documents; mention <- doc.getTargetCoref.mentions){
      assert(mention.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")
      doc.coref.addMention(mention.phrase).phrase.attr += mention.phrase.attr[OntonotesPhraseEntityType]
    }
    documents.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))  //remove the gold POS annotation
    val shifts =  ArrayBuffer[Int]()
    shifts += 0
    for(i <- 1 to options.mentionAlignmentShiftWidth){
      shifts +=  i
      shifts += -1*i
    }
//
//    //align gold mentions to detected mentions in order to get labels for detected mentions
//
    val alignmentInfo =  documents.zip(documents).par.map(d => alignMentions(d._1,d._2,WordNet,useEntityTypes, options, shifts))
    //val entityMaps = new HashMap[String,GenericEntityMap[Mention]]() ++=  alignmentInfo.map(_._1).seq.toSeq
//
//    //do some analysis of the accuracy of this alignment
    val prReports = alignmentInfo
    val numCorrect = prReports.map(_.numcorrect).sum.toDouble
    val numGT = prReports.map(_.numGT).sum.toDouble
    val numDetected = prReports.map(_.numDetected).sum.toDouble
    println("precision = " + numCorrect/numDetected)
    println("recall = " + numCorrect/numGT)
//
//    (documentsToBeProcessed)
    documents
  }
  def findMentions(d: Document)(implicit annotatorMap: DocumentAnnotatorMap) {
    cc.factorie.app.nlp.coref.mention.ParseBasedMentionFinding.FILTER_APPOS = true
    DocumentAnnotatorPipeline[MentionList](annotatorMap).process(d)
  }

  case class PrecRecReport(numcorrect: Int,numGT: Int, numDetected: Int)

  def alignMentions(gtDoc: Document, detectedDoc: Document,wn: WordNet, useEntityTypes: Boolean, options: Coref1Options, shifts: Seq[Int]): (PrecRecReport) = {
    val groundTruthMentions = gtDoc.getTargetCoref.mentions.toSeq
    val detectedMentions = detectedDoc.coref.mentions.toSeq

    val name = detectedDoc.name

    val gtSpanHash = mutable.HashMap[(Int,Int),Mention]()
    gtSpanHash ++= groundTruthMentions.map(m => ((m.phrase.start, m.phrase.length), m))
    val gtHeadHash = mutable.HashMap[Int,Mention]()
    gtHeadHash ++= groundTruthMentions.map(m => (getHeadTokenInDoc(m),m))

    val gtAligned = mutable.HashMap[Mention,Boolean]()
    gtAligned ++= groundTruthMentions.map(m => (m,false))
    var exactMatches = 0
    var relevantExactMatches = 0
    var unAlignedEntityCount = 0
    var debug = false
    //here, we create a bunch of new entity objects, that differ from the entities that the ground truth mentions point to
    //however, we index them by the same uIDs that the ground mentions use
    val entityHash = groundTruthMentions.groupBy(m => m.entity).toMap
    val falsePositives1 = ArrayBuffer[Mention]()
    detectedMentions.foreach(m => {
      val alignment = checkContainment(gtSpanHash,gtHeadHash,m, options, shifts)
      if(alignment.isDefined){
          val gtMention = alignment.get
          m.attr +=  gtMention.entity
          if(entityHash(gtMention.entity).length > 1) relevantExactMatches += 1
          exactMatches += 1
          //val predictedEntityType = if(useEntityTypes) MentionEntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString))  else "O"
          //m.attr += new MentionEntityType(m,predictedEntityType)
          gtAligned(gtMention) = true
          if(debug) println("aligned: " + gtMention.string +":" + gtMention.phrase.start   + "  " + m.phrase.string + ":" + m.phrase.start)
      }else{
          if(debug) println("not aligned: "  +  m.string + ":" + m.phrase.start)
          val entityUID = m.phrase.document.name + unAlignedEntityCount
          // TODO We still have to examine all this carefully concerning which Mentions are created in the target and non-target WithinDocCoref. -akm
          val newEntity = detectedDoc.getTargetCoref.entityFromUniqueId(entityUID) // was: new Entity(entityUID)
          m.attr += newEntity
         // val predictedEntityType = if(useEntityTypes) MentionEntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString))  else "O"
         // m.attr += new MentionEntityType(m,predictedEntityType)
          unAlignedEntityCount += 1
          falsePositives1 += m
      }
    })

    //now, we make a coref map
    val entityMap = new WithinDocCoref(gtDoc)

    val unAlignedGTMentions = gtAligned.filter(kv => !kv._2).map(_._1)
    val allCorefMentions =  detectedDoc.coref.mentions ++ unAlignedGTMentions
    val coref = detectedDoc.getCoref
    allCorefMentions.foreach(m => coref.addMention(m.phrase, coref.newEntity()))

    val corefEntities = allCorefMentions.groupBy(_.entity)
    //corefEntities.flatMap(_._2.sliding(2)).foreach(p => {
    //  if (p.size == 2) entityMap.addCoreferentPair(p(0), p(1))
    //})


    val relevantGTMentions = groundTruthMentions.count(m => entityHash(m.entity).length > 1)
    new PrecRecReport(relevantExactMatches,relevantGTMentions,detectedMentions.length)
  }

  def getHeadTokenInDoc(m: Mention): Int = {
    m.phrase.start + m.phrase.headTokenOffset
  }

  def checkContainment(startLengthHash: mutable.HashMap[(Int,Int),Mention], headHash: mutable.HashMap[Int,Mention] ,m: Mention, options: Coref1Options, shifts: Seq[Int]): Option[Mention] = {
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
//
//    //make copies of the documents with no annotation
//    val documentsToBeProcessedAll =  LoadConll2011.loadWithParse(f)
//    val documentsToBeProcessed =  documentsToBeProcessedAll.take((documentsToBeProcessedAll.length*portion).toInt)
//    //make sure that they have the same names, i.e. they were loaded in the same order and subspampled consistently
//    documents.zip(documentsToBeProcessed).foreach(dd => assert(dd._1.name == dd._2.name))
//
//
//    //documents.foreach(_.attr.remove[MentionList])
//    //now do POS tagging and parsing on the extracted tokens
//    //if(options.useNonGoldBoundaries){
//    //  cc.factorie.util.Threading.parForeach(documentsToBeProcessed, Runtime.getRuntime.availableProcessors())(findMentions(_)(map))
//    //}
//
//    //these are the offsets that mention boundary alignment will consider
//    //the order of this array is very important, so that it will take exact string matches if they exist
//
//  }
//
//
//  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
//  //the alignment is based on an **exact match** between the mention boundaries
//
//
//
//
//
//
//
//
//  def assertParse(tokens: Seq[Token],parse: ParseTree): Unit = {
//    val len = tokens.length
//    parse.parents.foreach(j => assert(j < len))
//  }
//
//}
//
//
