package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp.wordnet.WordNet
import scala.collection.mutable
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PennPosTag
import collection.mutable.{ArrayBuffer, HashMap}
import cc.factorie.app.nlp.parse.ParseTree
import scala.Some
import cc.factorie.app.nlp.load.LoadConll2011
import cc.factorie.app.nlp.coref.Coref1Options

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:51 PM
 */

object MentionAlignment {
  def makeLabeledData(f: String, outfile: String ,portion: Double, useEntityTypes: Boolean, options: Coref1Options, map: DocumentAnnotatorMap): (Seq[Document],mutable.HashMap[String,GenericEntityMap[Mention]]) = {
    //first, get the gold data (in the form of factorie Mentions)
    val documentsAll = LoadConll2011.loadWithParse(f)
    val documents = documentsAll.take((documentsAll.length*portion).toInt)

    //make copies of the documents with no annotation
    val documentsToBeProcessedAll =  LoadConll2011.loadWithParse(f)
    val documentsToBeProcessed =  documentsToBeProcessedAll.take((documentsToBeProcessedAll.length*portion).toInt)
    //make sure that they have the same names, i.e. they were loaded in the same order and subspampled consistently
    documents.zip(documentsToBeProcessed).foreach(dd => assert(dd._1.name == dd._2.name))

    documentsToBeProcessed.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))  //remove the gold POS annotation
    documentsToBeProcessed.foreach(_.attr.remove[MentionList])
    //now do POS tagging and parsing on the extracted tokens
    cc.factorie.util.Threading.parForeach(documentsToBeProcessed, Runtime.getRuntime.availableProcessors())(findMentions(_)(map))

    //these are the offsets that mention boundary alignment will consider
    //the order of this array is very important, so that it will take exact string matches if they exist
    val shifts =  ArrayBuffer[Int]()
    shifts += 0
    for(i <- 1 to options.mentionAlignmentShiftWidth){
      shifts +=  i
      shifts += -1*i
    }

    //align gold mentions to detected mentions in order to get labels for detected mentions

    val alignmentInfo =  documents.zip(documentsToBeProcessed).par.map(d => alignMentions(d._1,d._2,WordNet,useEntityTypes, options, shifts))
    val entityMaps = new HashMap[String,GenericEntityMap[Mention]]() ++=  alignmentInfo.map(_._1).seq.toSeq

    //do some analysis of the accuracy of this alignment
    val prReports = alignmentInfo.map(_._2)
    val numCorrect = prReports.map(_.numcorrect).sum.toDouble
    val numGT = prReports.map(_.numGT).sum.toDouble
    val numDetected = prReports.map(_.numDetected).sum.toDouble
    println("precision = " + numCorrect/numDetected)
    println("recall = " + numCorrect/numGT)

    (documentsToBeProcessed  , entityMaps)
  }


  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
  //the alignment is based on an **exact match** between the mention boundaries
  def alignMentions(gtDoc: Document, detectedDoc: Document,wn: WordNet, useEntityTypes: Boolean, options: Coref1Options, shifts: Seq[Int]): ((String,GenericEntityMap[Mention]),PrecRecReport) = {
    val groundTruthMentions: MentionList = gtDoc.attr[MentionList]
    val detectedMentions: MentionList = detectedDoc.attr[MentionList]

    val name = detectedDoc.name

    val gtSpanHash = mutable.HashMap[(Int,Int),Mention]()
    gtSpanHash ++= groundTruthMentions.map(m => ((m.start,m.length),m))
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
    val entityHash = groundTruthMentions.groupBy(m => m.attr[Entity]).toMap
    val falsePositives1 = ArrayBuffer[Mention]()
    detectedMentions.foreach(m => {
      val alignment = checkContainment(gtSpanHash,gtHeadHash,m, options, shifts)
      if(alignment.isDefined){
        val gtMention = alignment.get
        m.attr +=  gtMention.attr[Entity]
        if(entityHash(gtMention.attr[Entity]).length > 1) relevantExactMatches += 1
        exactMatches += 1
        //val predictedEntityType = if(useEntityTypes) MentionEntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString))  else "O"
        //m.attr += new MentionEntityType(m,predictedEntityType)
        gtAligned(gtMention) = true
        if(debug) println("aligned: " + gtMention.string +":" + gtMention.start   + "  " + m.string + ":" + m.start)
      }else{
        if(debug) println("not aligned: "  +  m.string + ":" + m.start)
        val entityUID = m.document.name + unAlignedEntityCount
        val newEntity = new Entity(entityUID)
        m.attr += newEntity
       // val predictedEntityType = if(useEntityTypes) MentionEntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString))  else "O"
       // m.attr += new MentionEntityType(m,predictedEntityType)
        unAlignedEntityCount += 1
        falsePositives1 += m
      }
    })

    //now, we make a generic entity map
    val entityMap = new GenericEntityMap[Mention]

    val unAlignedGTMentions = gtAligned.filter(kv => !kv._2).map(_._1)
    val allCorefMentions =  detectedDoc.attr[MentionList] ++ unAlignedGTMentions

    allCorefMentions.foreach(m => entityMap.addMention(m, entityMap.numMentions.toLong))

    val corefEntities = allCorefMentions.groupBy(_.attr[Entity])
    corefEntities.flatMap(_._2.sliding(2)).foreach(p => {
      if (p.size == 2) entityMap.addCoreferentPair(p(0), p(1))
    })


    val relevantGTMentions = groundTruthMentions.count(m => entityHash(m.attr[Entity]).length > 1)
    ((name,entityMap),new PrecRecReport(relevantExactMatches,relevantGTMentions,detectedMentions.length))
  }

  def getHeadTokenInDoc(m: Mention): Int = {
    m.start + m.headTokenIndex
  }
  def checkContainment(startLengthHash: mutable.HashMap[(Int,Int),Mention], headHash: mutable.HashMap[Int,Mention] ,m: Mention, options: Coref1Options, shifts: Seq[Int]): Option[Mention] = {
    val start = m.start
    val length = m.length
    val headTokIdxInDoc = m.headTokenIndex + m.start
    val startIdx = start
    val endIdx = start + length

    //first, try to align it based on the mention boundaries


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
  case class PrecRecReport(numcorrect: Int,numGT: Int, numDetected: Int)

  def findMentions(d: Document)(implicit annotatorMap: DocumentAnnotatorMap) {
    cc.factorie.app.nlp.coref.mention.ParseBasedMentionFinding.FILTER_APPOS = true
    DocumentAnnotatorPipeline[MentionList](annotatorMap).process(d)
  }

  def assertParse(tokens: Seq[Token],parse: ParseTree): Unit = {
    val len = tokens.length
    parse.parents.foreach(j => assert(j < len))
  }

}


