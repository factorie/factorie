package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Token, DocumentAnnotator, Document, DocumentAnnotatorLazyMap}
import scala.collection.mutable
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.app.nlp.mention.{Entity, MentionList, Mention}
import cc.factorie.app.nlp.pos.PTBPosLabel
import scala.collection.mutable.HashMap
import java.io.PrintWriter
import cc.factorie.app.nlp.parse.ParseTree

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:51 PM
 */

object MentionAlignment {
  def makeLabeledData(f: String, outfile: String ,wn: WordNet, corefGazetteers: CorefGazetteers, portion: Double, useEntityTypes: Boolean, options: Coref2Options)(implicit map: DocumentAnnotatorLazyMap): (Seq[Document],mutable.HashMap[String,GenericEntityMap[Mention]]) = {
    //first, get the gold data (in the form of factorie Mentions)
    val documentsAll = ConllCorefLoader.loadWithParse(f)
    val documents = documentsAll.take((documentsAll.length*portion).toInt)

    //make copies of the documents with no annotation
    val documentsToBeProcessedAll =  ConllCorefLoader.loadWithParse(f)
    val documentsToBeProcessed =  documentsToBeProcessedAll.take((documentsToBeProcessedAll.length*portion).toInt)
    //make sure that they have the same names, i.e. they were loaded in the same order and subspampled consistently
    documents.zip(documentsToBeProcessed).foreach(dd => assert(dd._1.name == dd._2.name))

    documentsToBeProcessed.foreach( d => d.tokens.foreach(t => t.attr.remove[PTBPosLabel]))  //remove the gold POS annotation
    documentsToBeProcessed.foreach(_.attr.remove[MentionList])
    //now do POS tagging and parsing on the extracted tokens
    documentsToBeProcessed.par.foreach(findMentions(_))

    //align gold mentions to detected mentions in order to get labels for detected mentions
    val alignmentInfo =  documents.zip(documentsToBeProcessed).par.map(d => alignMentions(d._1,d._2,wn,corefGazetteers,useEntityTypes, options))
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

  def printDocPOS(d: Document, out: PrintWriter): Unit = {
    out.println("#begin document (" + d.name + ")")
    var tokenCount = 0
    d.sentences.foreach(s => {
      s.tokens.zipWithIndex.foreach(ti => {
        val stuffToPrint = mutable.Seq(d.name,"0",tokenCount,ti._2,ti._1.string,ti._1.attr[PTBPosLabel].categoryValue)
        out.println(stuffToPrint.mkString("\t"))
        tokenCount += 1
      })
      out.println()
    })
    d.tokens.zipWithIndex.foreach(ti => {

      val stuffToPrint = mutable.Seq(d.name,"0",ti._2,ti._1.string,ti._1.attr[PTBPosLabel].categoryValue)
      out.println(stuffToPrint.mkString("\t"))
    })
    out.println()
  }

  def printDoc(d: Document, out: PrintWriter): Unit = {
    out.println("#begin document (" + d.name + ")")
    d.attr[MentionList].filter(m => m.attr.contains[EntityType]).foreach(m => {
      out.println(m.start + "\t" + m.length + "\t" + m.headTokenIndex + "\t" + m.attr[EntityType].categoryValue + "\t" + m.attr[EntityKey].name)
    })
    out.println()
  }

  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
  //the alignment is based on an **exact match** between the mention boundaries
  def alignMentions(gtDoc: Document, detectedDoc: Document,wn: WordNet, cg: CorefGazetteers,useEntityTypes: Boolean, options: Coref2Options): ((String,GenericEntityMap[Mention]),PrecRecReport) = {
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

    //here, we create a bunch of new entity objects, that differ from the entities that the ground truth mentions point to
    //however, we index them by the same uIDs that the ground mentions use
    val entityHash = groundTruthMentions.groupBy(m => m.attr[Entity]).toMap

    detectedMentions.foreach(m => {
      val alignment = checkContainment(gtSpanHash,gtHeadHash,m, options)
      if(alignment.isDefined){
        val gtMention = alignment.get
        m.attr +=  gtMention.attr[Entity]
        if(entityHash(gtMention.attr[Entity]).length > 1) relevantExactMatches += 1
        exactMatches += 1
        val predictedEntityType = if(useEntityTypes) EntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString),cg)  else "UKN"
        m.attr += new EntityType(m,predictedEntityType)
        gtAligned(gtMention) = true
      }else{
        val entityUID = m.document.name + unAlignedEntityCount
        val newEntity = new Entity(entityUID)
        m.attr += newEntity
        val predictedEntityType = if(useEntityTypes) EntityTypeAnnotator1Util.classifyUsingRules(m.span.tokens.map(_.lemmaString),cg)  else "UKN"
        m.attr += new EntityType(m,predictedEntityType)
        unAlignedEntityCount += 1
      }
    })
    if(true){
      def mentionString(m: Mention): String = {
        m.span.string
      }

      val missedDetections =  gtAligned.filter(x => !x._2).map(_._1)
      val falsePositives =  detectedMentions.filter(m => !entityHash.contains(m.attr[Entity]))
      println("\n\nMissed Detections\n" + missedDetections.map(mentionString(_)).mkString("\n") )
      println("\nFalse Positives\n"   + missedDetections.map(mentionString(_)).mkString("\n") )

    }

    //first, we add everything as a corefmention to the detectedDoc
    val cml = new MentionList
    cml ++= detectedDoc.attr[MentionList]
    detectedDoc.attr += cml

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
    //todo: a better way to do this is to get the head from the dependency parse produced on the detected mentions
    m.start + m.headTokenIndex
  }
  def checkContainment(startLengthHash: mutable.HashMap[(Int,Int),Mention], headHash: mutable.HashMap[Int,Mention] ,m: Mention, options: Coref2Options): Option[Mention] = {
    val start = m.start
    val length = m.length
    val headTokIdxInDoc = m.headTokenIndex + m.start
    val startIdx = start
    val endIdx = start + length

    //first, try to align it based on the mention boundaries
    val shifts = (-1*options.mentionAlignmentShiftWidth to options.mentionAlignmentShiftWidth)
    for (startShift <- shifts; endShift <- shifts; if startIdx + startShift <= endIdx + endShift) {
      val newStart = startIdx + startShift
      val newEnd = endIdx + endShift
      val key = (newStart, newEnd - newStart)
      if(startLengthHash.contains(key))
        return Some(startLengthHash(key))
    }

    //next, align it based on the head token
    if(headHash.contains(headTokIdxInDoc))
      return Some(headHash(headTokIdxInDoc))
    None
  }
  case class PrecRecReport(numcorrect: Int,numGT: Int, numDetected: Int)

  def findMentions(d: Document)(implicit annotatorMap: DocumentAnnotatorLazyMap) {
    cc.factorie.app.nlp.mention.ParseBasedMentionFinding.FILTER_APPOS = false
    val a = new DocumentAnnotator {
      def tokenAnnotationString(token: Token) = null
      def process1(document: Document) = document
      def prereqAttrs = Seq(classOf[MentionList])
      def postAttrs = Nil
    }
    a.process(d)
  }

  def assertParse(tokens: Seq[Token],parse: ParseTree): Unit = {
    val len = tokens.length
    parse.parents.foreach(j => assert(j < len))
  }

}


