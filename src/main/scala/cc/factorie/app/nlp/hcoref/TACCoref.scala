package cc.factorie.app.nlp.hcoref

import java.io.{FileReader, BufferedReader}
import scala.io.Source
import cc.factorie._
import cc.factorie.util.NonValidatingXML
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.coref.ParseForwardCoref
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import scala.util.Random

/**
 * @author John Sullivan
 */
object TACCoref {
  def main(args:Array[String]) {
    val tacRoot = args(0)
    val evalPath = args(1)

    val map = new Tac2009FlatDocumentMap(tacRoot)

    val refMentions = ProcessQueries.loadQueries(evalPath + ".xml", evalPath + ".tab")

    println("loaded %d mentions/queries in %d entities.".format(refMentions.size, refMentions.map(_.entId).toSet.size))

    refMentions.foreach{ rMention =>
      val doc = new Document(map.getDoc(rMention.docId).toIterator.mkString("\n")).setName(rMention.docId)
      rMention.doc = Some(doc)
      doc
    }


    val pipelineElements = Seq(
      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      ParseForwardCoref
    )

    val pipeline = DocumentAnnotatorPipeline(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap.toMap, Nil, pipelineElements.flatMap(_.postAttrs))

    val converter = new RefMentionConverter(pipeline)

    val mentions = refMentions.flatMap(converter.toDocEntNode).toSeq
    println("Found %d mentions in documents out of %d total mention (%.4f \\%)".format(mentions.size, refMentions.size, mentions.size.toDouble/refMentions.size))

    val splitPoint = (mentions.size * 0.75).toInt
    val (train, test) = mentions.splitAt(splitPoint)

    println("Split into %d training and %d testing".format(train.size, test.size))
    implicit val rand = new Random()

    val tacCoref = new DocEntityCoref {implicit val random: Random = rand

      def estimateIterations(mentionCount: Int) = mentionCount * 100

      val model = new DocEntityCorefModel(4.0, 0.25, 1.0, 2.0, 0.25, 1.0, 0.25, 3.0, 0.25, 1.0, 0.25)

      val autoStopThreshold = 10000
    }

    val sampler = tacCoref.getSampler(test)
    sampler.infer
  }
}

/**
 * Takes a docId and returns the raw text of the corresponding document
 */
trait DocumentMap {
  def getDoc(docId:String):BufferedReader
}

class Tac2009FlatDocumentMap(tacRoot:String) extends DocumentMap {
  def getDoc(docId:String):BufferedReader = {
    val filePath = s"$tacRoot/$docId.sgm"
    new BufferedReader(new FileReader(filePath))
  }
}

object ProcessQueries {


  def loadQueries(queryXMLFile:String, queryTabFile:String):Iterable[ReferenceMention] = {
    val entMap =  Source.fromFile(queryTabFile).getLines().map { line =>
      val Array(mentId, entId, _) = line.split("\\s+")
      mentId -> entId
    }.toMap

    NonValidatingXML.loadFile(queryXMLFile).\\("kbpentlink").\\("query").map { qXML =>
      val id = (qXML \ "@id").text.trim
      val name = (qXML \ "name").text.trim
      val docName = (qXML \ "docid").text.trim
      val beg = qXML \ "beg"
      val end = qXML \ "end"
      assert(beg.isEmpty == end.isEmpty)
      val offsets:Option[(Int, Int)] = if (beg.isEmpty || end.isEmpty) None else Some(beg.text.toInt, end.text.toInt)
      ReferenceMention(id, name, docName, offsets, entMap(id))
    }
  }
}

case class ReferenceMention(id:String, name:String, docId:String, offsets:Option[(Int, Int)], entId:String) {
  var doc:Option[Document] = None
}


class RefMentionConverter(val pipeline:DocumentAnnotationPipeline) {

  def toDocEntNode(ref:ReferenceMention):Option[Mention[DocEntityVars]] = {
    val doc = pipeline.process(ref.doc.get)

    val (s, e) = ref.offsets.get
    doc.getSectionByOffsets(s, e).flatMap(_.offsetSnapToTokens(s, e)) match {
      case Some(refSpan) =>
        implicit val d:DiffList = null
        val xMent = new Mention[DocEntityVars](new DocEntityVars())
        xMent.variables.names ++= refSpan.map{t:Token => t.lemmaString}.toCountBag
        xMent.variables.context ++= refSpan.contextWindow(10).map(_.lemmaString).toCountBag

        Option(doc.coref).flatMap{_.findOverlapping(refSpan)} match {
          case Some(ment) =>
            xMent.variables.++=(DocEntityVars.fromWithinDocEntity(ment.entity))(null)
            xMent.withinDocEntityId = ment.entity.uniqueId
          case None => println("Could not find coref or align mention: " + ref)
        }
        Some(xMent)
      case None =>
        println("WARNING: Failed to find tokens for reference mention: " + ref)
        None
    }
  }
}
