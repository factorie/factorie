package cc.factorie.app.nlp.hcoref

import java.io.{ByteArrayInputStream, InputStreamReader, FileReader, BufferedReader}
import scala.io.Source
import cc.factorie._
import cc.factorie.util.NonValidatingXML
import cc.factorie.variable.BagOfWordsVariable
import cc.factorie.app.nlp.{Token, TokenSpan, DocumentAnnotatorPipeline, Document}
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.coref.ParseForwardCoref
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.la.{DenseTensorLike1, Tensor1}
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

    val docs = refMentions.map{ rMention =>
      val doc = new Document(map.getDoc(rMention.docId).toIterator.mkString("\n")).setName(rMention.docId)
      rMention.doc = Some(doc)
      doc
    }


    val mentions = refMentions.flatMap(RefMentionConverter.toDocEntNode).toSeq
    println("Found %d mentions in documents out of %d total mention (%.4f \\%)".format(mentions.size, refMentions.size, mentions.size.toDouble/refMentions.size))

    val splitPoint = (mentions.size * 0.75).toInt
    val (train, test) = mentions.splitAt(splitPoint)

    println("Split into %d training and %d testing".format(train.size, test.size))
    implicit val rand = new Random()

    val model = new DocEntityCorefModel(3.0, )
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
  var mentionCoords:Option[Iterable[(Int, Range)]] = None
 // var trees:Option[IndexedSeq[AnnotatedTree]] = None
  def tokenizedName:IndexedSeq[String] = {
    mentionCoords.get.flatMap { case(sentIdx, offs) =>
      offs.map { idx =>
        trees.get.apply(sentIdx).tokens(idx).token
      }
    }.toIndexedSeq
  }

  def mentionHeads = {
    mentionCoords.get.flatMap{ case(sentIdx, offs) =>
      offs.map{ idx =>
        val t = trees.get.apply(sentIdx).tokens(idx)
        ("head=" + t.headLabel) -> t.headValue
      }
    }.groupBy(_._1).mapValues(_.map(_._2))
  }
}


object RefMentionConverter {

  var pipelineElements = Seq(
    OntonotesForwardPosTagger,
    NoEmbeddingsConllStackedChainNer,
    OntonotesTransitionBasedParser,
    ParseForwardCoref
  )

  var pipeline = DocumentAnnotatorPipeline(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap.toMap, Nil, pipelineElements.flatMap(_.postAttrs))

  def toDocEntNode(ref:ReferenceMention):Option[Mention[DocEntityVars]] = {
    val doc = pipeline.process(ref.doc.get)

    val (s, e) = ref.offsets.get
    doc.getSectionByOffsets(s, e).flatMap(_.offsetSnapToTokens(s, e)) match {
      case Some(refSpan) =>
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

/*
object RefMentionConverter {
  def toBOWNode(ref:ReferenceMention):Node[DocEntityVars] = {

    val tokens = processor.tokenizer.getTokens(ref.sgmDoc.get.body).asScala
    val names = processor.tokenizer.getTokens(ref.name).asScala
    val nameBag = new BagOfWordsVariable()
    nameBag += ref.name
    names.foreach { name =>
      nameBag += name
    }
    val contextBag = new BagOfWordsVariable()
    tokens.foreach { token =>
      contextBag += token
    }
    val truth = new BagOfWordsVariable()
    truth += ref.entId
    new Mention(new BagOfWordsVars(nameBag, contextBag, truth))(null)
  }

  def toArrayVar(v:Array[Double]):MutableDoubleArrayVar = {
    val a = new MutableDoubleArrayVar
    a.set(v)(null)
    a
  }
  def toEmbeddingNode(ref:ReferenceMention, space:EmbeddingSpace):Node[RawEmbeddingVars] = {

    val tokens = processor.tokenizer.getTokens(ref.sgmDoc.get.body).asScala
    val names = processor.tokenizer.getTokens(ref.name).asScala
    val nameBag = new BagOfWordsVariable()
    nameBag += ref.name
    names.foreach { name =>
      nameBag += name
    }
    val contextEmbed = toArrayVar(space.embedPhrase(tokens))
    val nameEmbed = toArrayVar(space.embedPhrase(names))

    /*
    val contextBag = new BagOfWordsVariable()
    tokens.foreach { token =>
      contextBag += token
    }
    */
    val truth = new BagOfWordsVariable()
    truth += ref.entId
    new Mention(new RawEmbeddingVars(nameBag, contextEmbed, nameEmbed, truth))(null)
  }

  /*
  def toEmbeddingNode(ref:ReferenceMention, space:EmbeddingSpace):Node[RawEmbeddingVars] = {
    val contextEmbed = toArrayVar(space.embedPhrase(ref.trees.get.flatMap(_.tokens.map(_.token))))
    val nameEmbed = toArrayVar(space.embedPhrase(ref.tokenizedName))
    val nameBag = new BagOfWordsVariable()
    nameBag += ref.name
    ref.tokenizedName.foreach { token =>
      nameBag += token
    }
    val truth = new BagOfWordsVariable()
    truth += ref.entId
    new Mention(new RawEmbeddingVars(nameBag, contextEmbed, nameEmbed, truth))(null)
  }
  */
  /*
  def toStructuredEmbeddingNode(ref:ReferenceMention, space:EmbeddingSpace):Node[StructuredEmbeddingVars] = {
    val depFeatures = ref.mentionHeads.mapValues(s => space.embedPhrase(s.toSeq)).mapValues{ emb =>
      toArrayVar(emb)
    }.withDefault{ _ =>
      new MutableDoubleArrayVar
    }
    val contextEmbed = toArrayVar(space.embedPhrase(ref.trees.get.flatMap(_.tokens.map(_.token))))
    val nameEmbed = toArrayVar(space.embedPhrase(ref.tokenizedName))
    val nameBag = new BagOfWordsVariable()
    nameBag += ref.name
    ref.tokenizedName.foreach { token =>
      nameBag += token
    }
    val truth = new BagOfWordsVariable()
    truth += ref.entId
    new Mention(new StructuredEmbeddingVars)(null)
  }
  */
}
*/
