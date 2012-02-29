package cc.factorie.app.nlp

import segment._
import coref._

import ner.NerSpan
import xml.{XML, NodeSeq}
import java.io.File
import relation.RelationVariables.{RelationMention, RelationMentions}

/**
 * @author brian martin
 * @date 12/23/11
 */

// TODO: consider moving this info into variables.
trait ACEEntityIdentifiers {
  def eId: String
  def eType: String
  def eSubtype: String
  def eClass: String
}

trait ACEMentionIdentifiers {
  def mId: String
  def offsetStart: Int
  def offsetEnd: Int
}

trait ACERelationIdentifiers {
  def rId: String
  def rType: String
  def rSubtype: String
}

class ACEFileIdentifier(val fileId: String)

object LoadACE {

  private val matchTag = "<[A-Za-z=_\"/ ]*>".r

  private def makeDoc(sgm: String): Document = {
    val doc = new Document(sgm, matchTag.replaceAllIn(io.Source.fromFile(sgm).mkString, _ => ""))
    doc.attr += new ACEFileIdentifier(sgm.dropRight(4) + ".apf.xml")
    Tokenizer.process(doc)
    SentenceSegmenter.process(doc)

    // trailing tokens should be in a sentence
    val end = doc.sentences.last.end
    if (end != doc.size - 1)
      new Sentence(doc, end + 1, doc.size - 1 - end)(null)
    doc
  }

  private def tokenIndexAtCharIndex(charOffset: Int, doc: Document): Int = {
    require(charOffset >= 0 && charOffset <= doc.string.length)
    var i = 0
    while (i < doc.tokens.size) {
      val t = doc.tokens(i)
      if (t.stringStart <= charOffset && charOffset <= t.stringStart + t.stringLength)
        return i
      i += 1
    }
    return -1
  }

  private def getTokenIdxAndLength(mention: NodeSeq, doc: Document): (Int, Int) = {
    val start = getAttr(mention \ "extent" \ "charseq", "START").toInt
    val end = getAttr(mention \ "extent" \ "charseq", "END").toInt + 1
    val startTokenIdx = tokenIndexAtCharIndex(start, doc)
    val endTokenIdx = tokenIndexAtCharIndex(end, doc)
    (startTokenIdx, endTokenIdx - startTokenIdx + 1)
  }

  private def getAttr(ns: NodeSeq, key: String): String = {
    val k = ns(0).attribute(key).getOrElse(null)
    if (k != null) k.text
    else "None"
  }

  def addMentionsFromApf(apf: NodeSeq, doc: Document): Unit = {
    for (entity <- apf \\ "entity") {
      val e = new EntityVariable(entity \ "entity_attributes" \ "name" \ "charseq" text)
      e.attr += new ACEEntityIdentifiers {
        def eId = getAttr(entity, "ID")
        def eType = getAttr(entity, "TYPE")
        def eSubtype = getAttr(entity, "SUBTYPE")
        def eClass = getAttr(entity, "CLASS")
      }

      for (mention <- entity \ "entity_mention") {
        val (start, length) = getTokenIdxAndLength(mention, doc)
        val m = new NerSpan(doc, e.attr[ACEEntityIdentifiers].eType, start, length)(null) with PairwiseMention
        if (m.sentence == null)
          println("NULL mention: (%d, %d) -> %s".format(start, length, m.string))
        m.attr += new ACEMentionIdentifiers {
          def mId = getAttr(mention, "ID")
          def offsetStart = getAttr(mention \ "extent" \ "charseq", "START").toInt
          def offsetEnd = getAttr(mention \ "extent" \ "charseq", "END").toInt
        }
        m.attr += new EntityRef(m, e)
      }
    }
  }

  private def lookupEntityMention(id: String, doc: Document): PairwiseMention =
    doc.spans.filter {
      s =>
        val a = s.attr[ACEMentionIdentifiers]
        a != null && a.mId == id
    }.head.asInstanceOf[PairwiseMention]

  def addRelationsFromApf(apf: NodeSeq, doc: Document): Unit = {
    doc.attr += new RelationMentions
    for (relation <- apf \\ "relation") {
      val identifiers = new ACERelationIdentifiers {
        val rId = getAttr(relation, "ID")
        val rType = getAttr(relation, "TYPE")
        val rSubtype = getAttr(relation, "SUBTYPE")
      }

      for (mention <- relation \ "relation_mention") {
        val args = mention \ "rel_mention_arg" map {
          arg => lookupEntityMention(getAttr(arg, "ENTITYMENTIONID"), doc)
        }
        assert(args.size == 2)
        val m = new RelationMention(args.head, args.last, identifiers.rType) // + "-" + identifiers.rSubtype)
        m.attr += identifiers
        doc.attr[RelationMentions].add(m)(null)
        args.foreach(_.attr.getOrElseUpdate(new RelationMentions).add(m)(null))
      }
    }
  }

  // drops the first two lines (xml decl, and dtd)
  private def loadXML(apfFile: String): NodeSeq = XML.loadString(io.Source.fromFile(apfFile).getLines().drop(2).mkString("\n"))

  // TODO: consider renaming this to fromFile to match the API for other loaders.
  // But if renamed, how can the user know that apf.xml is required (instead of alf.xml or .xml)?
  def fromApf(apfFile: String): Document = fromApf(apfFile, makeDoc(apfFile.dropRight(8) + ".sgm"))

  def fromApf(apfFile: String, doc: Document): Document = {
    addMentionsFromApf(loadXML(apfFile), doc)
    addRelationsFromApf(loadXML(apfFile), doc)
    doc
  }

  def fromDirectory(dir: String, takeOnly: Int = Int.MaxValue): Seq[Document] =
    new File(dir).listFiles().filter(_.getName.endsWith(".apf.xml")).take(takeOnly).map(f => fromApf(f.getAbsolutePath))

  def main(args: Array[String]): Unit = {
    val docs = fromDirectory(args(0))
    for (d <- docs)
      d.spansOfClass[TokenSpanMention].foreach(s => println(s))
  }

}
