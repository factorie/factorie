package cc.factorie.app.nlp.load
import cc.factorie.app.nlp._


import segment._
import hcoref._

import ner.NerSpan
import xml.{XML, NodeSeq}
import java.io.File
import relation.RelationVariables.{RelationMention, RelationMentions}

// TODO: consider moving this info into variables.
trait ACEEntityIdentifiers {
  def eId: String

  def eType: String

  def eSubtype: String

  def eClass: String
}

trait ACEMentionIdentifiers {
  def mId: String

  def mType: String

  def ldcType: String

  def offsetStart: Int

  def offsetEnd: Int
}

trait ACERelationIdentifiers {
  def rId: String

  def rType: String

  def rSubtype: String
}

class ACEFileIdentifier(val fileId: String)

class ACEMentionSpan(doc: Section, val labelString: String, start: Int, length: Int) extends TokenSpan(doc, start, length) with cc.factorie.app.nlp.hcoref.TokenSpanMention with PairwiseMention {
  override def toString = "ACEMentionSpan(" + length + "," + labelString + ":" + this.phrase + ")"
}
class ACEMentionSpanList extends TokenSpanList[ACEMentionSpan]

object LoadACE {

  private val matchTag = "<[A-Za-z=_\"/ ]*>".r

  private def makeDoc(sgm: String): Document = {
    val source = io.Source.fromFile(sgm)
    val sgmString = source.mkString
    source.close()
    val doc = new Document(matchTag.replaceAllIn(sgmString, _ => "")).setName(sgm)
    doc.attr += new ACEFileIdentifier(sgm.dropRight(4) + ".apf.xml")
    ClearTokenizer.process(doc)
    ClearSegmenter.process(doc)

    // trailing tokens should be in a sentence
    val end = doc.asSection.sentences.last.end
    if (end != doc.asSection.length - 1)
      new Sentence(doc.asSection, end + 1, doc.asSection.length - 1 - end)
    doc
  }

  private def tokenIndexAtCharIndex(charOffset: Int, doc: Document): Int = {
    require(charOffset >= 0 && charOffset <= doc.string.length)
    var i = 0
    for (t <- doc.tokens) {
      if (t.stringStart <= charOffset && charOffset <= t.stringEnd) return i
      i += 1
    }
    -1
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
      val e = new EntityVariable((entity \ "entity_attributes" \ "name" \ "charseq").text)
      e.attr += new ACEEntityIdentifiers {
        def eId = getAttr(entity, "ID")
        def eType = getAttr(entity, "TYPE")
        def eSubtype = getAttr(entity, "SUBTYPE")
        def eClass = getAttr(entity, "CLASS")
      }
      
      val spanList = doc.attr += new ACEMentionSpanList 
      for (mention <- entity \ "entity_mention") {
        val (start, length) = getTokenIdxAndLength(mention, doc)
        val m = new ACEMentionSpan(doc.asSection, e.attr[ACEEntityIdentifiers].eType, start, length)
        spanList += m
        if (m.sentence == null) println("NULL mention: (%d, %d) -> %s".format(start, length, m.string))
        m.attr += new ACEMentionIdentifiers {
          def mId = getAttr(mention, "ID")
          def mType = getAttr(mention, "TYPE")
          def ldcType = getAttr(mention, "LDCTYPE")
          def offsetStart = getAttr(mention \ "extent" \ "charseq", "START").toInt
          def offsetEnd = getAttr(mention \ "extent" \ "charseq", "END").toInt
        }
        m.attr += new EntityRef(m, e)

        val headCharIndex = getAttr(mention \ "head" \ "charseq", "END").toInt //- 1 // is the -1 necessary?
        val headLeftCharIndex = getAttr(mention \ "head" \ "charseq", "START").toInt
        try {
          val tokIndLeft = tokenIndexAtCharIndex(headLeftCharIndex, doc)
          // set head token to the rightmost token of the ACE head
          val tokIndRight = tokenIndexAtCharIndex(headCharIndex, doc)
          m._head = doc.asSection(tokIndRight)
          //m.attr += new ACEFullHead(new TokenSpan(doc.asSection, tokIndLeft, tokIndRight - tokIndLeft + 1)(null))
        } catch {
          case e: Exception =>
            println("doc: " + doc.tokens.mkString("\n"))
            println("mention: " + mention)
            println("headIndex: " + headCharIndex)
            println("headLeftIndex: " + headLeftCharIndex)
            e.printStackTrace()
            System.exit(1)
        }
      }
    }
  }

  private def lookupEntityMention(id: String, doc: Document): PairwiseMention =
    doc.attr[ACEMentionSpanList].find {
      s =>
        val a = s.attr[ACEMentionIdentifiers]
        a != null && a.mId == id
    }.get.asInstanceOf[PairwiseMention]

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
        val m = new RelationMention(args.head, args.last, identifiers.rType, Some(identifiers.rSubtype))
        if (m.arg1.sentence != m.arg2.sentence) println("sentence doesn't match")
        m.attr += identifiers
        doc.attr[RelationMentions].add(m)(null)
        args.foreach(_.attr.getOrElseUpdate(new RelationMentions).add(m)(null))
      }
    }
  }

  // drops the first two lines (xml decl, and dtd)
  private def loadXML(apfFile: String): NodeSeq = {
    val source = io.Source.fromFile(apfFile)
    val result = XML.loadString(source.getLines().drop(2).mkString("\n"))
    source.close()
    result
  }

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
    println("docs: " + docs.size)
    for (d <- docs)
      d.attr[ACEMentionSpanList].foreach(s => println(s))
  }

}
