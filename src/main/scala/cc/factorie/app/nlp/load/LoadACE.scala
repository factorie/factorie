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
package cc.factorie.app.nlp.load
import cc.factorie.app.nlp._


import segment._

import xml.{XML, NodeSeq}
import java.io.File
import cc.factorie.app.nlp.coref.Mention
import cc.factorie.app.nlp.phrase.Phrase

// TODO: consider moving this info into variables.
case class ACEEntityIdentifiers(eId: String, eType: String, eSubtype: String, eClass: String)

case class ACEMentionIdentifiers(mId: String, mType: String, ldcType: String, offsetStart: Int, offsetEnd: Int)

case class ACERelationIdentifiers(rId: String, rType: String, rSubtype: String)

case class ACEFileIdentifier(fileId: String)

object LoadACE {

  private val matchTag = "<[A-Za-z=_\"/ ]*>".r

  private def makeDoc(sgm: String): Document = {
    val source = io.Source.fromFile(sgm)
    val sgmString = source.mkString
    source.close()
    val doc = new Document(matchTag.replaceAllIn(sgmString, _ => "")).setName(sgm)
    doc.attr += new ACEFileIdentifier(sgm.dropRight(4) + ".apf.xml")
    DeterministicTokenizer.process(doc)
    DeterministicSentenceSegmenter.process(doc)

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
    val coref = doc.getCoref
    for (entity <- apf \\ "entity") {
      val entityKey = (entity \ "entity_attributes" \ "name" \ "charseq").text
      val e = coref.entityFromUniqueId(entityKey)
      e.attr += ACEEntityIdentifiers(eId = getAttr(entity, "ID"), eType = getAttr(entity, "TYPE"), eSubtype = getAttr(entity, "SUBTYPE"), eClass = getAttr(entity, "CLASS"))
      
      for (mention <- entity \ "entity_mention") {
        val (start, length) = getTokenIdxAndLength(mention, doc)
        val headCharIndex = getAttr(mention \ "head" \ "charseq", "END").toInt

        val headLeftCharIndex = getAttr(mention \ "head" \ "charseq", "START").toInt
        try {
          // set head token to the rightmost token of the ACE head
          val tokIndRight = tokenIndexAtCharIndex(headCharIndex, doc)
          val phrase = new Phrase(doc.asSection, start, length, tokIndRight)
          if (phrase.sentence == null) println("NULL mention: (%d, %d) -> %s".format(start, length, phrase.string))

          coref.addMention(phrase,e).attr += new ACEMentionIdentifiers(mId = getAttr(mention, "ID"), mType = getAttr(mention, "TYPE"), ldcType = getAttr(mention, "LDCTYPE"), offsetStart = getAttr(mention \ "extent" \ "charseq", "START").toInt, offsetEnd = getAttr(mention \ "extent" \ "charseq", "END").toInt)

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

  private def lookupEntityMention(id: String, doc: Document): Mention =
    doc.coref.mentions.find {
      m =>
        val a = m.attr[ACEMentionIdentifiers]
        a != null && a.mId == id
    }.get

//  def addRelationsFromApf(apf: NodeSeq, doc: Document): Unit = {
//    doc.getCoref
//    for (relation <- apf \\ "relation") {
//      val identifiers = new ACERelationIdentifiers(rId = getAttr(relation, "ID"), rType = getAttr(relation, "TYPE"), rSubtype = getAttr(relation, "SUBTYPE"))
//
//      for (mention <- relation \ "relation_mention") {
//        val args = mention \ "relation_mention_argument" map {
//          arg => lookupEntityMention(getAttr(arg, "REFID"), doc)
//        }
//        assert(args.size == 2)
//        val m = new RelationMention(args.head, args.last, identifiers.rType, Some(identifiers.rSubtype))
//        if (m.arg1.phrase.sentence != m.arg2.phrase.sentence) println("sentence doesn't match")
//        m.attr += identifiers
//        doc.attr[RelationMentions].add(m)(null)
//        args.foreach(_.attr.getOrElseUpdate(new RelationMentions).add(m)(null))
//      }
//    }
//  }

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
    //addRelationsFromApf(loadXML(apfFile), doc)
    doc
  }

  def fromDirectory(dir: String, takeOnly: Int = Int.MaxValue): Seq[Document] =
    new File(dir).listFiles().filter(_.getName.endsWith(".apf.xml")).take(takeOnly).map(f => fromApf(f.getAbsolutePath))

  def main(args: Array[String]): Unit = {
    val docs = fromDirectory(args(0))
    println("docs: " + docs.size)
    for (d <- docs)
      d.coref.mentions.foreach(s => println(s))
  }

}
