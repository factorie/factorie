package cc.factorie.app.nlp

import scala.io.Source
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.{LabeledCategoricalVariable, CategoricalDomain}
import scala.collection.mutable

/**
 * @author John Sullivan
 *
 * Loads shallow parsing/chunking data from Conll 2000 shared task
 * Each sentence becomes a document
 *
 * 1 token type
 * 2 gold POS Tag
 * 3 gold chunk (BIO notation)
 */
object LoadConll2000 extends Load {
  def fromSource(source: Source): Seq[Document] = {
    var docs = new mutable.ArrayBuffer[Document]()
    var doc = new Document()
    source.getLines().foreach{ line =>
      val resDoc = processWordLine(doc, line)
      if(resDoc != doc) {
        docs += doc
        doc = resDoc
      }
    }
    docs
  }

  val LineSplit = """([^\s]+) ([^\s]+) ([^\s]+)""".r
  val posTranslations = Map("(" -> "-LRB-", ")" -> "-RRB-")
  private def processWordLine(doc:Document, line:String):Document = line match {
    case LineSplit(tokenType, posTagString, chunkTagString) => {
      println(s"$line split into $tokenType|$posTagString|$chunkTagString")
      val t = new Token(doc, tokenType + " ")
      t.attr += new PTBPosLabel(t, posTranslations.getOrElse(posTagString, identity(posTagString)))
      t.attr += new BIOChunkTag(t, chunkTagString)
      doc
    }
    case empty if empty.isEmpty => new Document()
    case otw => throw new Exception("Expected either a line with token pos tag chunk tag, or an empty line, received: %s".format(otw))
  }
}

object BIOChunkDomain extends CategoricalDomain[String] {
  this ++= Vector("B-ADJP",
    "B-ADVP",
    "B-CONJP",
    "B-INTJ",
    "B-LST",
    "B-NP",
    "B-PP",
    "B-PRT",
    "B-SBAR",
    "B-UCP",
    "B-VP",
    "I-ADJP",
    "I-ADVP",
    "I-CONJP",
    "I-INTJ",
    "I-NP",
    "I-PP",
    "I-PRT",
    "I-SBAR",
    "I-UCP",
    "I-VP",
    "O")
  freeze()
}

class BIOChunkTag(val token:Token, tagValue:String) extends LabeledCategoricalVariable(tagValue) {
  def domain = BIOChunkDomain
}