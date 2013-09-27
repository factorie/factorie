package cc.factorie.app.nlp.load
import cc.factorie.app.nlp._


import scala.io.Source
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.variable._
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.UnknownDocumentAnnotator
import cc.factorie.app.nlp.pos.PennPosLabel

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

    val doc = new Document()
    doc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[PennPosLabel]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[BIOChunkTag]) = UnknownDocumentAnnotator.getClass

    var sent = new Sentence(doc)
    source.getLines().foreach{ line =>
      sent = processWordLine(doc, sent, line)
    }
    Seq(doc)
  }

  val lineSplit = """([^\s]+) ([^\s]+) ([^\s]+)""".r
  val posTranslations = Map("(" -> "-LRB-", ")" -> "-RRB-")
  private def processWordLine(doc:Document, sent:Sentence, line:String):Sentence = line match {
    case lineSplit(tokenType, posTagString, chunkTagString) => {
      val t = new Token(sent, tokenType + " ")
      t.attr += new PennPosLabel(t, posTranslations.getOrElse(posTagString, identity(posTagString)))
      t.attr += new BIOChunkTag(t, chunkTagString)
      sent
    }
    case empty if empty.isEmpty => new Sentence(doc)
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
    "I-LST",
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