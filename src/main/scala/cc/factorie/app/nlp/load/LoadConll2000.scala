package cc.factorie.app.nlp.load
import cc.factorie.app.nlp._


import scala.io.Source
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.variable._
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.UnknownDocumentAnnotator
import cc.factorie.app.nlp.pos.PennPosTag

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
    doc.annotators(classOf[PennPosTag]) = UnknownDocumentAnnotator.getClass
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
      t.attr += new PennPosTag(t, posTranslations.getOrElse(posTagString, identity(posTagString)))
      t.attr += new BIOChunkTag(t, chunkTagString)
      sent
    }
    case empty if empty.isEmpty => new Sentence(doc)
    case otw => throw new Exception("Expected either a line with token pos tag chunk tag, or an empty line, received: %s".format(otw))
  }

  def convertBIOtoBILOU(sentences: Seq[Sentence]){
    for(sentence <- sentences) {
      for(token <- sentence.tokens) {
        var prev : Token = null
        var next : Token = null
        if(token.sentenceHasPrev) prev = token.sentencePrev
        if(token.sentenceHasNext) next = token.sentenceNext
        token.sentenceNext
        val newLabel : String = BIOtoBILOU(prev, token, next)
        token.attr += new BILOUChunkTag(token, newLabel)
      }
    }
  }

  def BIOtoBILOU(prev : Token, token : Token,  next : Token) : String = {
    if(token.attr[BIOChunkTag].categoryValue == "O") return "O"
    val ts = token.attr[BIOChunkTag].categoryValue.split("-")
    var ps : Array[String] = null
    var ns : Array[String] = null
    if(next != null)
      ns = splitLabel(next)
    if(prev != null)
      ps = splitLabel(prev)

    if(token.attr[BIOChunkTag].categoryValue.contains("B-")) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      else
        return token.attr[BIOChunkTag].categoryValue
    }

    if(next == null || ns(1) != ts(1) || ns(0) == "B")
      return "L-" + ts(1)
    "I-" + ts(1)

  }

  private def splitLabel(token : Token) : Array[String] = {
    if(token.attr[BIOChunkTag].categoryValue.contains("-"))
      token.attr[BIOChunkTag].categoryValue.split("-")
    else
      Array("", "O")
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

object BILOUChunkDomain extends CategoricalDomain[String] {
  this ++= BIOChunkDomain.categories
  this ++= Vector( "L-ADVP",
    "L-ADJP",
    "L-CONJP",
    "L-INTJ",
    "L-LST",
    "L-NP",
    "L-PP",
    "L-PRT",
    "L-SBAR",
    "L-UCP",
    "L-VP",
    "U-ADJP",
    "U-ADVP",
    "U-CONJP",
    "U-INTJ",
    "U-LST",
    "U-NP",
    "U-PP",
    "U-PRT",
    "U-SBAR",
    "U-UCP",
    "U-VP")
  freeze()
}

class BILOUChunkTag(val token:Token, tagValue:String) extends LabeledCategoricalVariable(tagValue) {
  def domain = BILOUChunkDomain
}