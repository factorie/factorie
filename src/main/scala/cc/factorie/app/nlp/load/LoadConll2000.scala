package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.{Document,Sentence,Token,UnknownDocumentAnnotator}
import scala.io.Source
import cc.factorie.variable._
import cc.factorie.app.nlp.pos.PennPosTag
import scala.Predef._

/**
 * @author John Sullivan
 *
 * Loads shallow parsing/chunking data from Conll 2000 shared task
 * Each sentence becomes a document
 *
 * 1 token type
 * 2 gold POS Tag
 * 3 gold chunk (BIO notation default)
 */

object LoadConll2000 extends Load {
  //Default BIO encoding for loadConll2000 from Source since this is the standard encoding for conll2000 training data
  def fromSource(source: Source) = fromSource(source,"BIO")
  def fromSource(source: Source,encoding:String): Seq[Document] = {
    val doc = new Document()
    doc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[PennPosTag]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[BIOChunkTag]) = UnknownDocumentAnnotator.getClass

    //Enable multiple input encodings
    val newChunkLabel = encoding match {
      case "BILOU" => (t:Token,s:String) => new BILOUChunkTag(t,s)
      case "BIO" => (t:Token,s:String) => new BIOChunkTag(t,s)
      case "NESTED" => (t:Token,s:String) => new BILOUNestedChunkTag(t,s)
      case _ => (t:Token,s:String) => new BIOChunkTag(t,s)
    }
    var sent = new Sentence(doc)
    source.getLines().foreach{ line =>
      sent = processWordLine(doc, sent, line, newChunkLabel)
    }
    Seq(doc)
  }

  val lineSplit = """([^\s]+) ([^\s]+) ([^\s]+)""".r
  val posTranslations = Map("(" -> "-LRB-", ")" -> "-RRB-")
  private def processWordLine(doc:Document, sent:Sentence, line:String,newChunkLabel: (Token,String) => ChunkTag):Sentence = line match {
    case lineSplit(tokenType, posTagString, chunkTagString) => {
      val t = new Token(sent, tokenType + " ")
      t.attr += new PennPosTag(t, posTranslations.getOrElse(posTagString, identity(posTagString)))
      t.attr += newChunkLabel(t, chunkTagString)
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

//Standard conll2000 Chunk Tags
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

//For Noun Phrase Chunk Tagging
//Requires custom training data tagged in this notation
object BILOUNestedChunkDomain extends CategoricalDomain[String] {
  this ++= Vector( "B-NP:B-NP",
    "B-NP:I-NP",
    "B-NP:L-NP",
    "B-NP:U-NP",
    "B-NP:O",
    "I-NP:B-NP",
    "I-NP:I-NP",
    "I-NP:L-NP",
    "I-NP:U-NP",
    "I-NP:O",
    "L-NP:B-NP",
    "L-NP:I-NP",
    "L-NP:L-NP",
    "L-NP:U-NP",
    "L-NP:O",
    "U-NP:B-NP",
    "U-NP:I-NP",
    "U-NP:L-NP",
    "U-NP:U-NP",
    "U-NP:O",
    "O:B-NP",
    "O:I-NP",
    "O:L-NP",
    "O:U-NP",
    "O:O"
    )
  freeze()
}

//This could be combined into a single LabeledCategoricalVariable with a settable domain
abstract class ChunkTag(val token:Token, tagValue:String) extends LabeledCategoricalVariable(tagValue)

class BIOChunkTag(token:Token, tagValue:String) extends ChunkTag(token, tagValue) {
  def domain = BIOChunkDomain
}

class BILOUChunkTag(token:Token, tagValue:String) extends ChunkTag(token,tagValue) {
  def domain = BILOUChunkDomain
}

class BILOUNestedChunkTag(token:Token, tagValue:String) extends ChunkTag(token,tagValue) {
  def domain = BILOUNestedChunkDomain
}