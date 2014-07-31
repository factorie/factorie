package cc.factorie.app.nlp.load

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.WithinDocCoref
import java.io.{BufferedInputStream, InputStream, FileInputStream, File}
import cc.factorie.util.NonValidatingXML
import scala.Some
import cc.factorie.app.nlp.phrase.Phrase
import scala.io.Source
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.pos.PennPosTag
import scala.util.matching.Regex
import scala.xml.Node

/**
 * Takes a document and an apf.xml file that contains coreference annotation for that
 * document and annotates that document.
 *
 * If the document already has a (target) coref, this will overwrite mentions that
 * overlap with the mentions annotated here.
 *
 * @author John Sullivan
 */
class LoadAPFCoref(mentions:Seq[SerializableAPFMention], loadAsTarget:Boolean) extends DocumentAnnotator {

  def this(apfFile:File, loadAsTarget:Boolean = true) = this(SerializableAPFMention.fromAPFXML(NonValidatingXML loadFile apfFile), loadAsTarget)

  def tokenAnnotationString(token: Token) = null

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])

  def fixOffsets(span:(Int, Int))(implicit offset:OffsetMapper):(Int, Int) = {
    val (start, end) = span
    val startAdj = offset.fixOffset(start)
    val endAdj = startAdj + (end - start)
    startAdj -> endAdj
  }

  //todo do we want to add NER Types while we're at it?
  def process(document: Document) = {
    val coref = if(loadAsTarget) document.getTargetCoref else document.getCoref
    implicit val offset = new OffsetMapper(document.string)

    mentions.sortBy{case SerializableAPFMention(_, _, _, _, (start, end), _) => (end - start) * -1} // sorting by length here means only the longest of overlapping mentions will be loaded later.
      .foreach { case SerializableAPFMention(_, entId, entName, mentId, mentSpan, mentHeadSpan) =>
      val ent = coref.entityFromUniqueId(entId)
      if(ent.canonicalName != null && entName.isDefined) {
        ent.canonicalName = entName.get
      }
      val (mentStart, mentEnd) = fixOffsets(mentSpan)
      val (mentHeadStart, mentHeadEnd) = fixOffsets(mentHeadSpan)

      document.getSectionByOffsets(mentStart, mentEnd).foreach { sec =>
        sec.tokens.dropWhile(_.stringEnd <= mentStart).takeWhile(_.stringStart <= mentEnd) match {
          case toks if toks.size != 0 =>
            val headIndex = toks.dropWhile(_.stringEnd <= mentHeadStart).takeWhile(_.stringStart <= mentHeadEnd).headOption.map( t => t.position - toks.head.position).getOrElse(0)
            val tokSpan = new TokenSpan(toks)
            coref.findOverlapping(tokSpan) match {
              case Some(existingMention) => ent += existingMention
              case None => coref.addMention(new Phrase(tokSpan, headIndex), ent)
            }
          case _ => ()
        }
      }
    }
    coref.trimEmptyEntities()
    document.annotators += classOf[WithinDocCoref] -> classOf[LoadAPFCoref]
    document
  }
}

class OffsetMapper(private val rawText:String) {
  def this(f:File) = this(Source.fromFile(f).mkString("\n"))

  private val TagRegex = new Regex("""<[/\w\d "=]+>""")

  private var numXMLChars = 0
  private val offsets = TagRegex.findAllIn(rawText).matchData.map{ m =>
    numXMLChars += m.matched.length
    math.max(0, m.start - numXMLChars) -> numXMLChars
  }.toSeq


  def fixOffset(apfOffset:Int) = offsets.takeWhile(_._1 <= apfOffset ).last._2 + apfOffset
}

case class SerializableAPFMention(docId:String, entId:String, entName:Option[String], mentId:String, mentSpan:(Int, Int), mentHeadSpan:(Int, Int)) {
  def serialize:String = Seq(docId, entId, entName.getOrElse(""), mentId, "%s,%s".format(mentSpan._1, mentSpan._2), "%s,%s".format(mentHeadSpan._1,mentHeadSpan._2)).mkString("\t")
}

object SerializableAPFMention {
  def deserialize(str:String):Option[SerializableAPFMention] = str.split("\t") match {
    case Array(docId, entId, entNameStr, mentId, mentSpanStr, mentHeadSpanStr) =>
      val entName = if(entNameStr.isEmpty) None else Some(entNameStr)
      val Array(mentStart, mentEnd) = mentSpanStr.split(",")
      val Array(mentHeadStart, mentHeadEnd) = mentHeadSpanStr.split(",")
      val mentSpan = mentStart.toInt -> mentEnd.toInt
      val mentHeadSpan = mentHeadStart.toInt -> mentHeadEnd.toInt
      Some(SerializableAPFMention(docId, entId, entName, mentId, mentSpan, mentHeadSpan))
    case _ => None
  }

  private val trimRegex = """\n\s+""".r
  private def fixMentionString(str:String):String = trimRegex.replaceAllIn(str, "\n")
  private def offsetsFromCharSeq(charSeq:Node):(Int, Int) = (charSeq \ "@START").text.toInt -> ((charSeq \ "@END").text.toInt + 1)//these offsets include the xml/sgml of the original file

  def fromAPFXML(xml:Node):Seq[SerializableAPFMention] = {
    val docId = (xml \\ "document" \ "@DOCID").text
    val mentions = (xml \\ "entity").flatMap{ entNode =>
      val (entId, entName) = (entNode \ "@ID").text -> (entNode \ "entity_attributes" \ "name" match {
        case name if name.nonEmpty => name.head.attribute("NAME").map(a => fixMentionString(a.head.text))
        case _ => None
      })
      (entNode \ "entity_mention").map{ mentNode =>
        val mentId = (mentNode \ "@ID").text
        val mentSpan = offsetsFromCharSeq((mentNode \ "extent" \ "charseq").head) // we actually don't need to/can't fix these here
      val mentHeadSpan = offsetsFromCharSeq((mentNode \ "head" \ "charseq").head)
        SerializableAPFMention(docId, entId, entName, mentId, mentSpan, mentHeadSpan)
      }
    }
    mentions
  }
}

object LoadAPFCoref {

  val TagRegex = new Regex("""<[/\w\d "=]+>""")
  def main(args:Array[String]) {
    val apfFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.apf.xml")
    val sgmFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.sgm")


    val doc = new Document(Source.fromFile(sgmFile).getLines().mkString("\n"))



    (DeterministicTokenizer.process _ andThen DeterministicSentenceSegmenter.process)(doc)
    println("tokenized doc")
    val corefAnnotator = new LoadAPFCoref(apfFile)
    println("built anno")

    corefAnnotator.process(doc)
    println("annotated with coref")

    doc.targetCoref.entities.foreach { ent =>
      println("Entity: %s".format(ent.canonicalName -> ent.uniqueId))
      ent.mentions.foreach{ ment =>
        println("\tMention: %s with offsets: %s ".format(ment.phrase.string, ment.phrase.characterOffsets))
      }
    }

  }
}