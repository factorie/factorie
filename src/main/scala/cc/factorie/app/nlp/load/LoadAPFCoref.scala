package cc.factorie.app.nlp.load

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.WithinDocCoref
import java.io.{FileInputStream, File}
import cc.factorie.util.NonValidatingXML
import scala.Some
import cc.factorie.app.nlp.phrase.Phrase
import scala.io.Source
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.pos.PennPosTag
import scala.util.matching.Regex

/**
 * Takes a document and an apf.xml file that contains coreference annotation for that
 * document and annotates that document.
 *
 * If the document already has a target coref, this will overwrite it!
 *
 * @author John Sullivan
 */
class LoadAPFCoref(apfFile:File) extends DocumentAnnotator{

  // the APF XML has newlines with indents within mention strings that occur across newlines, this is a hack to remove them and replace them with a newline
  private val trimRegex = """\n\s+""".r
  private def fixMentionString(str:String):String = trimRegex.replaceAllIn(str, "\n")

  def tokenAnnotationString(token: Token) = ""

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])

  //todo do we want to add NER Types while we're at it?
  def process(document: Document) = {
    val targetCoref = document.getTargetCoref
    val offset = new OffsetMapper(document.string)

    (NonValidatingXML.load(new FileInputStream(apfFile)) \\ "entity").foreach { entNode =>

      val ent = targetCoref.entityFromUniqueId(entNode.attribute("ID").get.text)
      entNode \ "entity_attributes" \ "name" match {
        case name if name.nonEmpty => name.head.attribute("NAME").foreach(nameNode => ent.canonicalName = fixMentionString(nameNode.text))
        case _ => ()
      }
      (entNode \ "entity_mention").foreach { mentNode =>
        val charSeq = (mentNode \ "extent" \ "charseq").head

        val (mentStart, mentEnd) = charSeq.attribute("START").get.text.toInt -> (charSeq.attribute("END").get.text.toInt + 1)//these offsets include the xml/sgml of the original file
        val mentStartAdj = offset.fixOffset(mentStart)
        val mentEndAdj = mentStartAdj + (mentEnd - mentStart)

        document.getSectionByOffsets(mentStartAdj, mentEndAdj) match {
          case Some(sec) =>
            sec.tokens.dropWhile(_.stringEnd <= mentStartAdj).takeWhile(_.stringStart <= mentEndAdj) match {
              case toks if toks.size != 0 =>
                val ment = targetCoref.addMention(new Phrase(
                  new TokenSpan(toks), 0), ent) // todo use head annotation to find phrase head
                ment.attr += APFMentionId(mentNode.attribute("ID").get.text)
              case _ => () // todo should we do something?

            }
          case None => //todo do something
        }
      }
    }

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


case class APFMentionId(value:String)


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
        println("\tMention: %s with offsets: %s and id: %s".format(ment.phrase.string, ment.phrase.characterOffsets, ment.attr[APFMentionId].value))
      }
    }

  }
}