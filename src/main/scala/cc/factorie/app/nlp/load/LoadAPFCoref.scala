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
class LoadAPFCoref(apfFile:File, loadAsTarget:Boolean = true) extends DocumentAnnotator{

  // the APF XML has newlines with indents within mention strings that occur across newlines, this is a hack to remove them and replace them with a newline
  private val trimRegex = """\n\s+""".r
  private def fixMentionString(str:String):String = trimRegex.replaceAllIn(str, "\n")
  private def offsetsFromCharSeq(charSeq:Node)(implicit offset:OffsetMapper):(Int, Int) = {
    val (start, end) = (charSeq \ "@START").text.toInt -> ((charSeq \ "@END").text.toInt + 1)//these offsets include the xml/sgml of the original file
    val startAdj = offset.fixOffset(start)
    val endAdj = startAdj + (end - start)
    startAdj -> endAdj
  }

  def tokenAnnotationString(token: Token) = ""

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])

  //todo do we want to add NER Types while we're at it?
  def process(document: Document) = {
    val coref = if(loadAsTarget) document.getTargetCoref else document.getCoref
    implicit val offset = new OffsetMapper(document.string)

    val allMentions = (NonValidatingXML.load(new FileInputStream(apfFile)) \\ "entity").flatMap{ entNode =>
      val ent = (entNode \ "@ID").text -> (entNode \ "entity_attributes" \ "name" match {
        case name if name.nonEmpty => name.head.attribute("NAME").map(a => fixMentionString(a.head.text))
        case _ => None
      })
      val ments = (entNode \ "entity_mention") flatMap  { mentNode =>

        val (mentStart, mentEnd) = offsetsFromCharSeq((mentNode \ "extent" \ "charseq").head)
        val (headStart, headEnd) = offsetsFromCharSeq((mentNode \ "head" \ "charseq").head)


        document.getSectionByOffsets(mentStart, mentEnd) flatMap { sec =>
          sec.tokens.dropWhile(_.stringEnd <= mentStart).takeWhile(_.stringStart <= mentEnd) match {
            case toks if toks.size != 0 =>
              //token seq and head token index
              Some((mentStart -> mentEnd, toks, toks.dropWhile(_.stringEnd <= headStart).takeWhile(_.stringStart <= headEnd).headOption.map( t => t.position - toks.head.position).getOrElse(0)))
            case _ => None
          }
        }
      }
      ments.map(_ -> ent)
    }.sortBy{case (((start, end), _, _), _) => (end - start) * -1} // this puts the longest mentions first

    allMentions foreach { case((_, mentToks, mentHead), (entId, entName)) =>
      val ent = coref.entityFromUniqueId(entId)
      if(ent.canonicalName != null && entName.isDefined) {
        ent.canonicalName = entName.get
      }
      val mentSpan = new TokenSpan(mentToks)
      coref.findOverlapping(mentSpan) match {
        case Some(existingMention) => ent += existingMention
        case None => coref.addMention(new Phrase(mentSpan, mentHead), ent)
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