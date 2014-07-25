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

  val offset = new OffsetMapper(apfFile)

  def tokenAnnotationString(token: Token) = ???

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])

  //todo do we want to add NER Types while we're at it?
  def process(document: Document) = {
    val targetCoref = document.getTargetCoref

    (NonValidatingXML.load(new FileInputStream(apfFile)) \\ "entity").foreach { entNode =>
      //println("EntNode:\n%s".format(entNode))

      val ent = targetCoref.entityFromUniqueId(entNode.attribute("ID").get.text)
      entNode \ "entity_attributes" \ "name" match {
        case name if name.nonEmpty => name.head.attribute("NAME").foreach(nameNode => ent.canonicalName = nameNode.text)
        case _ => ()
      }
      (entNode \ "entity_mention").foreach { mentNode =>
        val charSeq = (mentNode \ "extent" \ "charseq").head

        //println("found mentNode: %s".format(charSeq))
        val (mentStart, mentEnd) = charSeq.attribute("START").get.text.toInt -> charSeq.attribute("END").get.text.toInt //these offsets include the xml/sgml of the original file
        //println ("mention with offset %s. labeled string: |%s| document slice: |%s|".format(mentStart -> mentEnd, charSeq.text, document.string.substring(mentStart, mentEnd +1)))
        val mentStartAdj = mentStart //offset.fixOffset(mentStart)
        val mentEndAdj = mentEnd  //offset.fixOffset(mentEnd)
        println("mention offset adjustment: %s. labeled string: |%s| document slice: |%s|".format(mentStartAdj -> mentEndAdj, charSeq.text, document.string.substring(mentStartAdj, mentEndAdj + 1)))

        document.getSectionByOffsets(mentStartAdj, mentEndAdj) match {
          case Some(sec) =>
            val toks = sec.tokens.dropWhile(_.stringEnd <= mentStartAdj).takeWhile(_.stringStart <= mentEndAdj)
            if(toks.size == 0) {
              println("empty tokenlist. start-end : %s ment xml is: %s\ntoks up to: %s\ntoks after: %s".format(mentStart -> mentEnd, charSeq, sec.tokens.dropWhile(_.stringEnd <= mentStart).take(5), sec.tokens.takeWhile(_.stringStart <= mentEnd).takeRight(5)))
            }
            val ment = targetCoref.addMention(new Phrase(
              new TokenSpan(
                sec.tokens.dropWhile(_.stringEnd <= mentStartAdj).takeWhile(_.stringStart <= mentEndAdj)), 0), ent) // todo use head annotation to find phrase head
            println("made mention: %s at offsets: %s. Original offsets were: %s".format(ment.phrase.string, ment.phrase.characterOffsets, mentStart -> mentEnd))
            ment.attr += APFMentionId(mentNode.attribute("ID").get.text)
          case None => //todo do something
        }
      }
    }
    document
  }
}

class OffsetMapper(f:File) {
  private val TagRegex = new Regex("""<[/\w\d "=]+>""")
  val src = Source.fromFile(f)
  private val rawText = src.mkString
  src.close()
  private val offsets = TagRegex.findAllIn(rawText).matchData.map{m => (m.end, m.end - m.start)}.toSeq

  def fixOffset(apfOffset:Int):Int = offsets.takeWhile(_._1 < apfOffset).map(_._2).sum + apfOffset
}


case class APFMentionId(value:String)

object LoadAPFCoref {

  val TagRegex = new Regex("""<[/\w\d "=]+>""")
  def main(args:Array[String]) {
    val apfFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.apf.xml")
    val sgmFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.sgm")

    val doc = new Document(TagRegex.replaceAllIn(Source.fromFile(sgmFile).getLines().mkString("\n"), ""))

    (DeterministicTokenizer.process _ andThen DeterministicSentenceSegmenter.process)(doc)
    println("tokenized doc")
    val corefAnnotator = new LoadAPFCoref(apfFile)
    println("built anno")
    /*
    val secs = doc.sections

    println("secs: %s".format(secs))

    secs.zipWithIndex.map{ case(sec, idx) =>
      println("sec %d range: %s doc range: %s".format(idx, sec.stringStart -> sec.stringEnd, doc.stringStart -> doc.stringEnd))

    }

    println("found section at %s : %s".format(100 -> 1000, doc.getSectionByOffsets(100, 1000)))
    */

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