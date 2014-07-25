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

    document.annotators += classOf[WithinDocCoref] -> classOf[LoadAPFCoref]
    document
  }
}

class OffsetMapper(f:File) {
  private val TagRegex = new Regex("""<[/\w\d "=]+>""")
  val src = Source.fromFile(f)
  private val rawText = src.mkString
  src.close()

  private var numXMLChars = 0
  private val newOffsets = TagRegex.findAllIn(rawText).matchData.map{ m =>
    numXMLChars += m.matched.length
    math.max(0, m.start - numXMLChars) -> numXMLChars
  }.toSeq


  def fixOffset(apfOffset:Int) = newOffsets.takeWhile(_._1 <= apfOffset).last._2 + apfOffset
}


case class APFMentionId(value:String)


object LoadAPFCoref {

  // the APF XML has newlines with indents within mention strings that occur across newlines, this is a hack to remove them and replace them with a newline
  val trimRegex = """\n\s+""".r
  def fixMentionString(str:String):String = trimRegex.replaceAllIn(str, "\n")

  def getOffsets(sgmFile:File):Seq[((Int, Int), String)] = NonValidatingXML.load(new FileInputStream(sgmFile)) \\ "entity_mention" map { mentNode =>
    val charSeq = (mentNode \ "extent" \ "charseq").head
    val mentSpan = charSeq.attribute("START").get.text.toInt -> (charSeq.attribute("END").get.text.toInt + 1)
    val mentString = fixMentionString(charSeq.text)
    assert(mentSpan._2 - mentSpan._1 == mentString.length, "lengths don't match in annotation! span %s, strlen %s for string %s".format(mentSpan._2 - mentSpan._1, mentString.length, mentString))

    mentSpan -> mentString
  }


  val TagRegex = new Regex("""<[/\w\d "=]+>""")
  def main(args:Array[String]) {
    val apfFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.apf.xml")
    val sgmFile = new File("/Users/johnsullivan/data/ace08_eval_sample/CNN889-3.940928.LDC98T25.sgm")

    val apfOffsets = getOffsets(apfFile).sortBy(_._1._1)

    //println("generated offsets: %s".format(apfOffsets))

    val mapper = new OffsetMapper(sgmFile)


    val adjustedOffsets = apfOffsets.map {case ((start, end), text) =>
      val len = end - start
      val adjStart = mapper.fixOffsetNew(start)
      (adjStart, adjStart + len)
    }
    val docString = Source.fromFile(sgmFile).getLines().mkString("\n")

    //println("loaded in document of length: %s".format(docString.length))
    var ctr = 0
    var totalCtr = 0
    apfOffsets zip adjustedOffsets foreach { case (((origStart, origEnd), text), (adjStart, adjEnd)) =>
      val adjString = docString.substring(adjStart, adjEnd)
      totalCtr += 1
      if(adjString != text) {
        ctr += 1
        println("Raw offsets at %s yield |%s|, adjusted at %s yield |%s|(%s), should be |%s|(%s)".format(origStart -> origEnd, docString.substring(origStart, origEnd), adjStart -> adjEnd, adjString, adjString.length, text, text.length))
      }
      assert(adjString.length == text.length, "lengths don't match, labeled: %s adjusted: %s".format(text.length, adjString.length))
    }
    println("%s in error in total, out of %s".format(ctr, totalCtr))

    //val doc = new Document(Source.fromFile(sgmFile).getLines().mkString("\n"))


    /*
    (DeterministicTokenizer.process _ andThen DeterministicSentenceSegmenter.process)(doc)
    println("tokenized doc")
    val corefAnnotator = new LoadAPFCoref(apfFile)
    println("built anno")
    */
    /*
    val secs = doc.sections

    println("secs: %s".format(secs))

    secs.zipWithIndex.map{ case(sec, idx) =>
      println("sec %d range: %s doc range: %s".format(idx, sec.stringStart -> sec.stringEnd, doc.stringStart -> doc.stringEnd))

    }

    println("found section at %s : %s".format(100 -> 1000, doc.getSectionByOffsets(100, 1000)))
    */
    /*
    corefAnnotator.process(doc)
    println("annotated with coref")

    doc.targetCoref.entities.foreach { ent =>
      println("Entity: %s".format(ent.canonicalName -> ent.uniqueId))
      ent.mentions.foreach{ ment =>
        println("\tMention: %s with offsets: %s and id: %s".format(ment.phrase.string, ment.phrase.characterOffsets, ment.attr[APFMentionId].value))
      }
    }
    */
  }
}