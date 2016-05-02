/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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

import java.io._
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.WithinDocCoref
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.segment.{DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter}
import cc.factorie.util.{DefaultCmdOptions, NonValidatingXML}

import scala.io.Source
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

  def this(apfFile:File, loadAsTarget:Boolean = true) = this({
    val src = new BufferedInputStream(new FileInputStream(apfFile))
    val offsets = SerializableAPFMention.fromAPFXML(NonValidatingXML load src)
    src.close()
    offsets
  }, loadAsTarget)

  def tokenAnnotationString(token: Token) = null

  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])

  def fixOffsets(span:(Int, Int))(implicit offset:OffsetMapper):(Int, Int) = {
    val (start, end) = span
    val startAdj = offset.fixOffset(start)
    val endAdj = startAdj + (end - start)
    startAdj -> endAdj
  }

  def processFromOffsets(offset:OffsetMapper, document:Document):Document = {
    val coref = if(loadAsTarget) document.getTargetCoref else document.getCoref
    mentions.sortBy{case SerializableAPFMention(_, _, _, _, (start, end), _) => (end - start) * -1} // sorting by length here means only the longest of overlapping mentions will be loaded later.
      .foreach { case SerializableAPFMention(_, entId, entName, mentId, mentSpan, mentHeadSpan) =>
      val ent = coref.entityFromUniqueId(entId)
      if(ent.canonicalName != null && entName.isDefined) {
        ent.canonicalName = entName.get
      }
      val (mentStart, mentEnd) = fixOffsets(mentSpan)(offset)
      val (mentHeadStart, mentHeadEnd) = fixOffsets(mentHeadSpan)(offset)

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
    document
  }

  //todo do we want to add NER Types while we're at it?
  def process(document: Document) = {

    implicit val offset = new OffsetMapper(document.string)
    // side effects!
    processFromOffsets(offset, document)

    document.annotators += classOf[WithinDocCoref] -> classOf[LoadAPFCoref]
    document
  }
}

class OffsetMapper(val offsets:Seq[(Int, Int)]) {
  def this(rawText:String) = this{
    var numXMLChars = 0
    new Regex("""<[/\w\d "=]+>""").findAllIn(rawText).matchData.map{ m =>
      numXMLChars += m.matched.length
      math.max(0, m.start - numXMLChars) -> numXMLChars
    }.toSeq
  }

  def this(f:File) = this{
    val src = Source.fromFile(f)
    val docString = src.mkString("\n")
    src.close()
    docString
  }



  def fixOffset(apfOffset:Int) = offsets.takeWhile(_._1 <= apfOffset ).lastOption.getOrElse(0 -> 0)._2 + apfOffset

  def serialize:String = offsets.map{case (i, j) => i + "?" + j}.mkString(",")
}

object OffsetMapper {
  def deserialize(str:String):OffsetMapper = new OffsetMapper(str.split(",").map{ s =>
    val Array(i, j) = s.split('?')
    i.toInt -> j.toInt
  }.toSeq)

  def buildMapperLine(docId:String, docString:String):String = docId + "\t" + new OffsetMapper(docString).serialize



  def tacDocumentSplitter(tacDocFile:File):Iterator[String] = new Iterator[String] {

    private val docEndString = """</doc>"""
    private val webDocStartString = """<DOC>"""
    private val docIdRegex = """(?i)<DOC ID="([^"]+)"[^>]*>""".r
    private val webDocIdRegex = """(?i)<DOCID> ([^ ])+ </DOCID>""".r

    private val tacReader = if(tacDocFile.getName.endsWith(".gz")) {
      new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(tacDocFile))))
    } else {
      new BufferedReader(new InputStreamReader(new FileInputStream(tacDocFile)))
    }

    //priming the pump
    private var line = tacReader.readLine()
    private var lineNum = 1

    // grouping together to avoid forgetting something
    @inline
    private def advanceLine(docBuffer:StringBuilder) {
      docBuffer append line
      docBuffer append "\n"
      line = tacReader.readLine()
      lineNum += 1
    }


    def next() = {
      val docBuffer = new StringBuilder()


      var docIdMatchOpt = docIdRegex.findFirstMatchIn(line)

      // We should be at the start of a new document here, otherwise we have a problem.
      assert(line.equalsIgnoreCase(webDocStartString) || docIdMatchOpt.isDefined, "Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, tacDocFile.getName))

      val docId = if(docIdMatchOpt.isDefined) {
        docIdMatchOpt.get.toString()
      } else if(line equalsIgnoreCase webDocStartString) { // we know that one must be true but let's not tempt fate
        advanceLine(docBuffer)
        webDocIdRegex.findFirstMatchIn(line).get.toString()
      } else {
        throw new Exception("Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, tacDocFile.getName))
      }

      while(!line.equalsIgnoreCase(docEndString)) {
        advanceLine(docBuffer)
      }
      // the loop exits when the doc end is found, but that us still part of the previous document so we need to consume it.
      advanceLine(docBuffer)
      docBuffer.toString()
    }

    def hasNext = line != null
  }

  def splitByLine(docOffsetFile:String, tacRoot:String, outputFile:String) {

    val docEndString = """</doc>"""
    val webDocStartString = """<DOC>"""
    val docIdRegex = """(?i)<DOC ID="([^"]+)"[^>]*>""".r
    val webDocIdRegex = """(?i)<DOCID> ([^ ])+ </DOCID>""".r
    val wrt = new BufferedWriter(new FileWriter(outputFile))
    var count = 0
    var lineCount = 0
    var docStringBuf = new StringBuilder()
    var prevLine = null.asInstanceOf[String]
    var line = null.asInstanceOf[String]

    Source.fromFile(docOffsetFile).getLines().map {_.split('\t').apply(1)}.toSet.foreach{ filePath:String =>
      val tacFileReader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(tacRoot + "/" + filePath)), "UTF-8"))
      line = tacFileReader.readLine()
      lineCount += 1


      var docIdRegex(docId) = line
      while (line != null) {
        docStringBuf append line
        if(line equalsIgnoreCase docEndString) {
          // write out the doc offsets
          wrt write buildMapperLine(docId, docStringBuf.toString())
          wrt.newLine()

          // clear out the buffer
          docStringBuf = new StringBuilder()

          // report our results
          if(count % 1000 == 0) { // this number is a total guess
            println("Wrote offsets for %d files".format(count))
            wrt.flush()
          }
          count += 1
        }
        prevLine = line
        line = tacFileReader.readLine()
        if(line != null && prevLine != null && prevLine.equalsIgnoreCase(docEndString)) {
          var docIdRegex(docId) = line
        }
      }
    }

    wrt.flush()
    wrt.close()
    println("Wrote offsets for %d files".format(count))

  }

  def main(args:Array[String]) {
    val opts = new OffsetMapperOpts
    opts.parse(args)

    splitByLine(opts.docOffsetFile.value, opts.tacRoot.value, opts.outputFile.value)

  }

}

class OffsetMapperOpts extends DefaultCmdOptions {
  val docOffsetFile = new CmdOption("doc-offset", "", "FILE", "A file containing the offsets of documents into the raw tac document.")
  val tacRoot = new CmdOption("tac-root", "", "DIRECTORY", "The root directory in which tac data is stored.")
  val outputFile = new CmdOption("output-file", "", "FILE", "The file into which to write the resulting offsets.")
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



    (DeterministicNormalizingTokenizer.process _ andThen DeterministicSentenceSegmenter.process)(doc)
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
