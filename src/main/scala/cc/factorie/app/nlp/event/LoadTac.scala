package cc.factorie.app.nlp.event

import cc.factorie.app.nlp.{Section, Document}
import scala.io.Source
import java.util.zip.GZIPInputStream
import java.io.{File, FileInputStream}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.xml.pull.{EvText, EvElemEnd, EvElemStart, XMLEventReader}
import scala.util.matching.Regex

/**
 * Created by beroth on 3/31/15.
 */

class Paragraph(val document:Document, val stringStart:Int, val stringEnd:Int) extends Section

case class FileId(service: String, id1: String, id2: String) {
  val dfPattern = "(bolt.*)".r
  val webPattern = "(eng.*)".r
  def asNormalizedId: String = "%s-%s-%s".format(service, id1, id2)
  def asId: String = service match{
    case dfPattern(_) => s"$service-${id1.take(3)}-${id1.drop(3)}-$id2"
    case webPattern(_) => s"$service-${id1.take(2)}-${id1.drop(2)}-$id2"
    case _ => s"${service}_$id1.$id2"
  }
  // for backwards compatibility
  def asNormalizedFilepath(root: String, extension: String): File = new File(asNormalizedFilepathStr(root, extension))
  def asNormalizedFilepathStr(root: String, extension: String): String = "%s/%s/%s/%s.%s".format(root, service, id1, asNormalizedId, extension)

  def asFilepath(root: String, extension: String): File = new File(asFilepathStr(root, extension))
  def asFilepathStr(root: String, extension: String): String = "%s/%s/%s/%s.%s".format(root, service, id1, asId, extension)
  def makeDirectory(root: String): Boolean = new File("%s/%s/%s".format(root, service, id1)).mkdirs()
}

object FileId {
  //  private val DocRegex = """([\w_]+)_(\d{8})_(.+)""".r
  private val DocRegex = """(\D+)[-_](\d+-?\d+)[-.](\d+)""".r

  // e.g. eng-NG-31-109501-8131900, bolt-eng-DF-170-181103-15978491
  //  private val DocRegex1 = """\s+-(\d+)-(\d{4})(\d{2})-(\d+)""".r

  // e.g. AFP_ENG_20091101.0001
  //  private val DocRegex2 = """\s+_(\d{4})(\d{2})(\d{2})\.(\d+)""".r

  def apply(file: File): FileId = {
    //    println(s"file.getName: ${file.getName}")
    //    println(s"file.getName: ${file.getName.split(".").mkString("|")}")
    val idString = file.getName.dropRight(4) //.replaceFirst("[.][^.]+$", "").replaceAll("""\.""","_")
    val DocRegex(source, id1, id2) = idString
    new FileId(source, id1.replaceFirst("-", ""), id2)
  }

  def apply(idString: String): FileId = {
    val processedIdString = idString.trim.replaceFirst("\\.\\D+", "") // get rid of possible file extension
    val DocRegex(source, id1, id2) = processedIdString
    new FileId(source, id1.replaceFirst("-", ""), id2)
  }
}

object LoadTac{

  val encoding = "utf-8"

  // global vars for xml parsing (pulled from last year)
  var offset = 0
  var start = 0
  var inside: Boolean = false

  // load from gzip if file extension matches, otherwise plain text, either way using encoding
  def tacSourceFromFilename(filename: String): Source = {
    if(filename.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new FileInputStream(filename)), encoding)
    else Source.fromFile(filename, encoding)
  }

  def fromFilename(filename: String, docIds: Seq[String] = Seq()): Seq[Document] = {

    // TODO make this more reliable?
    val docParser: (Document) => Document = if(filename.contains("bolt") || filename.contains("Event")) fromStringToBoltDoc _  else fromStringToSgmDoc(false) _

    val docs = new ArrayBuffer[Document]()
    val src = tacSourceFromFilename(filename)

    // want to match each doc in file non-greedily
    val docRegexes = {
      if(!docIds.isEmpty){
        // want to process only doc with given id
        docIds.map(id => {
          if(id.startsWith("eng")) ("""(?si)<doc>\n<docid> """ + FileId(id).asId + """ </docid>.*?</doc>\s*""").r
          else ("""(?si)<doc id="""" + FileId(id).asId + """".*?</doc>\s*""").r
        })
      }
      else Seq("""(?si)<doc.*?</doc>\s*""".r)
    }

    // for keeping track of the document's offset in the file
    var docCharOffset = 0
    var srcString = src.mkString
    var i = 0
    while((!srcString.trim.isEmpty && docIds.isEmpty) || (!docIds.isEmpty && docs.length != docIds.length)) {
      if(!docIds.isEmpty) println(s"looking for id ${docIds(i)}")
      val docMatch = docRegexes(i).findFirstMatchIn(srcString)
      if(docMatch.isDefined) {
        val docString = docMatch.get.matched
        val charsBefore = docMatch.get.start
        val doc = new Document(docString)
        //        doc.startOffset = if(docIds.isEmpty) docCharOffset else charsBefore
        docParser(doc)
        docs += doc
        docCharOffset += docString.length
        if(docIds.isEmpty) srcString = srcString.drop(docString.length) else i += 1
      }
      else throw new Error(s"Text remaining in file $filename not picked up in a document: ${srcString.take(50)}...")
    }
    src.close
    docs.toSeq
  }

  def fromStringToBoltDoc(doc: Document): Document = {
    val src = Source.fromString(doc.string)
    val er = new XMLEventReader(src)
    parse(er, doc)
    src.close
    doc
  }

  // TODO replace BadXmlReader with anything else
  def fromStringToSgmDoc(headline : Boolean)(doc: Document): Document = {
    val b = new BadXmlReader(doc.string)
    val id = if(b.emptyAttr) { (b \ "DOCID").head.innerText.trim } else { b.attributes("id") }
    println(s"Found sgm doc with id $id")
    val source = if(b.emptyAttr) { (b \ "DOCTYPE").head.attributes("SOURCE") } else { b.attributes("type") }
    source match {
      case "newswire" => newswireSgm(doc, b, headline=headline)
      case "usenet" => usenetSgm(doc, b, headline=headline)
      case "blog" => usenetSgm(doc, b, headline=headline)
      case "weblog" => usenetSgm(doc, b, headline=headline)
      case "broadcast news" => broadcastSgm(doc, b, headline=headline)
      case "broadcast conversation" => broadcastSgm(doc, b, headline=headline)
      case "story" => storySgm(doc, b, headline=headline)
      case "multi" => textSgm(doc, b, headline=headline)
      case "other" => textSgm(doc, b, headline=headline)
      case "advis" => textSgm(doc, b, headline=headline)
      case _ => Nil
    }
    doc.setName(id)
    doc
  }

  def newswireSgm(doc : Document, node : BadXmlReader, headline : Boolean = false) {
    if(headline && node.\("BODY").head.\("HEADLINE").nonEmpty) {
      val head = (node.\("BODY").head.\("HEADLINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    val ps = (node.\("BODY").head.\("TEXT").head \ "P")
    for(p <- ps) doc += new Paragraph(doc, p.startInd, p.endInd)
  }

  def storySgm(doc : Document, node : BadXmlReader, headline : Boolean = false) {
    if(headline && node.\("BODY").head.\("HEADLINE").nonEmpty) {
      val head = (node.\("BODY").head.\("HEADLINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    if(headline && node.\("BODY").head.\("DATELINE").nonEmpty) {
      val head = (node.\("BODY").head.\("DATELINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    val ps = (node.\("TEXT").head \ "P")
    for(p <- ps) doc += new Paragraph(doc, p.startInd, p.endInd)
  }

  def textSgm(doc : Document, node : BadXmlReader, headline : Boolean = false) {
    if(headline && node.\("BODY").head.\("HEADLINE").nonEmpty) {
      val head = (node.\("BODY").head.\("HEADLINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    val txt = (node.\("TEXT")).head
    doc += new Paragraph(doc, txt.startInd, txt.endInd)
  }

  def usenetSgm(doc : Document, node : BadXmlReader, headline : Boolean = false) {
    if(headline && node.\("BODY").head.\("HEADLINE").nonEmpty) {
      val head = (node.\("BODY").head.\("HEADLINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    val ps = (node.\("BODY").head.\("TEXT").head \ "POST")
    for(p <- ps) {
      /*if((p \ "POSTDATE").nonEmpty) {
        doc += new Paragraph(doc, (p \ "POSTDATE").head.afterTextStart, p.quotableEnd)
      } elseif if((p \ "POSTER").nonEmpty) {
        doc += new Paragraph(doc, (p \ "POSTER").head.afterTextStart, p.quotableEnd)
      }*/
      //if(p.quotableEndEnd != p.endInd) doc += new Paragraph(doc, p.quotableEndEnd, p.startSecondQuote)
      if(!p.hasQuotes) {
        doc += new Paragraph(doc, p.startPost, p.startSecondQuote)
      } else {
        var prev = null.asInstanceOf[(Int,Int)]
        for(quote <- p.quotes) {
          val start = if(prev == null) { p.startPost } else prev._2
          if(doc.string.substring(start, quote._1).trim.length > 1) doc += new Paragraph(doc, start, quote._1)
          prev = quote
        }
        if(doc.string.substring(prev._2, p.endInd).trim.length > 1) doc += new Paragraph(doc, prev._2, p.endInd)
      }
    }
  }

  def broadcastSgm(doc : Document, node : BadXmlReader, headline : Boolean = false) {
    if(headline && node.\("BODY").head.\("HEADLINE").nonEmpty) {
      val head = (node.\("BODY").head.\("HEADLINE").head)
      doc += new Paragraph(doc, head.startInd, head.endInd)
    }
    val ps = (node.\("BODY").head.\("TEXT").head \ "TURN")
    for(p <- ps) {
      val speaker = (p \ "SPEAKER").head
      doc += new Paragraph(doc, speaker.startInd, speaker.endInd)
      doc += new Paragraph(doc, speaker.afterTextStart, speaker.afterTextEnd)
    }
  }

  object BadXmlReaderRegexes{
    val tag = "<[A-Za-z ]*( [A-Za-z_]*=[A-Za-z\"&;\\-/\\[\\]\\.:0-9_@,\\(\\) ]*)*>".r
    val tagNameReg = "<[A-Za-z]*".r
    val attributesReg = "[A-Za-z_]*=\"[A-Za-z &;\\-/\\[\\]\\.:0-9_@,\\(\\) ]*\"".r
    val q = "<QUOTE PREVIOUSPOST=\"".r
    val q2 = "\">"
  }
  class BadXmlReader(var source : String) {
    def \(s : String) : ArrayBuffer[BNode] = head.nodes.head._2.head \ s
    def attributes(s : String) = head.nodes.head._2.head.attributes(s)
    def emptyAttr = head.nodes.head._2.head.attributes.isEmpty
    // TODO fix this, this is kind of dumb -- want ids to just be lowercase
    def $(s : String) : Option[ArrayBuffer[BNode]] = if(head.nodes.contains(s)) Some(head.nodes(s)) else head.nodes.get(s.toLowerCase)
    val head = new BNode("TOP", source, source, 0, source.length, source, source.length, null)
  }
  class BNode(val name : String, val innerText : String, val outerText : String, val startInd : Int, val endInd : Int, val source : String, val tend : Int, val parent : BNode) {

    val nodes : HashMap[String,ArrayBuffer[BNode]] = new HashMap[String,ArrayBuffer[BNode]]()
    val attributes : HashMap[String, String] = new HashMap[String, String]()
    val tags = BadXmlReaderRegexes.tag.findAllMatchIn(innerText)
    var lastEnd : Int = 0
    val qMatches = BadXmlReaderRegexes.q.findAllMatchIn(innerText) // TODO make one call to findAllMatchIn
    val firstQuote = BadXmlReaderRegexes.q.findFirstMatchIn(innerText).getOrElse(null)
    val secondQuote = if(qMatches.size > 1) { qMatches.next; qMatches.next } else null.asInstanceOf[Regex.Match]
    val endFirstQuote = if(firstQuote == null) endInd else innerText.indexOf(BadXmlReaderRegexes.q2, firstQuote.start) + startInd + 2
    val startSecondQuote = if(secondQuote == null) endInd else secondQuote.start + startInd

    def startPost = if(nodes.contains("POSTDATE")) nodes("POSTDATE").head.endInd else if(nodes.contains("POSTER")) nodes("POSTER").head.endInd else startInd

    val hasQuotes = innerText.indexOf("<QUOTE") > -1

    def quotableEnd : Int = if(firstQuote == null) endInd else firstQuote.start + startInd
    def quotableEndEnd : Int = endFirstQuote

    val quotes = BadXmlReaderRegexes.q.findAllMatchIn(innerText).map{ quote => (quote.start+startInd, innerText.indexOf(BadXmlReaderRegexes.q2,quote.start)+startInd+2) }

    def \(s : String) : ArrayBuffer[BNode] = if(nodes.contains(s)) nodes(s) else ArrayBuffer[BNode]()

    def afterTextStart : Int = this.tend
    def afterTextEnd : Int = this.parent.endInd

    def afterText : String = source.substring(afterTextStart, afterTextEnd)

    //  println(s"tags: ${tags.mkString(",")}")
    for(t <- tags) {
      //        println(s"tag: ${t.group(0)}")
      val start = t.start + startInd
      val end = t.end + startInd
      if(start >= lastEnd) {
        val s = source.substring(start, end)
        val attributes = BadXmlReaderRegexes.attributesReg.findAllIn(s)
        val tagName = BadXmlReaderRegexes.tagNameReg.findFirstIn(s).getOrElse("").substring(1)
        var endStart = 0
        var endEnd = 0
        var endS = ""
        if(s.endsWith("/>")) {
          endStart = end
          endEnd = end
          endS = tagName
        } else {
          val endRegex = ("</" + tagName + ">").r
          val endTag = endRegex.findFirstMatchIn(source.substring(end)).getOrElse(null)
          if(endTag!=null) {
            endStart = endTag.start + end
            endEnd = endTag.end + end
            endS = source.substring(endStart, endEnd)
          } else {
            endStart = end
            endEnd = end
            endS = tagName
          }
        }
        lastEnd = endEnd
        val node = new BNode(tagName, source.substring(end,endStart), source.substring(start,endEnd), end,endStart,source, endEnd, this)
        //          println(s"node w/ tag=${tagName}")

        for(a <- attributes) {
          val split = a.split("=")
          val key = split(0)
          val value = split(1).substring(1,split(1).length-1)
          node.attributes(key) = value
        }
        if(nodes.contains(tagName)) nodes(tagName) += node
        else nodes(tagName) = ArrayBuffer(node)
      }
    }
  }

  def parse(xml: XMLEventReader, d : Document) {
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, a, _) =>
            if(label.toLowerCase == "doc") {
              val idNode = a.get("id")
              if(idNode.isDefined && !idNode.get.isEmpty){
                val id =idNode.get.head.toString
                d.setName(id)
                println(s"Found bolt doc with id $id")
              }
              else throw new Error(s"Unable to set document id, no id value in SGML. Doc: ${d.name}")
            }
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            //println("End element: " + label)
            if(currNode.head == ("post"))

              addSectionDoc(d)
            loop(currNode.tail)
          case EvText(text) =>
            addSection(text, currNode, d)
            loop(currNode)
          case _ => loop(currNode)
        }
      }
    }
    start = 0
    offset = 0
    loop(List.empty)
  }

  def addSection(text : String, currNode : List[String], d : Document) {
    if(currNode.isEmpty) return
    if(d.string.indexOf(text, offset) == -1) {
      println(d.string + " cannot find: " + text)
      println(text)
      println("Offset: " + offset)
      //println("After offset: " + d.string.substring(offset))
      offset = d.string.indexOf(text)
    }
    if(currNode.head == "post") {
      if(!inside) {
        start = d.string.indexOf(text, offset)
        inside = true
      }
      offset = d.string.indexOf(text, offset) + text.length
    }
    if(currNode.head == "quote") {
      if(inside) {
        if(offset > start && d.string.substring(start,offset).trim.length > 1) d += new Paragraph(d, start, offset)
        start = d.string.indexOf(text, offset) + text.length
        inside = false
      } else {
        start = d.string.indexOf(text, offset) + text.length
      }
      offset = d.string.indexOf(text, offset) + text.length
    }
    if(currNode.head == "a" && !currNode.contains("quote")) {
      if(!inside) {
        start = d.string.indexOf(text, offset)
        inside = true
      }
      offset = d.string.indexOf(text, offset) + text.length + 4
    }
  }
  def addSectionDoc(d : Document) {
    inside = false
    if(offset > start && d.string.substring(start,offset).trim.length > 1) d += new Paragraph(d, start, offset)
  }
}