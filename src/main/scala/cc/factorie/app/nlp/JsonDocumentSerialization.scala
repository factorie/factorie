package cc.factorie.app.nlp


import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.parse.{ParseTree, ParseTreeLabelDomain}
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.app.nlp.pos.PennPosLabel
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.lang.reflect.{Type, ParameterizedType}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference





object JsonDocumentSerialization {

  def serialize(docs: Iterator[cc.factorie.app.nlp.Document], outputFilename: String): Unit = {
    val writer = new PrintWriter(new OutputStreamWriter(
      new GZIPOutputStream(new FileOutputStream(outputFilename)), "ISO-8859-15"))
    for (d <- docs) {
      val str = serialize(d)
      writer.println(str)
    }
    writer.flush()
    writer.close
  }

  def deserializeFiles(filenames: Seq[String],
                       encoding: String = "UTF-8" //ISO-8859-15"
                        ): Iterator[cc.factorie.app.nlp.Document] = {
    filenames.iterator.flatMap(f => deserializeFile(f, encoding))
  }

  def deserializeFile(inputFilename: String, encoding: String = "UTF-8"): Iterator[cc.factorie.app.nlp.Document] = { //ISO-8859-15"
    //*
    new Iterator[cc.factorie.app.nlp.Document] {
      val reader = new BufferedReader(new InputStreamReader(
        new GZIPInputStream(new FileInputStream(inputFilename)), encoding))
      var line = reader.readLine()

      def hasNext = line != null

      def next() = {
        val d = deserialize(line)
        line = reader.readLine()
        d
      }
    }
  }

  def deserializeFileInPar(inputFilename: String, encoding: String = "UTF-8"): Iterator[cc.factorie.app.nlp.Document] = {
    val fileIter = new Iterator[String] {
      val reader = new BufferedReader(new InputStreamReader(
        new GZIPInputStream(new FileInputStream(inputFilename)), encoding))
      var line = reader.readLine()
      def hasNext = line != null
      def next() = {
        val d = line
        line = reader.readLine()
        d
      }
    }
    fileIter.grouped(100).flatMap(_.par.map(line => deserialize(line)))
  }

  def serialize(doc: cc.factorie.app.nlp.Document, newString: String = ""): String = {
    JacksonWrapper.serialize(serializeDoc(doc, newString))
  }

  def deserialize(string: String): cc.factorie.app.nlp.Document = {
    deserializeDoc((JacksonWrapper).deserialize[LightweightDocument](string))
  }

  private def serializeDoc(d: cc.factorie.app.nlp.Document, newString: String = ""): LightweightDocument = {
    val sections: ArrayBuffer[LightweightSection] = new ArrayBuffer
    for (s <- d.sections) {
      val tokens: ArrayBuffer[LightweightToken] = new ArrayBuffer
      for (t <- s.tokens) {
        tokens += LightweightToken(t.stringStart, t.stringEnd - t.stringStart,
          t.string, t.lemmaString, t.posLabel.categoryValue, t.parseParentIndex,
          t.parseLabel.categoryValue)
      }
      sections += LightweightSection(s.stringStart, s.stringEnd,
        s.sentences.map(_.start), s.sentences.map(_.length), tokens)
    }


    LightweightDocument(d.name, if (newString == "") d.string else newString,
      sections)
  }

  private def deserializeDoc(doc: LightweightDocument): cc.factorie.app.nlp.Document = {
    var result:cc.factorie.app.nlp.Document = null
    try{
      val d = new cc.factorie.app.nlp.Document(doc.text.replaceAll("\t", " ").replaceAll("\n", " ")).setName(doc.name)
      for (s <- doc.sects) d += deserializeSection(s, d)
      result=d
    }
    catch{
      case e:Exception => {result=new cc.factorie.app.nlp.Document("").setName("[EMPTY]");println("Exception caught while reading document. Returning empty document.");e.printStackTrace}
      case e:Error => {result=new cc.factorie.app.nlp.Document("").setName("[EMPTY]");println("Error thrown while reading document. Returning empty document.");e.printStackTrace}
    }
    result
  }

  private def deserializeSection(s: LightweightSection, doc: cc.factorie.app.nlp.Document) = {
    val section = new Paragraph(doc, s.start, s.end)
    val tokenStarts = s.tokens.map(_.start)

    val tokenStrings = s.tokens.map(_.string)
    val tokenLemma = s.tokens.map(_.lemma)
    val tokenPOS = s.tokens.map(_.pos)
    val tokenLengths = s.tokens.map(_.length)


    for (i <- 0 until tokenStarts.length) {
      val token = new cc.factorie.app.nlp.Token(section, tokenStarts(i), tokenStarts(i) + tokenLengths(i))
      token.attr += new TokenString(token, tokenStrings(i))
      token.attr += new TokenLemma(token, tokenLemma(i))
      token.attr += new PennPosLabel(token, tokenPOS(i))
    }

    val sentenceStarts = s.senStarts
    val sentenceLens = s.senLens

    //val tokenNER = this.tokenNER.value
    val tokenParseParent = s.tokens.map(_.parseParent)
    val tokenParseDepRel = s.tokens.map(_.parseLabel)


    for (i <- 0 until sentenceStarts.length) {
      // add sentence
      new cc.factorie.app.nlp.Sentence(section, sentenceStarts(i), sentenceLens(i))
    }

    // add parse (fill the domain to start)
    ParseTreeLabelDomain ++= tokenParseDepRel
    for (s <- section.sentences) {
      val newTree = new ParseTree(s)
      for (si <- 0 until s.length) {
        val di = s.start + si
        newTree.setParent(si, tokenParseParent(di))
        val li = ParseTreeLabelDomain.index(tokenParseDepRel(di))
        newTree.label(si).set(li)(null)
      }
      s.attr += newTree
    }

    section
  }

  class Paragraph(val document:Document, val stringStart:Int, val stringEnd:Int) extends Section


  case class LightweightToken(start: Int,
                              length: Int,
                              string: String,
                              lemma: String,
                              pos: String,
                              parseParent: Int,
                              parseLabel: String)

  case class LightweightSection(start: Int,
                                end: Int,
                                senStarts: Seq[Int],
                                senLens: Seq[Int],
                                tokens: Seq[LightweightToken])

  case class LightweightDocument(name: String,
                                 text: String,
                                 sects: Seq[LightweightSection]
                                  )

}



class JacksonWrapper {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def serialize(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def deserialize[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) {m.erasure}
    else new ParameterizedType {
      def getRawType = m.erasure

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }
}

object JacksonWrapper extends JacksonWrapper





