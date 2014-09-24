package cc.factorie.app.nlp
import cc.factorie.util._
import cc.factorie.app.nlp.segment._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.parse._
import com.mongodb._
import org.bson.types.BasicBSONList
import cc.factorie.db.mongo.MongoCubbieCollection
import java.io.File

object DocumentStore {
  
  class DocumentCubbie extends Cubbie {
    val name = StringSlot("name")
    val date = DateSlot("date")
    val string = StringSlot("string")
    val tokens = IntSeqSlot("tokens")
    val sentences = IntSeqSlot("sentences")
    val pos = IntSeqSlot("pos")
    //val ner = IntSeqSlot("ner")
    val parse = IntSeqSlot("parse")
    val mentions = IntSeqSlot("mentions")
    def store(doc:Document): this.type = {
      require(doc.sections.length == 1)
      this.id = doc.name
      this.date := new java.util.Date()
      this.string := doc.string
      val section = doc.asSection
      // Tokens
      val tokenOffsets = new IntArrayBuffer(section.tokens.size)
      var o = 0
      for (token <- section.tokens) {
        val startOffset = token.stringStart - o
        //println(s"startOffset=$startOffset stringStart=${token.stringStart} stringEnd=${token.stringEnd} o=$o token=${token.string}");
        require(startOffset >= 0 && startOffset < 0xffff)
        val endOffset = token.stringEnd - token.stringStart; require(endOffset >= 0 && endOffset < 0xffff)
        tokenOffsets += (startOffset << 16) + endOffset
        o = token.stringEnd - 1
      }
      this.tokens := tokenOffsets
      // Sentences
      val sentenceOffsets = new IntArrayBuffer(section.sentences.length * 2)
      for (sentence <- section.sentences) {
        sentenceOffsets += sentence.start
        sentenceOffsets += sentence.length
      }
      this.sentences := sentenceOffsets
      // Part-of-speech tags
      this.pos := new ArrayIntSeq(section.tokens.map(_.posTag.intValue).toArray)
//      // Parse
//      val parseIndices = new IntArrayBuffer(section.tokens.length * 2)
//      for (sentence <- section) {
//        val parse = sentence.parse
//        parseIndices ++= parse.parents
//        parseIndices ++= parse.labels.map(_.intValue)
//      }
//      this.parse := parseIndices
      this
    }
    def fetch: Document = {
      val document = new Document(string.value)
      val section = document.asSection
      // Tokens
      val tokenOffsets = tokens.value
      var len = tokenOffsets.length; var o = 0; var i = 0; while (i < len) {
        val tokenOffset = tokenOffsets(i)
        val stringStart = o + (tokenOffset >>> 16)
        val stringEnd = stringStart + (tokenOffset & 0xffff)
        //println(s"to=tokenOffset stringStart=$stringStart stringEnd=$stringEnd o=$o")
        o = stringEnd - 1
        new Token(section, stringStart, stringEnd)
        i += 1
      }
      // Sentences
      val sentenceOffsets = sentences.value
      len = sentenceOffsets.length; i = 0; while (i < len) {
        new Sentence(section, sentenceOffsets(i), sentenceOffsets(i+1))
        i += 2
      }
      // Part-of-speech tags
      val posIndices = pos.value; i = 0
      for (token <- section.tokens) {
        token.attr += new PennPosTag(token, posIndices(i))
        i += 1
      }
//      // Parse
//      val parseIndices = parse.value
//      i = 0
//      for (sentence <- section.sentences) {
//        val parents = parseIndices.slice(i, i+sentence.length).toSeq; i += sentence.length
//        val labels = parseIndices.slice(i, i+sentence.length).toSeq.map(ParseTreeLabelDomain.category(_)); i += sentence.length
//        val parse = new ParseTree(sentence, parents, labels)
//      }
//      assert(i == parseIndices.length)
      document
    }
  }
    
  class MentionCubbie extends Cubbie {
    val docid = StringSlot("docid")
  }
  
  def main(args:Array[String]): Unit = {
    val ds = new DocumentStore()
    //val f = new File("/Users/mccallum/research/data/text/plain/tweet1.txt"); ds += f
    for (file <- new File("/Users/mccallum/research/data/text/plain").listFiles) ds += file
    ds.show()
  }
}

class DocumentStore(mongoDB:String = "DocumentDB") {
  import DocumentStore.DocumentCubbie
  val mongo = new Mongo()
  val db = mongo.getDB(mongoDB)
  val collection = db.getCollection("documents")
  val cubbieCollection = new MongoCubbieCollection[DocumentCubbie](collection, () => new DocumentCubbie)

  //val annotator = DocumentAnnotatorPipeline(DeterministicTokenizer, DeterministicSentenceSegmenter, OntonotesForwardPosTagger, WSJTransitionBasedParser)
  val annotator = DocumentAnnotatorPipeline(DeterministicTokenizer, DeterministicSentenceSegmenter)
  def +=(doc:Document): Unit = {
    annotator.process(doc)
    println(s"Adding doc tokens=${doc.tokenCount}")
    cubbieCollection += (new DocumentCubbie().store(doc))
  }
  def ++=(docs:Iterable[Document]): Unit = {
    annotator.processParallel(docs)
    cubbieCollection ++=(docs.map(d => new DocumentCubbie().store(d)))
  }
  
  def +=(file:java.io.File): Unit = {
    val doc = new Document(scala.io.Source.fromFile(file).mkString)
    doc.setName("file:/"+file.toString)
    +=(doc)
  }
  def +=(url:java.net.URL): Unit = {
    val doc = new Document(scala.io.Source.fromInputStream(url.openStream).mkString)
    doc.setName(url.toString)
    +=(doc)
  }
  def +=(docString:String, name:String): Unit = {
    val doc = new Document(docString)
    doc.setName(name)
    +=(doc)
  }
  
  def show(): Unit = {
    val cubbieIterator = cubbieCollection.iterator 
    for (cubbie <- cubbieIterator) {
      val doc = cubbie.fetch
      println(doc.owplString(annotator))
      println()
    }
    cubbieIterator.close()
  }
  
  
  // Scraps not currently used:
  
  class PosCubbie extends Cubbie {
    val annotator = StringSlot("annotator")
    val annotation = StringSlot("annotation")
    val timestamp = DateSlot("ts")
    val data = IntSeqSlot("data")
  }
  
  def tokensToIntSeq(doc:Document): IntSeq = {
    val tokenOffsets = new IntArrayBuffer(doc.asSection.tokens.size)
    var o = 0
    for (token <- doc.asSection.tokens) {
      val startOffset = token.stringStart - o
      //println(s"startOffset=$startOffset stringStart=${token.stringStart} stringEnd=${token.stringEnd} o=$o token=${token.string}");
      require(startOffset >= 0 && startOffset < 0xffff)
      val endOffset = token.stringEnd - token.stringStart; require(endOffset >= 0 && endOffset < 0xffff)
      tokenOffsets += (startOffset << 16) + endOffset
      o = token.stringEnd - 1
    }
    tokenOffsets
  }
  def intSeqToTokens(doc:Document, tokenOffsets:IntSeq): Unit = {
    val section = doc.asSection
    var len = tokenOffsets.length; var o = 0; var i = 0; while (i < len) {
      val tokenOffset = tokenOffsets(i)
      val stringStart = o + (tokenOffset >>> 16)
      val stringEnd = stringStart + (tokenOffset & 0xffff)
      //println(s"to=tokenOffset stringStart=$stringStart stringEnd=$stringEnd o=$o")
      o = stringEnd - 1
      new Token(section, stringStart, stringEnd)
      i += 1
    }
  }
  

}

