package cc.factorie.app.nlp
import cc.factorie.util._
import cc.factorie.app.nlp.segment._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.parse._
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.phrase._
import com.mongodb._
import org.bson.types.BasicBSONList
import cc.factorie.db.mongo.MongoCubbieCollection
import java.io.File

/** A Cubbie with custom slot classes for storing various nlp.Document annotations. */
class DocumentCubbie extends Cubbie {
  
  /** Stores all Sections stringStart and stringEnd, but no other annotations. */
  class DocumentSectionsSlot(name:String) extends IntSeqSlot(name) {
    def pickle(document:Document): this.type = {
      val sectionOffsets = new IntArrayBuffer(document.sections.length)
      for (section <- document.sections) {
        sectionOffsets += section.stringStart
        sectionOffsets += section.stringEnd
      }
      this := sectionOffsets
      this
    }
    def unpickle(document:Document): this.type = {
      val sectionOffsets = this.value
      val len = sectionOffsets.length; var i = 0; while (i < len) {
        new BasicSection(document, sectionOffsets(i), sectionOffsets(i+1))
        i += 2
      }
      this
    }
  }
  object DocumentSectionsSlot { def apply(name:String) = new DocumentSectionsSlot(name) }
  
  /** Stores Token start/end within a Section. */
  class SectionTokensSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
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
      this := tokenOffsets
      this
    }
    def =:(section:Section): this.type = {
      val tokenOffsets = this.value
      var len = tokenOffsets.length; var o = 0; var i = 0; while (i < len) {
        val tokenOffset = tokenOffsets(i)
        val stringStart = o + (tokenOffset >> 16)
        val stringEnd = stringStart + (tokenOffset & 0xffff)
        //println(s"to=tokenOffset stringStart=$stringStart stringEnd=$stringEnd o=$o")
        o = stringEnd - 1 // Allow a 1-character overlap between a token and its successor.  This is used by the tokenizer for "U.S.A." "." for end-of-sentence
        new Token(section, stringStart, stringEnd)
        i += 1
      }
      section.document.annotators(classOf[Token]) = this.getClass
      this
    }
  }
  object SectionTokensSlot { def apply(name:String) = new SectionTokensSlot(name) }
  
  /** Stores Sentence start/length within a Section. */
  class SectionSentencesSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      val sentenceOffsets = new IntArrayBuffer(section.sentences.length * 2)
      for (sentence <- section.sentences) {
        sentenceOffsets += sentence.start
        sentenceOffsets += sentence.length
      }
      this := sentenceOffsets
      this
    }
    def unpickle(section:Section): this.type = {
      val sentenceOffsets = this.value
      val len = sentenceOffsets.length; var i = 0; while (i < len) {
        new Sentence(section, sentenceOffsets(i), sentenceOffsets(i+1))
        i += 2
      }
      section.document.annotators(classOf[Sentence]) = this.getClass
      this
    }
  }
  object SectionSentencesSlot { def apply(name:String) = new SectionSentencesSlot(name) }
  
  /** Store the Token start/end and Sentences start/length in one IntSeq.  Combining both improves compression. */
  class SectionTokensAndSentencesSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      val offsets = new IntArrayBuffer(2 + section.tokens.length * 2 + section.sentences.length * 2)
      offsets += section.tokens.length
      offsets += section.sentences.length
      var o = 0
      for (token <- section.tokens) {
        offsets += token.stringStart - o
        offsets += token.stringEnd - o
        o = token.stringStart
      }
      o = 0
      for (sentence <- section.sentences) {
        offsets += sentence.start - o
        offsets += sentence.length
        o = sentence.start + sentence.length
      }
      this := offsets
      this
    }
    def =:(section:Section): this.type = {
      val offsets = this.value
      var i = 0
      val numTokens = offsets(i); i += 1
      val numSentences = offsets(i); i += 1
      var cap = 2 + numTokens*2
      var o = 0
      while (i < cap) {
        val t = new Token(section, offsets(i) + o, offsets(i+1) + o)
        o = t.stringStart
        i += 2
      }
      cap = 2 + numTokens*2 + numSentences*2
      assert(cap == offsets.length)
      o = 0
      while (i < cap) {
        //println(s"new Sentence(${offsets(i)+o} ${offsets(i+1)})")
        val s = new Sentence(section, offsets(i) + o, offsets(i+1))
        o = s.start + s.length
        i += 2
      }
      section.document.annotators(classOf[Token]) = this.getClass
      section.document.annotators(classOf[Sentence]) = this.getClass
      this
    }
  }
  object SectionTokensAndSentencesSlot { def apply(name:String) = new SectionTokensAndSentencesSlot(name) }

  /** Store the part-of-speech tags for every Token within a Section. */
  class SectionPennPosTagsSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      this := new ArrayIntSeq(section.tokens.map(_.attr[PennPosTag].intValue).toArray)
      this
    }
    def =:(section:Section): this.type = {
      val posIndices = this.value; var i = 0
      for (token <- section.tokens) {
        token.attr += new PennPosTag(token, posIndices(i))
        i += 1
      }
      section.document.annotators(classOf[PennPosTag]) = this.getClass
      this
    }
  }
  object SectionPennPosTagsSlot { def apply(name:String) = new SectionPennPosTagsSlot(name) }

  /** Store the BILOU CoNLL ner tags for every Token within a Section */
  class SectionConllNerTagsSlot(name: String) extends IntSeqSlot(name) {
    def :=(section: Section): this.type = {
      this := new ArrayIntSeq(section.tokens.map(_.attr[BilouConllNerTag].intValue).toArray)
      this
    }
    def =:(section: Section): this.type = {
      val nerIndices = this.value; var i = 0
      for (token <- section.tokens) {
        token.attr += new BilouConllNerTag(token, BilouConllNerDomain.category(nerIndices(i)))
        i += 1
      }
      section.document.annotators(classOf[BilouConllNerTag]) = this.getClass
      this
    }
  }
  object SectionConllNerTagsSlot { def apply(name: String) = new SectionConllNerTagsSlot(name) }
  
  /** Store the ParseTree for every Sentence within a Section. */
  class SectionParseTreesSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      val parseIndices = new IntArrayBuffer(section.tokens.length * 2)
      for (sentence <- section.sentences) {
        val parse = sentence.parse
        parseIndices ++= parse.parents
        parseIndices ++= parse.labels.map(_.intValue)
      }
      this := parseIndices
      this
    }
    def =:(section:Section): this.type = {
      val parseIndices = this.value; var i = 0
      for (sentence <- section.sentences) {
        val parse = new ParseTree(sentence, parseIndices.slice(i, i+sentence.length).asArray, parseIndices.slice(i+sentence.length, i+2*sentence.length).asArray)
        i += 2*sentence.length
      }
      section.document.annotators(classOf[ParseTree]) = this.getClass
      this
    }
  }
  object SectionParseTreesSlot { def apply(name:String) = new SectionParseTreesSlot(name) }
  
  /** Store together in one IntSeq the part-of-speech tags for every Token within a Section and the ParseTree for every Sentence within a Section. */
  class SectionPosAndParseSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      val indices = new IntArrayBuffer(section.tokens.length * 3)
      for (token <- section.tokens) indices += token.attr[PennPosTag].intValue
      for (sentence <- section.sentences) {
        val l = indices.length
        val parse = sentence.parse
        indices ++= parse.parents
        indices ++= parse.labels.map(_.intValue)
        assert(l+ 2*sentence.length == indices.length)
      }
      this := indices
      this
    }
    def =:(section:Section): this.type = {
      val indices = this.value; var i = 0
      for (token <- section.tokens) {
        token.attr += new PennPosTag(token, indices(i))
        i += 1
      }
      for (sentence <- section.sentences) {
        val parents = indices.slice(i, i+sentence.length).asArray
        val labels = indices.slice(i+sentence.length, i+2*sentence.length).asArray
        //println(s"n=${parents.length} parents = ${parents.mkString(",")}\n labels = ${labels.mkString(",")}")
        val parse = new ParseTree(sentence, parents, labels)
        sentence.attr += parse
        i += 2*sentence.length
      }
      section.document.annotators(classOf[PennPosTag]) = this.getClass
      section.document.annotators(classOf[ParseTree]) = this.getClass
      this
    }
  }
  object SectionPosAndParseSlot { def apply(name:String) = new SectionPosAndParseSlot(name) }

  /** Stores, Phrases, Mentions, Entities, and Mention-Entity linkage.
      Does not store attributes of Entities, Mentions or Phrases.  This could be done in different DocumentCubbie slots.
      The DocumentSectionSlot must be unpicked before this one. */
  class DocumentMentionsSlot(name:String) extends IntSeqSlot(name) {
    def :=(document:Document): this.type = {
      val coref = document.coref
      val mentions = coref.mentions
      val corefIndices = new IntArrayBuffer(mentions.length * 5)
      //val sectionMap = if (document.sectionCount == 1) new scala.collection.mutable.LinkedHashMap() ++= document.sections.zipWithIndex
      val entityMap = new scala.collection.mutable.LinkedHashMap() ++= coref.entities.zipWithIndex
      for (entity <- coref.entities) require(entity.uniqueId.startsWith(document.name))
      for (mention <- mentions) {
        corefIndices += mention.phrase.section.indexInDocument
        corefIndices += mention.phrase.start 
        corefIndices += mention.phrase.length 
        corefIndices += mention.phrase.headTokenOffset
        corefIndices += entityMap(mention.entity)
      }
      this := corefIndices
      this
    }
    def =:(document:Document): this.type = {
      val coref = new WithinDocCoref(document)
      val corefIndices = this.value;
      var i = 1; while (i < corefIndices.length) {
        val phrase = new Phrase(document.sections(corefIndices(i)), corefIndices(i+1), corefIndices(i+2), corefIndices(i+3))
        coref.addMention(phrase, corefIndices(i+1)) // The last argument is the "entityKey", and will automatically lookup or create the proper WithinDocEntity
        i += 5
      }
      assert(i == corefIndices.length)
      document.annotators(classOf[Mention]) = this.getClass
      this
    }
  }
  object DocumentMentionsSlot { def apply(name:String) = new DocumentMentionsSlot(name) }
  
  /** Stores ConllEntityType attributes on Phrase.
      The DocumentMentionsSlot must be unpicked before this one. */
  class DocumentMentionsConllEntityTypeSlot(name:String) extends IntSeqSlot(name) {
    def :=(document:Document): this.type = {
      val mentions = document.coref.mentions
      val etIndices = new IntArrayBuffer(mentions.length)
      for (mention <- mentions) etIndices += mention.phrase.attr[ConllEntityType].intValue
      this := etIndices
      this
    }
    def unpickle(document:Document): this.type = {
      val coref = new WithinDocCoref(document)
      val etIndices = this.value;
      var i = 0; for (mention <- document.coref.mentions) {
        mention.attr += new ConllEntityType(etIndices(i))
        i += 1
      }
      assert(i == etIndices.length)
      document.annotators(classOf[ConllEntityType]) = this.getClass
      this
    }
  }
  object DocumentMentionsConllEntityTypeSlot { def apply(name:String) = new DocumentMentionsConllEntityTypeSlot(name) }
}

class StandardSectionAnnotationsCubbie extends DocumentCubbie {
  def this(section:Section) = { this(); this := section }
  val start = IntSlot("start")
  val end = IntSlot("end")
  val ts = SectionTokensAndSentencesSlot("ts")
  val pp = SectionPosAndParseSlot("pp")
  val ner = SectionConllNerTagsSlot("ner")
  def :=(section:Section): this.type = {
    start := section.stringStart
    end := section.stringEnd
    ts := section
    if (section.document.annotatorFor(classOf[PosTag]).isDefined && section.sentences.head.attr.contains(classOf[ParseTree])) pp := section
    if (section.document.annotatorFor(classOf[NerTag]).isDefined) ner := section
    this
  }
  def =:(document:Document): this.type = {
    val section = new BasicSection(document, start.value, end.value); document += section
    section =: ts 
    if (pp.isDefined) section =: pp
    if (ner.isDefined) section =: ner
    this
  }
}

class StandardDocumentCubbie extends DocumentCubbie {
  def this(document:Document) = { this(); this := document }
  val string = StringSlot("string")
  val name = StringSlot("name")
  val date = DateSlot("date")
  val sections = CubbieListSlot("sections", () => new StandardSectionAnnotationsCubbie) // Only present if there are multiple Sections
  val section = CubbieSlot("section", () => new StandardSectionAnnotationsCubbie) // Only present if there is one Section
  val mentions = DocumentMentionsSlot("mentions")
  def :=(document:Document): this.type = {
    name := document.name
    date := new java.util.Date()
    string := document.string
    if (document.sections.length == 1 && document.sections.head == document.asSection)
      section := new StandardSectionAnnotationsCubbie(document.asSection)
    else 
      sections := document.sections.map(section => new StandardSectionAnnotationsCubbie(section))
    if (document.coref ne null) mentions := document 
    this
  }
  def document: Document = {
    val doc = new Document(string.value)
    doc.setName(name.value)
    doc.attr += date.value
    if (sections.isDefined) { 
      for (sc <- sections.value) 
        doc =: sc
    }
    else { assert(section.isDefined); doc =: section.value }
    if (mentions.isDefined) doc =: mentions
    doc
  }
}

class MentionCubbie extends Cubbie {
  val docid = StringSlot("docid")
}

/** Facilities for efficiently and compactly storing annotated Documents to Cubbies (and through them to MongoDB).
    @author Andrew McCallum */
object DocumentStore {
  
  def main(args:Array[String]): Unit = {
    val ds = new DocumentStore()
    ds.db.dropDatabase()
    //val f = new File("/Users/mccallum/research/data/text/plain/tweet1.txt"); ds += f
    for (file <- new File("/Users/mccallum/research/data/text/plain").listFiles) ds += file
    ds.show()
  }

}

class DocumentStore(mongoDB:String = "DocumentDB") {
  val mongo = new MongoClient()
  val db = mongo.getDB(mongoDB)
  val collection = db.getCollection("documents")
  val cubbieCollection = new MongoCubbieCollection[StandardDocumentCubbie](collection, () => new StandardDocumentCubbie)

  val annotator = DocumentAnnotatorPipeline(DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter, OntonotesForwardPosTagger, WSJTransitionBasedParser, ParseForwardCoref)
  //val annotator = DocumentAnnotatorPipeline(DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter, OntonotesForwardPosTagger, WSJTransitionBasedParser)
  //val annotator = DocumentAnnotatorPipeline(DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter)
  def +=(doc:Document): Unit = {
    annotator.process(doc)
    //println(s"Adding doc tokens=${doc.tokenCount}")
    println("Input document:"); println(doc.owplString(annotator))
    cubbieCollection += new StandardDocumentCubbie(doc)
  }
  def ++=(docs:Iterable[Document]): Unit = {
    annotator.processParallel(docs)
    cubbieCollection ++= docs.map(d => new StandardDocumentCubbie(d))
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
      val doc = cubbie.document
      println(doc.owplString(annotator))
      for (entity <- doc.coref.entities) {
        println(entity.uniqueId+":")
        for (mention <- entity.mentions) {
          println("  "+ mention.phrase.string+"\ttoken"+mention.phrase.head.position)
        }
      }
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

