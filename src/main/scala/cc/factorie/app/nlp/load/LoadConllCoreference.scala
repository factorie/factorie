package cc.factorie.app.nlp.load

import java.io.File

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.{Mention, WithinDocCoref, WithinDocEntity}
import cc.factorie.app.nlp.phrase.{OntonotesPhraseEntityType, Phrase}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.variable.Span

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

/**
 * Loader for conll 2011 or conll 2012, call separately for training data and test data if both are needed
 * If auto data file or directory is NOT supplied:
 *     if loadFromParse = False then gold mentions grouped as their true entities annotated in the goldFile are loaded onto the Document.targetCoref object
 *     if loadFromParse = True then gold mentions grouped as their true entities and all noun phrases, added as singletons, are loaded onto the targetCoref
 * If autoDirectory or File IS supplied:
 *     Gold annotated mentions grouped as entities are loaded from the goldFile given onto the Document.targetCoref
 *     if loadFromParse = False then annotated predicted mentions in the autoDir/autoFile will be loaded as singletons onto Document.coref to be coreferred
 *     if loadFromParse = True then all noun phrases and pronouns from the predicted constituency parse will be added as singletons to the Document.coref
 *
 * goldFilename String path to flattened conll key file with gold coreference annotation, if only annotation and not testing is wanted, the flattened auto filename can be given here as well
 * limitNumDocuments Int count of documents to load
 * autoDirOpt Option[String] (Optional) Directory path or file path to conll_auto data
 */

object LoadConllCoreference {
  def load(goldFilename: String, limitNumDocuments: Int, loadFromParse: Boolean, autoDirOpt:Option[String] = None): Seq[Document] = {
    val conllLoader = new Conll2011Iterator(goldFilename, loadFromParse, autoDirOpt.map {new File(_)})
    val docs = new ArrayBuffer[Document]()
    while (conllLoader.hasNext && (limitNumDocuments == -1 || docs.size < limitNumDocuments)) {
      docs += conllLoader.next()
    }
    disperseEntityTypes(docs.map(_.getTargetCoref))
    if (autoDirOpt.isDefined) disperseEntityTypes(docs.map(_.getCoref))
    docs
  }

  def disperseEntityTypes(corefDocs:Seq[WithinDocCoref]):Unit = {
    for (corefDoc <- corefDocs) {
      val entities = corefDoc.mentions.toSeq.groupBy(m => m.entity).filter(x => x._2.length > 1)
      for (ent <- entities) {
        val entityTypes = ent._2.map(m => m.phrase.attr[OntonotesPhraseEntityType].categoryValue).filter(t => t != "O").distinct
        if (entityTypes.length > 0) {
          //Note, this takes the first entity type in case of within cluster entity type agreement
          val newType = entityTypes(0)
          ent._2.foreach(m => m.phrase.attr[OntonotesPhraseEntityType].target.setCategory(newType)(null))
        }
      }
    }
  }
}

class Conll2011Iterator(goldFile: String, loadFromParse: Boolean = true, autoDirOpt:Option[File] = None) extends Iterator[Document] {
  private val goldDocs = new ConllOWPLIterator(goldFile)
  private val useExactEntTypeMatch = true
  private val autoMapOpt = autoDirOpt.map { autoDir =>
    if(autoDir.isDirectory) {
      autoDir.listFiles().flatMap {autoFile => new ConllOWPLIterator(autoFile.getAbsolutePath).toSeq}.toMap
    } else {
      new ConllOWPLIterator(autoDir.getAbsolutePath).toMap
    }
  }

  def next() = {
    val (id, goldLines) = goldDocs.next()
    val doc = new Document()
    doc.setName(id)

    val autoLinesOpt = autoMapOpt.flatMap(_.get(id))

    doc.getCoref

    var docTokIdx = -1
    var sentenceIdx = -1
    var currSentence: Sentence = null

    val goldAnnotationResolver = new DocumentMentionBoundariesResolver(doc.getTargetCoref, key = true)
    val autoAnnotationResolver: DocumentMentionBoundariesResolver = if (autoLinesOpt.isDefined) new DocumentMentionBoundariesResolver(doc.getCoref, key = false) else null

    while (goldLines.hasNext) {
      val goldLine = goldLines.next()
      val autoLine: Option[String] = autoLinesOpt.map {
        _.next()
      }

      if (goldLine == "") {
        doc.appendString("\n")
      } else if (goldLine != "#end document") {
        docTokIdx += 1
        val goldFields = goldLine.split("\\s+")
        val tokId = goldFields(2).toInt
        val word = unescapeBrackets(goldFields(3))
        doc.appendString(" ")
        if (tokId == 0) {
          currSentence = new Sentence(doc)
          goldAnnotationResolver.createNewSentence(currSentence)
          if (autoLinesOpt.isDefined) autoAnnotationResolver.createNewSentence(currSentence)
          sentenceIdx += 1
        }

        val token = new Token(currSentence, word)
        token.attr += new PennPosTag(token, goldFields(4))
        token.attr += new MentionSpeaker(goldFields(9))

        goldAnnotationResolver.storeNEREntityChunks(goldFields(10), currSentence, token, docTokIdx)
        goldAnnotationResolver.storeCorefEntityChunk(goldFields.last, currSentence, docTokIdx, doc.name)

        if (autoLinesOpt.isDefined) {
          val autoFields = autoLine.get.split("\\s+")
          val autoWord = unescapeBrackets(autoFields(3))
          if(autoWord != word)
            println( "Gold Document and Auto Document are out of sync")
          val constituencyLabels = autoFields(5).split("\\*")
          if (constituencyLabels.length >= 1 && loadFromParse) {
            if (token.posTag.categoryValue == "PRP" || token.posTag.categoryValue == "PRP$") autoAnnotationResolver.createPRPMentions(token, docTokIdx)
            autoAnnotationResolver.createNPMentionsFromParse(constituencyLabels, docTokIdx)
          } else if (!loadFromParse) {
            autoAnnotationResolver.resolveAnnotatedMentions(docTokIdx)
          }
          autoAnnotationResolver.prevWord = word
        } else {
          val constituencyLabels = goldFields(5).split("\\*")
          if (constituencyLabels.length >= 1 && loadFromParse) {
            if (token.posTag.categoryValue == "PRP" || token.posTag.categoryValue == "PRP$") goldAnnotationResolver.createPRPMentions(token, docTokIdx)
            goldAnnotationResolver.createNPMentionsFromParse(constituencyLabels, docTokIdx)
          }
        }
        goldAnnotationResolver.resolveAnnotatedMentions(docTokIdx)
        goldAnnotationResolver.prevWord = word
      }
    }
    doc
  }

  val tokenizer = """(\(|\||\)|\d+)""".r
  val entityTypeTokenizer = """(\(|[^\)]+|\)|)""".r
  val asteriskStripper = """\*""".r

  @inline def unescapeBrackets(s: String) =
    s match {
      case "-LRB-" => "("
      case "-RRB-" => ")"
      case "-LSB-" => "["
      case "-RSB-" => "]"
      case "-LCB-" => "{"
      case "-RCB-" => "}"
      case _ => s
    }

  class DocumentMentionBoundariesResolver(coref: WithinDocCoref, key: Boolean = true, loadFromParse: Boolean = false) {
    val mentionBoundaries = new scala.collection.mutable.LinkedHashMap[Span[Section, Token], CoreferentEntityChunk]
    val useEntityType = key
    val _spanToEntityType = new scala.collection.mutable.LinkedHashMap[Span[Section, Token], String]
    var unResolvedEntityType: EntityTypeChunk = null
    val entities = Map[String, WithinDocEntity]()

    val parseStack = collection.mutable.Stack[(String, Int)]()
    var currParseTree: ConstituencyParse = null

    val openEntityStack = mutable.Map[String, collection.mutable.Stack[CoreferentEntityChunk]]()

    var numMentions = 0
    var prevPhrase = ""
    var prevWord = ""

    case class CoreferentEntityChunk(entityId: String, mentionStart: Int, var found: Boolean = false)
    case class EntityTypeChunk(entityType: String, start: Int)

    private def tokenizeEntityType(s: String): Array[String] = {
      entityTypeTokenizer.findAllIn(s).map(x => asteriskStripper.replaceAllIn(x, "")).map(_.toString).toArray
    }

    def createNewSentence(newSentence: Sentence) = {
      parseStack.clear()
      currParseTree = new ConstituencyParse(newSentence,0,"TOP")
      prevPhrase = ""
      prevWord = ""
    }

    def storeNEREntityChunks(nerChunk: String, sentence: Sentence, token: Token, docTokIdx: Int): Unit = {
      val entityTypeTokens = tokenizeEntityType(nerChunk).filterNot(_.isEmpty)
      entityTypeTokens match {
        case Array("(", entityTypeString: String, ")") => _spanToEntityType.put(new TokenSpan(sentence.section, docTokIdx, 1).value, entityTypeString)
        case Array("(", entityTypeString) =>
          assert(unResolvedEntityType eq null, "Nested Entity Types Found")
          unResolvedEntityType = new EntityTypeChunk(entityTypeString, docTokIdx)
        case Array(")") =>
          _spanToEntityType.put(new TokenSpan(sentence.section, unResolvedEntityType.start, docTokIdx - unResolvedEntityType.start + 1).value, unResolvedEntityType.entityType)
          unResolvedEntityType = null
        case _ =>
      }
    }

    def createPRPMentions(token: Token, docTokIdx: Int): Unit = {
      val span = new TokenSpan(coref.document.asSection, docTokIdx, 1)
      val newMention = coref.addMention(new Phrase(span, 0))//span.tokens.indexOf(currParseTree.current.getHeadToken(docTokIdx))))
      numMentions += 1

      val entityChunkForMention = mentionBoundaries.getOrElse(newMention.phrase.value, new CoreferentEntityChunk(coref.document.name + "-" + (-coref.mentions.size), docTokIdx, true))
      //Register that we have found this mention
      entityChunkForMention.found = true
      val entityKey = if (key) entityChunkForMention.entityId
      else coref.document.name + "-" + (-coref.mentions.size)
      newMention.attr += new EntityKey(entityKey)

      val (entityTypeLabel, exactTypeExists) = getNEREntityType(newMention, docTokIdx)
      newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypeLabel, exactTypeExists)

      val corefEntity = coref.entityFromUniqueId(entityKey)
      corefEntity += newMention
    }

    def createNPMentionsFromParse(constituencyLabels: Array[String], docTokIdx: Int): Unit = {
      val bracketOpens = constituencyLabels(0)
      val bracketCloses = if (constituencyLabels.length > 1) constituencyLabels(1) else ""
      for (nonTerminal <- bracketOpens.split("\\(").drop(1)) {
        parseStack.push((nonTerminal, docTokIdx))
        currParseTree.addChild(nonTerminal, docTokIdx)
      }
      for (close <- bracketCloses) {
        val (phrase, start) = parseStack.pop()
        val parentPhrase = if (parseStack.nonEmpty) parseStack(0)._1 else ""
        currParseTree.current.setEnd(docTokIdx)
        if (phrase == "NP") {
          val span = new TokenSpan(coref.document.asSection, start, docTokIdx - start + 1)
          val newMention = coref.addMention(new Phrase(span, -1))
          numMentions += 1
          currParseTree.closeLabel(docTokIdx)

          val entityChunkForMention = mentionBoundaries.getOrElse(newMention.phrase.value, new CoreferentEntityChunk(coref.document.name + "-" + (-coref.mentions.size), start, true))
          //Register that we have found this mention
          entityChunkForMention.found = true
          //Assign mention to an entity cluster
          val entityKey = if (key) entityChunkForMention.entityId
                          else coref.document.name + "-" + (-coref.mentions.size)
          newMention.attr += new EntityKey(entityKey)
          val corefEntity = coref.entityFromUniqueId(entityKey)
          corefEntity += newMention
          //Set OntoNotesEntityTYpe
          val (entityTypeLabel, exactTypeExists) = getNEREntityType(newMention, docTokIdx)
          newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypeLabel, exactTypeExists)
        } else currParseTree.closeLabel(docTokIdx)
        prevPhrase = phrase
      }
    }

    def storeCorefEntityChunk(entityLabel: String, sentence: Sentence, docTokIdx: Int, docId: String): Unit = {
      val entityLabels = entityLabel.split('|').map(_.trim)
      for (label <- entityLabels) {
        val corefTags = tokenizeEntityType(label).filterNot(l => l.isEmpty)
        corefTags match {
          case Array("(", entityId, ")") => mentionBoundaries.put(new Span[Section,Token](sentence.section, docTokIdx, 1), new CoreferentEntityChunk(docId + "-*" + entityId, docTokIdx))
          case Array("(", entityId) => if (openEntityStack.contains(entityId))
            openEntityStack.get(entityId).get.push(new CoreferentEntityChunk(docId + "-*" + entityId, docTokIdx))
          else
            openEntityStack.put(entityId, new mutable.Stack[CoreferentEntityChunk]().push(CoreferentEntityChunk(docId + "-*" + entityId, docTokIdx)))
          case Array(entityId, ")") => {
            val lastOpenedEntityStack = openEntityStack.get(entityId).get
            val lastOpenedEntity = lastOpenedEntityStack.pop()
            mentionBoundaries.put(new TokenSpan(sentence.section, lastOpenedEntity.mentionStart, docTokIdx - lastOpenedEntity.mentionStart + 1).value, lastOpenedEntity)
          }
          case _ =>
        }
      }
    }

    def getNEREntityType(mention: Mention, docTokIdx: Int): (String, Boolean) = {
      var (entityTypeLabel, exactMatchExists) = ("O", false)
      //Find all entity type spans within the given mention
      val entityTypesForSpan = _spanToEntityType.filterKeys(mention.phrase.value.contains)
      if (entityTypesForSpan.nonEmpty && useEntityType) {
        val exactMatch = entityTypesForSpan.find(entitySpan => (entitySpan._1.start == mention.phrase.start) && (entitySpan._1.end == docTokIdx))
        exactMatchExists = exactMatch.isDefined
        if (exactMatch.isDefined) {
          entityTypeLabel = exactMatch.get._2
        }
        else if (!useExactEntTypeMatch) {
          val headEntityType = entityTypesForSpan.find(s => s._1.exists(t => t == mention.phrase.headToken))
          if (headEntityType.isDefined) entityTypeLabel = headEntityType.get._2
        }
      }
      (entityTypeLabel, exactMatchExists)
    }

    def resolveAnnotatedMentions(docTokIdx: Int) {
      for ((goldMentionSpan, goldMentionEntityInfo) <- mentionBoundaries.filter { case (mentionSpan, mentionEntityInfo) => !mentionEntityInfo.found}) {
        //assert(currParseTree.current.parent.start == start,"Not in Parent")
        val newMention = coref.addMention(new Phrase(coref.document.asSection, goldMentionSpan.start, goldMentionSpan.length, -1))
        //Find and add an OntonotesPhraseEntityType
        val (entityTypeLabel, exactTypeExists) = getNEREntityType(newMention, docTokIdx)
        newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypeLabel, exactTypeExists)
        numMentions += 1
        //Assign mention to entity
        val entityChunkForMention = mentionBoundaries.getOrElse(newMention.phrase.value, new CoreferentEntityChunk(coref.document.name + "-" + coref.mentions.size + 1, goldMentionSpan.start, true))
        entityChunkForMention.found = true
        val entityKey =
          if (key) entityChunkForMention.entityId
          else coref.document.name + "-" + coref.mentions.size + 1
        newMention.attr += new EntityKey(entityKey)
        val corefEntity = coref.entityFromUniqueId(entityKey)
        corefEntity += newMention
      }
    }
  }

  def hasNext = goldDocs.hasNext
}

protected class ConllOWPLIterator(file:String) extends Iterator[(String, Iterator[String])] {
  private val source = scala.io.Source.fromFile(file).getLines()

  private val DocStart = """#begin document \(([^)]+)\).*""".r
  private val DocEnd = """#end document.*""".r

  private var currentLine = source.next()

  def next() = {
    assert(DocStart.findFirstMatchIn(currentLine).isDefined, "Failed to find start of document")
    val DocStart(id) = currentLine
    val fullId = id+"-"+currentLine.takeRight(3)
    val lines = mutable.ArrayBuffer[String]()
    while(!DocEnd.findFirstMatchIn(currentLine).isDefined && source.hasNext) {
      currentLine = source.next()
      lines += currentLine
    }
    while(!DocStart.findFirstMatchIn(currentLine).isDefined && source.hasNext) currentLine = source.next()
    fullId -> lines.toIterator
  }

  def hasNext = {
    val res = source.hasNext
    res
  }
}

case class MentionSpeaker(name: String)