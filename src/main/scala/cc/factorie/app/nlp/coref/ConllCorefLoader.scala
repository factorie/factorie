package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosLabel}
import mention.{MentionEntityType, MentionList, Mention, Entity}
import scala.collection.mutable.{ ArrayBuffer, Map, Stack }
import scala.collection.mutable
import scala.util.control.Breaks._

class EntityKey(val name: String)

// TODO This should be moved to app.nlp.LoadConll2011 -akm
object ConllCorefLoader {

  //this is used when loading gold entity type annotation. If this variable is set to true, the loader
  // only uses the entity type if its boundaries exactly match the boundaries of the annotated mention
  val useExactEntTypeMatch = true

  // to be used with test-with-gold-mention-boundaries
  val autoFileFilter = new java.io.FileFilter() {
    override def accept(file: java.io.File): Boolean =
      file.getName.endsWith("auto_conll")
  }

  // to be used with test-key
  val goldFileFilter = new java.io.FileFilter() {
    override def accept(file: java.io.File): Boolean =
      file.getName.endsWith("gold_conll")
  }



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


  //this returns true if the noun phrase passed in shouln't be added as a mention. We use the
  //rules from 'Mention Detection: Heuristics for the OntoNotes annotations'
  def filterMention(phrase: String,parentPhrase: String,prevPhrase: String,prevWord: String): Boolean = {
    assert(phrase == "NP")
    val apposition = (prevPhrase == "NP") && (parentPhrase == "NP") && (prevWord == ",")
    val copular = (parentPhrase == "VP") && copularVerbs.contains(prevWord)  //todo: prevWord has been properly case-normalized, right?
    apposition || copular
  }

  final val copularVerbs = collection.immutable.HashSet[String]() ++ Seq("is","are","was","'m")

  // disperseEntityTypes optionally gives entity type information to all things that are coreferent with something that has entity type annotation
  def loadWithParse(f: String, loadSingletons: Boolean = true, limitNumDocuments:Int = -1, disperseEntityTypes:Boolean = false): Seq[Document] = {
    // println("loading " + f)
    val docs = ArrayBuffer[Document]()
    val tokenizer = """(\(|\||\)|\d+)""".r
    val entityTypeTokenizer = """(\(|[^\)]+|\)|)""".r
    val asteriskStripper = """\*""".r
    def tokenizeEntityType(s: String): Array[String] = {
      entityTypeTokenizer.findAllIn(s).map(x => asteriskStripper.replaceAllIn(x,"")).toArray
    }

    var currDoc: Document = null
    var currSent: Sentence = null
    var currEntId: Int = 0
    var docTokInd: Int = -1
    val mentions = Map[String, Stack[Int]]() // map of (entityId, tokenIndex) of unfinished mentions in a sentence
    var numMentions = 0 // total number mentions in a document
    val entities = Map[String, Entity]()
    val startMap = mutable.HashMap[String,Stack[Int]]()
    var sentenceId: Int = -1
    var tokenId: Int = -1
    var numNegEntities = 1
    val parseStack = collection.mutable.Stack[(String,Int)]()
    val source = scala.io.Source.fromFile(f)
    var prevPhrase = ""
    var prevWord = ""
    var currentlyInEntityTypeBracket = false
    var currentEntityTypeStr = ""
    var entityTypeStart = -1
    var currentlyUnresolvedClosedEntityTypeBracket = false
    var exactTypeMatches = 0
    var totalTypeCounts  = 0

    breakable { for (l <- source.getLines()) {
      if (l.startsWith("#begin document ")) {
        if (docs.length == limitNumDocuments) break()
        val fId = l.split("[()]")(1) + "-" + l.takeRight(3)
        currDoc = new Document("").setName(fId)
        currDoc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
        currDoc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
        //currDoc.attr += new FileIdentifier(fId, true, fId.split("/")(0), "CoNLL")
        docs += currDoc
        currDoc.attr += new MentionList
      } else if (l.startsWith("#end document")) {
        currDoc = null
        currEntId = 0
        mentions.clear()
        entities.clear()
        parseStack.clear()
        startMap.clear()
        docTokInd = -1
        sentenceId = -1
        tokenId = -1
        assert(!currentlyInEntityTypeBracket)
      } else if (l.length == 0) {
        currDoc.appendString("\n")
        parseStack.clear()
        currSent = null
      } else {
        docTokInd += 1
        val fields = l.split("\\s+")
        val tokId = fields(2).toInt
        val word = unescapeBrackets(fields(3))
        currDoc.appendString(" ")
        if (tokId == 0) {
          currSent = new Sentence(currDoc)
          prevPhrase = ""
          prevWord = ""
        }
        val token = new Token(currSent, word)
        PennPosDomain.unfreeze()     //todo: factorie PennPosDomain currently contains all of the ontonotes tags. Might want to freeze this up for thread safety
        token.attr += new PennPosLabel(token,fields(4))
        tokenId += 1
        if (tokId == 0) sentenceId += 1

        //parse the info for entity types
        var thisTokenClosedTheEntityType = false
        val entityTypeTokens = tokenizeEntityType(fields(10))

        for(i<- 0 until entityTypeTokens.length){
          val t = entityTypeTokens(i)
          if(t == "("){
            entityTypeStart = docTokInd
            currentEntityTypeStr = entityTypeTokens(i+1)
            assert(!currentlyInEntityTypeBracket) //this makes sure that the data doesn't have nested entity types
            currentlyInEntityTypeBracket = true
          }
          if(t == ")"){
            thisTokenClosedTheEntityType = true
            currentlyInEntityTypeBracket = false
            currentlyUnresolvedClosedEntityTypeBracket = true
          }
        }



        val closedEntities = ArrayBuffer[(String,Int)]()
        val entityTokens = tokenizer.findAllIn(fields.last).toArray
        for (i <- 0 until entityTokens.length) {
          val t = entityTokens(i)
          if (t == "(") {
            val number = entityTokens(i+1)
            startMap.getOrElseUpdate(number, Stack[Int]()).push(docTokInd)
          } else if (t == ")") {
            val number = entityTokens(i-1)
            val start = startMap.getOrElseUpdate(number, Stack[Int]()).pop()
            closedEntities.append((number,start))
            // println("Adding entity " + number + " " + start + " " + docTokInd)
          }
        }

        val foo = fields(5).split("\\*")
        if (foo.length >= 1 && loadSingletons) {
          val bracketOpens = foo(0)
          val bracketCloses = if (foo.length > 1) foo(1) else ""
          for (nonTerminal <- bracketOpens.split("\\(").drop(1)) {
            parseStack.push((nonTerminal, docTokInd))
          }
          for (close <- bracketCloses) {
            val (phrase, start) = parseStack.pop()
            val parentPhrase = if(!parseStack.isEmpty) parseStack(0)._1 else ""
            if (phrase == "NP" && !filterMention(phrase,parentPhrase,prevPhrase,prevWord)) {
              val span = new TokenSpan(currDoc.asSection, start, docTokInd - start + 1)
              val m = new Mention(span, getHeadToken(span))
              currDoc.attr[MentionList] += m
              numMentions += 1

              if(currentlyUnresolvedClosedEntityTypeBracket && (entityTypeStart >= start)){
                val exactMatch = (entityTypeStart == start) && thisTokenClosedTheEntityType
                if(!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatch)){
                  m.attr += new MentionEntityType(m,currentEntityTypeStr)
                }else{
                  m.attr += new MentionEntityType(m,"O")
                }
                currentlyUnresolvedClosedEntityTypeBracket = false
              }else
                m.attr += new MentionEntityType(m,"O")

              var i = 0
              var found = false
              while (i < closedEntities.length) {
                closedEntities(i) match {
                  case (number, entStart) =>
                    if (entStart == start) {
                      found = true
                      val key = fields(0) + "-" + number
                      m.attr += new EntityKey(key)
                      val ent = entities.getOrElseUpdate(key, new Entity(word))
                      m.attr += ent
                      closedEntities(i) = null
                    }
                  case _ =>
                }
                i += 1
              }
              if (!found) {
                numNegEntities += 1
                val key = fields(0) + "-" + (-numNegEntities)
                m.attr += new EntityKey(key)
                val ent = entities.getOrElseUpdate(key, new Entity(word))
                m.attr += ent
              }
            }
            prevPhrase = phrase
          }
        }
        //this makes mentions for the ground truth mentions that weren't NPs
        for ((number,start) <- closedEntities.filter(i =>  i ne null)) {
          val span = new TokenSpan(currDoc.asSection, start, docTokInd - start + 1)
          val m = new Mention(span, getHeadToken(span))
          currDoc.attr[MentionList] += m
          if(currentlyUnresolvedClosedEntityTypeBracket && (entityTypeStart >= start)){
            val exactMatch = (entityTypeStart == start) && thisTokenClosedTheEntityType
            if(!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatch)){
              m.attr += new MentionEntityType(m,currentEntityTypeStr)
            }else
              m.attr += new MentionEntityType(m,"O")
            currentlyUnresolvedClosedEntityTypeBracket = false
          }else
            m.attr += new MentionEntityType(m,"O")

          numMentions += 1
          val key = fields(0) + "-" + number
          m.attr += new EntityKey(key)
          val ent = entities.getOrElseUpdate(key, new Entity(word))
          m.attr += ent
        }
        prevWord = word
      }

    }} // closing "breakable"
    if(disperseEntityTypes){
      for(doc <- docs){
        val entities = doc.attr[MentionList].groupBy(m => m.attr[Entity]).filter(x => x._2.length > 1)
        for(ent <- entities){
          val entityTypes = ent._2.map(m => m.attr[MentionEntityType].categoryValue).filter(t => t != "O").distinct
          if(entityTypes.length > 1){
           // println("warning: there were coreferent mentions with different annotated entity types: " + entityTypes.mkString(" ") + "\n" + ent._2.map(m => m.span.string).mkString(" "))
          }else if(entityTypes.length == 1){
            val newType = entityTypes(0)
            ent._2.foreach(m => m.attr[MentionEntityType].target.setCategory(newType)(null))
          }

        }
      }
    }
    source.close()
    docs
  }

  //this is a span-level offset. Since we don't have a dep parse, we just take the final noun in the span
  def getHeadToken(span: TokenSpan): Int = {
    val toReturn = span.value.lastIndexWhere(_.posLabel.categoryValue.startsWith("NN"))
    if(toReturn == -1){
      span.length - 1   //todo: is it really true that sometimes in the annotation, annotated mentions won't have a noun in them
    }else{
      toReturn
    }
  }

}



