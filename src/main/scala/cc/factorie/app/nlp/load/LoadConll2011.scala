package cc.factorie.app.nlp.load

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosTag}
import cc.factorie.variable.Span


//import cc.factorie.app.nlp.coref.mention.{MentionEntityType, MentionList, Mention, Entity}
import cc.factorie.app.nlp.coref._ //{Mention,Mention,MentionList,Entity}
import cc.factorie.app.nlp.phrase.{OntonotesEntityType, Phrase, OntonotesPhraseEntityType}
import scala.collection.mutable.{ListBuffer, ArrayBuffer, Map, Stack}
import scala.collection.mutable
import scala.util.control.Breaks._

class EntityKey(val name: String)

object LoadConll2011 {

  //this is used when loading gold entity type annotation. If this variable is set to true, the loader
  // only uses the entity type if its boundaries exactly match the boundaries of the annotated mention
  val useExactEntTypeMatch = false

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

  //(15|(43
  final val copularVerbs = collection.immutable.HashSet[String]() ++ Seq("is","are","was","'m")
  //val openEntity = """\( (\d+)""".r
  val singleLineEntity = """"""
  val tokenizer = """(\(|\||\)|\d+)""".r
  val entityTypeTokenizer = """(\(|[^\)]+|\)|)""".r

  //val corefEntityTokenizer = """(\(|[^\)]+|\)|)""".r


  val asteriskStripper = """\*""".r



  private def tokenizeEntityType(s: String): Array[String] = {
    entityTypeTokenizer.findAllIn(s).map(x => asteriskStripper.replaceAllIn(x,"")).map(_.toString).toArray
  }




  // disperseEntityTypes optionally gives entity type information to all things that are coreferent with something that has entity type annotation
  //2 Documents in Train: 161.5 mentions/doc
  def loadWithParse(f: String, loadSingletons: Boolean = true, limitNumDocuments:Int = -1, disperseEntityTypes:Boolean = false): Seq[Document] = {
    // println("loading " + f)
    val docs = ArrayBuffer[Document]()

    var coref: WithinDocCoref = null
    var currDoc: Document = null
    var currSent: Sentence = null
    var currEntId: Int = 0
    var docTokInd: Int = -1
    val mentions = Map[String, Stack[Int]]() // map of (entityId, tokenIndex) of unfinished mentions in a sentence
    var numMentions = 0 // total number mentions in a document
    val entities = Map[String, WithinDocEntity]()
    val startMap = mutable.HashMap[String,Stack[Int]]()
    var sentenceId: Int = -1
    var tokenId: Int = -1
    var numNegEntities = 1

    val parseStack = collection.mutable.Stack[(String,Int)]()
    var parseTrees = new ArrayBuffer[ConstituencyParse]()
    var currParseTree:ConstituencyParse = null

    val source = scala.io.Source.fromFile(f)
    var prevPhrase = ""
    var prevWord = ""

    val goldMentionBoundaries = new scala.collection.mutable.LinkedHashMap[Span[Section,Token],CoreferentEntityChunk]

    val _spanToEntityType = new scala.collection.mutable.LinkedHashMap[Span[Section,Token],String]
    var unResolvedEntityType:EntityTypeChunk = null

    val openEntityStack = mutable.Stack[CoreferentEntityChunk]()

    val currentlyInEntityTypeBracket = false  //Todo: I want to remove these


    breakable { for (l <- source.getLines()) {
      if (l.startsWith("#begin document ")) {
        if (docs.length == limitNumDocuments) break()
        val fId = l.split("[()]")(1) + "-" + l.takeRight(3)
        currDoc = new Document("").setName(fId)
        currDoc.getCoref
        coref = currDoc.getTargetCoref // This also puts a newly created WithinDocCoref in currDoc.attr.
        currDoc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
        currDoc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
        //currDoc.attr += new FileIdentifier(fId, true, fId.split("/")(0), "CoNLL")
        docs += currDoc
      } else if (l.startsWith("#end document")) {
        coref = null
        currDoc = null
        currEntId = 0
        mentions.clear()
        _spanToEntityType.clear()
        goldMentionBoundaries.clear()
        openEntityStack.clear()
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
          currParseTree = new ConstituencyParse(currSent,0,"TOP")
          parseTrees += currParseTree
          prevPhrase = ""
          prevWord = ""
        }
        val token = new Token(currSent, word)
        PennPosDomain.unfreeze()     //todo: factorie PennPosDomain currently contains all of the ontonotes tags. Might want to freeze this up for thread safety
        token.attr += new PennPosTag(token,fields(4))
        tokenId += 1
        if (tokId == 0) sentenceId += 1

        //resolveEntityType(fields(10),docTokInd,currentlyInEntityTypeBracket)

        val entityTypeTokens = tokenizeEntityType(fields(10)).filterNot(_.isEmpty)
        entityTypeTokens match {
          case Array("(",entityTypeString:String,")") => _spanToEntityType.put(new TokenSpan(currSent.section,docTokInd,1).value,entityTypeString)   //todo:Don't forget to change this to new span
          case Array("(",entityTypeString) =>
            assert(unResolvedEntityType eq null,"Nested Entity Types Found")
            unResolvedEntityType = new EntityTypeChunk(entityTypeString,docTokInd)
          case Array(")") =>
            _spanToEntityType.put(new TokenSpan(currSent.section,unResolvedEntityType.start,docTokInd-unResolvedEntityType.start+1).value,unResolvedEntityType.entityType)
            unResolvedEntityType = null
          case _ =>
        }


        /*
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
        }*/
        //parse the info for entity types
        //var thisTokenClosedTheEntityType = false






        //val closedEntities = ArrayBuffer[(String,Int)]()
        val entityLabels = fields.last.split('|').map(_.trim)
        //println(entityLabels.mkString(" "))
        for(label <- entityLabels){
          val corefTags = tokenizeEntityType(label).filterNot(l => l.isEmpty)
          corefTags match {
            case Array("(",entityId,")") => goldMentionBoundaries.put(new Span(currSent.section,docTokInd,1),new CoreferentEntityChunk(fields(0)+"-*"+entityId,docTokInd))
            case Array("(",entityId) => openEntityStack.push(new CoreferentEntityChunk(fields(0)+"-*"+entityId,docTokInd))
            case Array(entityId,")") =>
              val lastOpenedEntity = openEntityStack.pop()
              goldMentionBoundaries.put(new TokenSpan(currSent.section,lastOpenedEntity.mentionStart,docTokInd - lastOpenedEntity.mentionStart + 1).value,lastOpenedEntity)
            case _ =>
          }
        }
        //val entityTokens = tokenizer.findAllIn(fields.last).toArray
        //for (i <- 0 until entityTokens.length) {
        //  val t = entityTokens(i)
          //t match {
          //  case
          //}
        //  if (t == "(") {
        //    val number = entityTokens(i+1)
        //    startMap.getOrElseUpdate(number, Stack[Int]()).push(docTokInd)
        //  } else if (t == ")") {
        //    val number = entityTokens(i-1)
        //    val start = startMap.getOrElseUpdate(number, Stack[Int]()).pop()
        //    closedEntities.append((number,start))
            // println("Adding entity " + number + " " + start + " " + docTokInd)
        //  }
        //}

        val constituencyLabels = fields(5).split("\\*")
        if (constituencyLabels.length >= 1 && loadSingletons) {
          val bracketOpens  = constituencyLabels(0)
          val bracketCloses = if (constituencyLabels.length > 1) constituencyLabels(1) else ""
          for (nonTerminal <- bracketOpens.split("\\(").drop(1)) {
            parseStack.push((nonTerminal, docTokInd))
            currParseTree.addChild(nonTerminal,docTokInd)
          }
          for (close <- bracketCloses) {
            val (phrase, start) = parseStack.pop()
            val parentPhrase = if(!parseStack.isEmpty) parseStack(0)._1 else ""
            //if(Vector("NP","PRP","PP").contains(phrase))
            currParseTree.current.setEnd(docTokInd)
            if (phrase == "NP") {
              val span = new TokenSpan(currDoc.asSection, start, docTokInd - start + 1)
              //parseTree.push((phrase,new TokenSpan(currDoc.asSection, start, docTokInd - start + 1)))

              val newMention = coref.addMention(new Phrase(span, span.tokens.indexOf(currParseTree.current.getHeadToken(docTokInd))))//getHeadToken(span)))
              //println(m.string + ": " + m.phrase.headToken.string)

              numMentions += 1
              currParseTree.closeLabel(docTokInd)
              val entityTypesForSpan = _spanToEntityType.filterKeys(span.value.contains)
              if(!entityTypesForSpan.isEmpty){
              //val exactMatchEntity = entityTypesForSpan.
              //if(currentlyUnresolvedClosedEntityTypeBracket && (entityTypeStart >= start)) {
                val exactMatch = entityTypesForSpan.find(entitySpan => (entitySpan._1.start == start) && (entitySpan._1.end == docTokInd) )
                val exactMatchExists = exactMatch ne null
                if (!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatchExists))
                  newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypesForSpan.find(s => s._1.exists(t=> t == newMention.phrase.headToken)).getOrElse(entityTypesForSpan.head)._2,exactMatchExists)
                else
                  newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O",exactMatchExists)
                //currentlyUnresolvedClosedEntityTypeBracket = false
              } else
                newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O")

              //var i = 0
              //var found = false
              //val entitiesClosedThisLine = goldMentionBoundaries.filterKeys(_.end == docTokInd)
              val entityChunkForMention = goldMentionBoundaries.getOrElse(newMention.phrase.value,new CoreferentEntityChunk(fields(0)+"-"+(-coref.mentions.size),start,true))
              entityChunkForMention.found = true
              newMention.attr += new EntityKey(entityChunkForMention.entityId)
              val corefEntity = entities.getOrElseUpdate(entityChunkForMention.entityId,coref.entityFromUniqueId(entityChunkForMention.entityId))
              corefEntity += newMention
              //for(entity <- entitiesClosedThisLine) {
              /*  entity match {
                  case (entitySpan, entityId) =>
                    if (entitySpan.start == start) {
                      found = true
                      val key = fields(0) + "-*" + entityId
                      m.attr += new EntityKey(key)
                      //val ent = entities.getOrElseUpdate(key, new WithinDocEntity(currDoc))
                      val ent = entities.getOrElseUpdate(key, coref.entityFromUniqueId(key)) // TODO I'm not sure that key is right argument here. -akm 
                      ent += m
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
                //val ent = entities.getOrElseUpdate(key, new WithinDocEntity(currDoc))
                val ent = entities.getOrElseUpdate(key, coref.entityFromUniqueId(key))
                ent += m
              } */
            }else currParseTree.closeLabel(docTokInd)
            prevPhrase = phrase
          }
        }
        //this makes mentions for the ground truth mentions that weren't found by the NP, PRP Rules
        for ((goldMentionSpan,goldMentionEntityInfo) <- goldMentionBoundaries.filter{case (mentionSpan,mentionEntityInfo) =>  !mentionEntityInfo.found}) {
          //assert(currParseTree.current.parent.start == start,"Not in Parent")
          val span = new TokenSpan(currDoc.asSection, goldMentionSpan.start, goldMentionSpan.length)

          val newMention = coref.addMention(new Phrase(span, getSimpleHeadToken(span)))

          val entityTypesForSpan = _spanToEntityType.filterKeys(span.value.contains)
          if(!entityTypesForSpan.isEmpty){
            //val exactMatchEntity = entityTypesForSpan.
            //if(currentlyUnresolvedClosedEntityTypeBracket && (entityTypeStart >= start)) {
            val exactMatch = entityTypesForSpan.getOrElse(span.value,null)//.find(entitySpan => (entitySpan._1.start == start) && (entitySpan._1.end == docTokInd) )
            val exactMatchExists = exactMatch ne null
            if (!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatchExists))
              newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypesForSpan.find(s => s._1.exists(t=> t == newMention.phrase.headToken)).getOrElse(entityTypesForSpan.head)._2,exactMatchExists)
            else
              newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O",exactMatchExists)
            //currentlyUnresolvedClosedEntityTypeBracket = false
          } else
            newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O")

          /*
          if(currentlyUnresolvedClosedEntityTypeBracket && (entityTypeStart >= start)){
            val exactMatch = (entityTypeStart == start) && thisTokenClosedTheEntityType
            if(!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatch)){
              m.phrase.attr += new OntonotesPhraseEntityType(m.phrase, currentEntityTypeStr)
            }else
              m.phrase.attr += new OntonotesPhraseEntityType(m.phrase, "O")
            currentlyUnresolvedClosedEntityTypeBracket = false
          }else
            m.phrase.attr += new OntonotesPhraseEntityType(m.phrase, "O")
          */


          numMentions += 1

          val entityChunkForMention = goldMentionBoundaries.getOrElse(newMention.phrase.value,new CoreferentEntityChunk(fields(0)+"-"+coref.mentions.size+1,goldMentionSpan.start,true))
          entityChunkForMention.found = true
          newMention.attr += new EntityKey(entityChunkForMention.entityId)
          val corefEntity = entities.getOrElseUpdate(entityChunkForMention.entityId,coref.entityFromUniqueId(entityChunkForMention.entityId))
          corefEntity += newMention


          //val key = fields(0) + "-" + number
          //m.attr += new EntityKey(key)
          //val ent = entities.getOrElseUpdate(key, coref.entityFromUniqueId(key))
          //m.attr += ent
        }
        prevWord = word
      }

    }} // closing "breakable"
    if (disperseEntityTypes) {
      for(doc <- docs){
        val entities = doc.attr[MentionList].groupBy(m => m.entity).filter(x => x._2.length > 1)
        for(ent <- entities){
          val entityTypes = ent._2.map(m => m.phrase.attr[OntonotesPhraseEntityType].categoryValue).filter(t => t != "O").distinct
          if(entityTypes.length > 1){
           // println("warning: there were coreferent mentions with different annotated entity types: " + entityTypes.mkString(" ") + "\n" + ent._2.map(m => m.span.string).mkString(" "))
          }else if(entityTypes.length == 1){
            val newType = entityTypes(0)
            ent._2.foreach(m => m.phrase.attr[OntonotesPhraseEntityType].target.setCategory(newType)(null))
          }

        }
      }
    }
    source.close()
    docs
  }
  /*
  def resolveEntityType(entityTypeField:String,currTokenIdx:Int,currentlyInEntityTypeBracket:Boolean)={
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
        println("Adding entity " + number + " " + start + " " + docTokInd)
      }
    }

  } */
  case class CoreferentEntityChunk(entityId:String,mentionStart:Int,var found:Boolean = false)
  case class EntityTypeChunk(entityType:String, start:Int)
  //this is a span-level offset. Since we don't have a dep parse, we just take the final noun in the span
  def getSimpleHeadToken(span: TokenSpan): Int = {
    //val interiorNP = parseTree.current.children.find(_.label == "NP")
    val toReturn = span.value.lastIndexWhere(_.posTag.categoryValue.startsWith("NN"))
    //val allNP = span.value.filter(_.posTag.categoryValue.startsWith("NN")).map(_.string).toSeq
    if(toReturn == -1){
      span.length - 1
    }else{

      toReturn
    }

  }

}




class ConstituencyParse(val sent: Sentence,rootStart:Int,rootLabel:String){
  var current = new ConstLabel(rootLabel,rootStart)
  def addChild(label:String,start:Int) = {
    val newChild = new ConstLabel(label,start,current)
    current.children += newChild
    current = newChild
  }
  def closeLabel(end:Int){
    current.setEnd(end)
    current = current.parent
  }

  class ConstLabel(val label:String,val start:Int,parentNode:ConstLabel = null){
    val parent:ConstLabel = parentNode
    val children:ArrayBuffer[ConstLabel] = new ArrayBuffer[ConstituencyParse.this.type#ConstLabel]()
    var endIdx:Int = -1
    var span:TokenSpan = null
    def setEnd(end:Int) = {
      span = new TokenSpan(sent.section,start,end - start + 1)
      endIdx = end
    }
    def getHeadToken(docTokInd:Int):Token ={
      val childNP = children.filter(_.label == "NP")
      val possNP = span.tokens.find(_.posTag.intValue == PennPosDomain.posIndex)
      if(possNP.isDefined && possNP.get != span.last && possNP.get.posTag.categoryValue.startsWith("NN")) {
        return possNP.get.next
      }
      else if(!childNP.isEmpty) childNP.last.getHeadToken(docTokInd)
      else {
        span.value.foreach(t=>assert(t.posTag != null))
        val lastIndexOfNoun = span.value.lastIndexWhere(_.posTag.categoryValue.startsWith("NN"))
        if(lastIndexOfNoun == -1 && span!=null) {
          //println("** Head Error: " + span.string+"  "+span.last.string)
          span.last
        }
        else span.tokens(lastIndexOfNoun)
      }
    }
  }
}




