/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
  def loadWithParse(f: String, loadSingletons: Boolean = true, limitNumDocuments:Int = -1, callDisperseEntityTypes:Boolean = false): Seq[Document] = {
    // println("loading " + f)
    val docs = ArrayBuffer[Document]()

    var coref: WithinDocCoref = null
    var currDoc: Document = null
    var currSent: Sentence = null
    var currEntId: Int = 0
    var docTokInd: Int = -1
    var numMentions = 0 // total number mentions in a document
    val entities = Map[String, WithinDocEntity]()
    var sentenceId: Int = -1
    var tokenId: Int = -1

    val parseStack = collection.mutable.Stack[(String,Int)]()
    var currParseTree:ConstituencyParse = null

    val source = scala.io.Source.fromFile(f)
    var prevPhrase = ""
    var prevWord = ""

    val goldMentionBoundaries = new scala.collection.mutable.LinkedHashMap[Span[Section,Token],CoreferentEntityChunk]
    val _spanToEntityType = new scala.collection.mutable.LinkedHashMap[Span[Section,Token],String]
    var unResolvedEntityType:EntityTypeChunk = null

    val openEntityStack = mutable.Stack[CoreferentEntityChunk]()

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

        _spanToEntityType.clear()
        goldMentionBoundaries.clear()
        openEntityStack.clear()
        entities.clear()
        parseStack.clear()
        docTokInd = -1
        sentenceId = -1
        tokenId = -1

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
          prevPhrase = ""
          prevWord = ""
        }
        val token = new Token(currSent, word)
        PennPosDomain.unfreeze()     //todo: factorie PennPosDomain currently contains all of the ontonotes tags. Might want to freeze this up for thread safety
        token.attr += new PennPosTag(token,fields(4))
        tokenId += 1
        if (tokId == 0) sentenceId += 1

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

        val entityLabels = fields.last.split('|').map(_.trim)
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
              val newMention = coref.addMention(new Phrase(span, span.tokens.indexOf(currParseTree.current.getHeadToken(docTokInd))))
              numMentions += 1
              currParseTree.closeLabel(docTokInd)

              val entityTypesForSpan = _spanToEntityType.filterKeys(span.value.contains)
              if(!entityTypesForSpan.isEmpty){
                val exactMatch = entityTypesForSpan.find(entitySpan => (entitySpan._1.start == start) && (entitySpan._1.end == docTokInd) )
                val exactMatchExists = exactMatch ne null
                if (!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatchExists))
                  newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypesForSpan.find(s => s._1.exists(t=> t == newMention.phrase.headToken)).getOrElse(entityTypesForSpan.head)._2,exactMatchExists)
                else
                  newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O",exactMatchExists)
              } else
                newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O")

              val entityChunkForMention = goldMentionBoundaries.getOrElse(newMention.phrase.value,new CoreferentEntityChunk(fields(0)+"-"+(-coref.mentions.size),start,true))
              //Register that we have found this mention
              entityChunkForMention.found = true
              newMention.attr += new EntityKey(entityChunkForMention.entityId)
              val corefEntity = entities.getOrElseUpdate(entityChunkForMention.entityId,coref.entityFromUniqueId(entityChunkForMention.entityId))
              corefEntity += newMention
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
            val exactMatch = entityTypesForSpan.getOrElse(span.value,null)//.find(entitySpan => (entitySpan._1.start == start) && (entitySpan._1.end == docTokInd) )
            val exactMatchExists = exactMatch ne null
            if (!useExactEntTypeMatch ||(useExactEntTypeMatch && exactMatchExists))
              newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, entityTypesForSpan.find(s => s._1.exists(t=> t == newMention.phrase.headToken)).getOrElse(entityTypesForSpan.head)._2,exactMatchExists)
            else
              newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O",exactMatchExists)
          } else
            newMention.phrase.attr += new OntonotesPhraseEntityType(newMention.phrase, "O")

          numMentions += 1

          val entityChunkForMention = goldMentionBoundaries.getOrElse(newMention.phrase.value,new CoreferentEntityChunk(fields(0)+"-"+coref.mentions.size+1,goldMentionSpan.start,true))
          entityChunkForMention.found = true
          newMention.attr += new EntityKey(entityChunkForMention.entityId)
          val corefEntity = entities.getOrElseUpdate(entityChunkForMention.entityId,coref.entityFromUniqueId(entityChunkForMention.entityId))
          corefEntity += newMention

        }
        prevWord = word
      }

    }} // closing "breakable"
    if (callDisperseEntityTypes) disperseEntityTypes(docs.map(_.getTargetCoref))
    source.close()
    docs
  }

  case class CoreferentEntityChunk(entityId:String,mentionStart:Int,var found:Boolean = false)
  case class EntityTypeChunk(entityType:String, start:Int)

  def disperseEntityTypes(corefDocs:Seq[WithinDocCoref]):Unit = {
    for(corefDoc <- corefDocs){
      val entities = corefDoc.mentions.toSeq.groupBy(m => m.entity).filter(x => x._2.length > 1)
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

  /**This is a span-level offset. Since we don't have a dep parse, we just take the final noun in the span */
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
      if(possNP.isDefined && possNP.get != span.last && possNP.get.next.posTag.categoryValue.startsWith("N")) {
        return possNP.get.next
      }
      else if(!childNP.isEmpty) childNP.head.getHeadToken(docTokInd)
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




