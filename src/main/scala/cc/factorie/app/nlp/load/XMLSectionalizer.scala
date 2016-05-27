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

import cc.factorie.app.nlp.segment.IsSgmlTag
import cc.factorie.app.nlp.{Section, Token, Document, DocumentAnnotator}
import scala.collection.mutable
import cc.factorie.app.nlp.load.TACDocTypes._

/** The token span is assumed to be contiguous */
abstract class TACSection(tks:Iterable[Token]) extends Section {
  val document:Document = tks.head.document
  val stringStart:Int = tks.head.stringStart
  val stringEnd:Int = tks.last.stringEnd
  // this needs to go after the definition of document because of the wonky way
  // that token.document and section.document interact.
  tks foreach this.+=
}
class UsableText(tokens:Iterable[Token]) extends TACSection(tokens)
class UnusableText(tokens:Iterable[Token]) extends TACSection(tokens)

/** A document annotator that creates [[UsableText]] sections for texts within boundaryToken that
  * are not within excludeTokens. Everything else goes in [[UnusableText]] sections. */
class XMLSectionalizer(boundaryToken:String, excludeTokens:Set[String]) extends DocumentAnnotator {

  // we want a regex that will never match anything in a text document, visual bell is a good bet
  private val nullRegex = 7.toChar.toString.r

  sealed trait State
  case object Usable extends State
  case object Unusable extends State

  val acceptedOpenTag = ("""(?i)< *(""" + boundaryToken + """)[^\n>]*?>""").r
  val acceptedCloseTag = ("""(?i)</ *(""" + boundaryToken + """) *>""").r

  val excludedOpenTag = if (excludeTokens.isEmpty) nullRegex  else ("""(?i)< *(""" + excludeTokens.mkString("|") + """)[^\n>]*?>""").r
  val excludedCloseTag = if(excludeTokens.isEmpty) nullRegex else ("""(?i)</ *(""" + excludeTokens.mkString("|") + """) *>""").r

  def tokenAnnotationString(token: Token) = null

  val prereqAttrs = Seq(classOf[Token])
  val postAttrs = Seq(classOf[TACSection])


  def process(document:Document) = {
    val tagStack = mutable.Stack[String]()
    val stateStack = mutable.Stack[State]()
    stateStack push Unusable
    val sectionBuffer = mutable.ArrayBuffer[TACSection]()
    val tokenBuffer = mutable.ArrayBuffer[Token]()

    def addToken(t: Token) = if(!t.attr.contains[IsSgmlTag.type]) tokenBuffer += t

    document.tokens.foreach { t =>
      (t.string, stateStack.top) match {
        case (acceptedOpenTag(tag), Unusable) =>
          addToken(t)
          tagStack push tag.asInstanceOf[String]
          if (tokenBuffer.nonEmpty) {
            sectionBuffer += new UnusableText(tokenBuffer)
            tokenBuffer.clear()
          }
          stateStack push Usable
        case (acceptedCloseTag(tag), Usable) if tagStack.headOption == Some(tag.asInstanceOf[String]) =>
          tagStack.pop()
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          stateStack.pop()
          addToken(t)
        case (excludedOpenTag(tag), Usable) =>
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          addToken(t)
          stateStack push Unusable
        case (excludedOpenTag(tag), Unusable) =>
          addToken(t)
          stateStack push Unusable
        case (excludedCloseTag(tag), Unusable) if tagStack.headOption == Some(tag.asInstanceOf[String]) =>
          tagStack.pop()
          if (tokenBuffer.nonEmpty) {
            sectionBuffer += new UnusableText(tokenBuffer)
            tokenBuffer.clear()
          }
          stateStack.pop()
        case (acceptedCloseTag(tag), Unusable) =>
          // we are in this state because we found an excluded open tag without a corresponding close tag.
          // In that event we just read in everything as usable text
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          addToken(t)
          stateStack.pop()
        case _ =>
          addToken(t)
      }
    }
    document.clearSections()
    sectionBuffer foreach document.+=
    document.annotators += classOf[TACSection] -> this.getClass
    document
  }
}

object WebTextSectionalizer extends XMLSectionalizer("post", Set("postdate", "poster", "quote"))
object ForumPostSectionalizer extends XMLSectionalizer("post", Set("quote"))
object NewswireSectionalizer extends XMLSectionalizer("text", Set.empty[String])
object BroadcastSectionalizer extends XMLSectionalizer("text",Set("speaker"))


object TACSectionalizer extends DocumentAnnotator {
  def tokenAnnotationString(token: Token) = null

  val prereqAttrs = Seq(classOf[Token], classOf[TACDocumentType])
  val postAttrs = Seq(classOf[TACSection])

  def process(document: Document) = (document.attr[TACDocumentType] match {
    case Newswire => NewswireSectionalizer
    case DiscussionForum => ForumPostSectionalizer
    case WebDocument => WebTextSectionalizer
    case Broadcast => BroadcastSectionalizer
  }).process(document)
}