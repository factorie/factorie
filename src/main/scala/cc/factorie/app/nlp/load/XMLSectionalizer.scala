package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.load.TACDocTypes._
import cc.factorie.app.nlp.{Document, DocumentAnnotator, Section, Token}

import scala.collection.mutable

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
  sealed trait State
  case object Usable extends State
  case object Unusable extends State

  val acceptedOpenTag = ("""(?i)< *(""" + boundaryToken + """)[^\n>]*?>""").r
  val acceptedCloseTag = ("""(?i)</ *(""" + boundaryToken + """) *>""").r

  val excludedOpenTag = ("""(?i)< *(""" + excludeTokens.mkString("|") + """)[^\n>]*?>""").r
  val excludedCloseTag = ("""(?i)</ *(""" + excludeTokens.mkString("|") + """) *>""").r

  def tokenAnnotationString(token: Token) = null

  val prereqAttrs = Seq(classOf[Token])
  val postAttrs = Seq(classOf[TACSection])


  def process(document:Document) = {
    val tagStack = mutable.Stack[String]()
    val stateStack = mutable.Stack[State]()
    stateStack push Unusable
    val sectionBuffer = mutable.ArrayBuffer[TACSection]()
    val tokenBuffer = mutable.ArrayBuffer[Token]()
    document.tokens.foreach { t =>
      (t.string, stateStack.top) match {
        case (acceptedOpenTag(tag), Unusable) =>
          tokenBuffer += t
          tagStack push tag.asInstanceOf[String]
          sectionBuffer += new UnusableText(tokenBuffer)
          tokenBuffer.clear()
          stateStack push Usable
        case (acceptedCloseTag(tag), Usable) if tagStack.headOption == Some(tag.asInstanceOf[String]) =>
          tagStack.pop()
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          stateStack.pop()
          tokenBuffer += t
        case (excludedOpenTag(tag), Usable) =>
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          tokenBuffer += t
          stateStack push Unusable
        case (excludedOpenTag(tag), Unusable) =>
          tokenBuffer += t
          stateStack push Unusable
        case (excludedCloseTag(tag), Unusable) if tagStack.headOption == Some(tag.asInstanceOf[String]) =>
          tagStack.pop()
          sectionBuffer += new UnusableText(tokenBuffer)
          tokenBuffer.clear()
          stateStack.pop()
        case (acceptedCloseTag(tag), Unusable) =>
          // we are in this state because we found an excluded open tag without a corresponding close tag.
          // In that event we just read in everything as usable text
          if(tokenBuffer.nonEmpty) {
            sectionBuffer += new UsableText(tokenBuffer)
            tokenBuffer.clear()
          }
          tokenBuffer += t
          stateStack.pop()
        case _ =>
          tokenBuffer += t
      }
    }
    document.clearSections()
    sectionBuffer foreach document.+=
    document
  }
}

object WebTextSectionalizer extends XMLSectionalizer("post", Set("postdate", "poster", "quote"))
object ForumPostSectionalizer extends XMLSectionalizer("post", Set("quote"))
object NewswireSectionalizer extends XMLSectionalizer("text", Set.empty[String])

object TACSectionalizer extends DocumentAnnotator {
  def tokenAnnotationString(token: Token) = null

  val prereqAttrs = Seq(classOf[Token], classOf[TACDocumentType])
  val postAttrs = Seq(classOf[TACSection])

  def process(document: Document) = (document.attr[TACDocumentType] match {
    case Newswire => NewswireSectionalizer
    case DiscussionForum => ForumPostSectionalizer
    case WebDocument => WebTextSectionalizer
  }).process(document)
}
