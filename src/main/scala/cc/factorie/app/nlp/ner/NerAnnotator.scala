package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp.{Section, Token, Document, DocumentAnnotator}
import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable

/**
 * @author John Sullivan
 */
abstract class NerAnnotator[Span <: NerSpan : ClassTag, Tag <: NerTag : ClassTag] extends DocumentAnnotator with Serializable {
  def tokenAnnotationString(token: Token) = token.attr.exactly[Tag].categoryValue

  val postAttrs = Seq(classTag[Tag].runtimeClass, classTag[Span].runtimeClass)
  var createChunks = true

  /** This class should annotate all of the tokens in the document with the appropriate NER tag */
  def annotateTokens(document:Document):Document

  def newBuffer:NerSpanBuffer[Span]
  def newSpan(sec:Section, start:Int, length:Int, category:String):Span

  def process(document: Document) = {
    sealed trait State
    case class Reading(tag:String) extends State
    case object NotReading extends State

    val doc = annotateTokens(document)
    if(createChunks) {
      val spanBuffer = newBuffer
      doc.sections.foreach { sec =>
        val iter = sec.tokens.iterator
        var tok:Token = null
        var state:State = NotReading
        val tokBuffer = mutable.ArrayBuffer[Token]()
        while(iter.hasNext) {
          tok = iter.next()
          val tag = tok.attr.exactly[Tag]
          (tag.spanPrefix, state) match {
            case ("O", NotReading) => ()
            case ("U", NotReading) => spanBuffer.add(newSpan(sec, tok.positionInSection, 1, tag.baseCategoryValue))(null)
            case ("B", NotReading) =>
              tokBuffer += tok
              state = Reading(tag.baseCategoryValue)
            case ("I", Reading(_)) =>
              tokBuffer += tok
            case ("O", Reading(t)) =>
              spanBuffer.add(newSpan(sec, tokBuffer.head.positionInSection, tokBuffer.size, t))(null)
              tokBuffer.clear()
              state = NotReading
            case ("L", Reading(t)) =>
              tokBuffer += tok
              spanBuffer.add(newSpan(sec, tokBuffer.head.positionInSection, tokBuffer.size, t))(null)
              tokBuffer.clear()
              state = NotReading
            case (prefix, s) =>
              println((tok.prevWindow(3) ++ Seq(tok) ++ tok.nextWindow(3)).map(t => t.string + "\t" + t.nerTag.categoryValue).mkString("\n"))
              throw new Error("Invalid combination of states, prefix %s, state %s at token %s".format(prefix, s, tok))
          }
        }
        if(tokBuffer.nonEmpty) {
          spanBuffer.add(newSpan(sec, tokBuffer.head.positionInSection, tokBuffer.size, state.asInstanceOf[Reading].tag))(null)
        }
      }
      doc.attr += spanBuffer
      doc.annotators ++= Seq(classTag[Tag].runtimeClass -> this.getClass, classTag[NerSpanBuffer[Span]].runtimeClass -> this.getClass)
    }
    doc
  }
}
