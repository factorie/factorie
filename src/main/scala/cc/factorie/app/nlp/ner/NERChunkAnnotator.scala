package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp.{Section, Token, Document, DocumentAnnotator}
import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable

import java.util.logging.{Logger, Level}

/**
 * @author John Sullivan
 */
object BilouConllNerChunkAnnotator extends NerChunkAnnotator[ConllNerSpan, BilouConllNerTag]({() => new ConllNerSpanBuffer}, {(s:Section, start:Int, end:Int, cat:String) => new ConllNerSpan(s, start, end, cat)})
object BilouOntonotesNerChunkAnnotator extends NerChunkAnnotator[OntonotesNerSpan, BilouOntonotesNerTag]({() => new OntonotesNerSpanBuffer}, {(s:Section, start:Int, end:Int, cat:String) => new OntonotesNerSpan(s, start, end, cat)})
object BioConllNerChunkAnnotator extends NerChunkAnnotator[ConllNerSpan, BioConllNerTag]({() => new ConllNerSpanBuffer}, {(s:Section, start:Int, end:Int, cat:String) => new ConllNerSpan(s, start, end, cat)})
object BioOntonotesNerChunkAnnotator extends NerChunkAnnotator[OntonotesNerSpan, BioOntonotesNerTag]({() => new OntonotesNerSpanBuffer}, {(s:Section, start:Int, end:Int, cat:String) => new OntonotesNerSpan(s, start, end, cat)})

/** Takes documents that are already annotated with token-level NerTags of type Tag and annotates them with NerSpans
  * of type Span */
class NerChunkAnnotator[Span <: NerSpan : ClassTag, Tag <: NerTag : ClassTag](newBuffer:() => NerSpanBuffer[Span], newSpan:(Section, Int, Int, String) => Span) extends DocumentAnnotator {

  val prereqAttrs = Seq(classTag[Tag].runtimeClass)
  val postAttrs = Seq(classTag[Span].runtimeClass)

  def tokenAnnotationString(token: Token) = token.attr.exactly[Tag].categoryValue

  def process(doc: Document) = {
    sealed trait State
    case class Reading(tag:String) extends State
    case object NotReading extends State

    val spanBuffer = newBuffer()
    doc.sections.foreach { sec =>
      val iter = sec.tokens.iterator
      var tok:Token = null
      var state:State = NotReading
      val tokBuffer = mutable.ArrayBuffer[Token]()
      while(iter.hasNext) {
        tok = iter.next()
        val tag = tok.attr[Tag]
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
            val prevStr = {
              val prevToken = if (tok.hasPrev) tok.prev else null
              if (prevToken != null) prevToken.attr[Tag].categoryValue else "<null>"
            }
            NerChunkAnnotator.logger.log(Level.FINE, "Invalid combination of states, prefix %s, state %s at token %s. Previous token was %s.".format(prefix, s, tok, prevStr))
        }
      }
      if (tokBuffer.nonEmpty) {
        spanBuffer.add(newSpan(sec, tokBuffer.head.positionInSection, tokBuffer.size, state.asInstanceOf[Reading].tag))(null)
      }
    }
    doc.attr += spanBuffer
    doc.annotators ++= Seq(classTag[Tag].runtimeClass -> this.getClass, classTag[NerSpanBuffer[Span]].runtimeClass -> this.getClass)
    doc
  }
}

object NerChunkAnnotator {
    private val logger : Logger = Logger.getLogger(getClass.getName)
}
