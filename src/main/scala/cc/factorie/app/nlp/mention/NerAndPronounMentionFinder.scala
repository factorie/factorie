package cc.factorie.app.nlp.mention

import cc.factorie.app.nlp.{TokenSpan, Document, Token, DocumentAnnotator}
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.app.nlp.ner.{NerSpan, NerLabel}
import scala.collection.mutable.ArrayBuffer

/**
 * User: apassos
 * Date: 8/6/13
 * Time: 3:34 PM
 */
object NerAndPronounMentionFinder extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[NerLabel], classOf[PTBPosLabel])
  def postAttrs = Seq(classOf[MentionList])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.span.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+m.span.indexOf(token)).mkString(","); case _ => "_" }

  def getNerSpans(doc: Document): Seq[TokenSpan] = {
    val spans = ArrayBuffer[TokenSpan]()
    for (s <- doc.sections;t <- s.tokens) {
      if (t.attr[NerLabel].categoryValue != "O") {
        val attr = t.attr[NerLabel].categoryValue.split("-")
        if (attr(0) == "U") {
          spans += new TokenSpan(s, t.positionInSection, 1)(null)
        } else if (attr(0) == "B") {
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.attr[NerLabel].categoryValue.matches("(I|L)-" + attr(1))) lookFor = lookFor.next
            spans += new TokenSpan(s, t.positionInSection, lookFor.positionInSection - t.positionInSection)(null)
          } else {
            spans += new TokenSpan(s, t.positionInSection, 1)(null)
          }
        }
      }
    }
    spans.toSeq
  }

  def getPronounSpans(doc: Document): Seq[TokenSpan] = {
    doc.tokens.filter(_.posLabel.isPersonalPronoun).map(t => new TokenSpan(t.section, t.positionInSection, 1)).toSeq
  }


  def process1(document: Document) = {
    val nerMentions = getNerSpans(document).map(s => {
      val m = new Mention(s, s.length-1)
      m.attr += new MentionType(m, "NAM")
      m
    })
    val pronounMentions = getPronounSpans(document).map(s => {
      val m = new Mention(s, 0)
      m.attr += new MentionType(m, "PRO")
      m
    })
    document.attr += new MentionList() ++= (nerMentions ++ pronounMentions).sortBy(m => m.span.end)
    document
  }
}
