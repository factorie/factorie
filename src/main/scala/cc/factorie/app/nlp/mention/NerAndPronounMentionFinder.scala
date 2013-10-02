package cc.factorie.app.nlp.mention

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.app.nlp.ner.{NerSpan, NerLabel}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.Span

/**
 * User: apassos
 * Date: 8/6/13
 * Time: 3:34 PM
 */

class NerMentionList extends MentionList

object NerAndPronounMentionFinder extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[NerLabel], classOf[PennPosLabel])
  def postAttrs = Seq(classOf[NerMentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r
  def getNerSpans(doc: Document): Seq[(String,TokenSpan)] = {
    val spans = ArrayBuffer[(String,TokenSpan)]()
    for (s <- doc.sections;t <- s.tokens) {
      if (t.attr[NerLabel].categoryValue != "O") {
        val attr = t.attr[NerLabel].categoryValue.split("-")
        if (attr(0) == "U") {
          val lab = attr(1)
          spans += (lab -> new TokenSpan(s, t.positionInSection, 1))
        } else if (attr(0) == "B") {
          val lab = attr(1)
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.attr[NerLabel].categoryValue.matches("(I|L)-" + attr(1))) lookFor = lookFor.next
            spans += (lab -> new TokenSpan(s, t.positionInSection, lookFor.positionInSection - t.positionInSection))
          } else {
            spans += (lab -> new TokenSpan(s, t.positionInSection, 1))
          }
        }
      } else {
        if ( t.string.length > 2 && !t.containsLowerCase && upperCase.findFirstIn(t.string).nonEmpty && (t.getNext ++ t.getPrev).exists(i => i.containsLowerCase)) {
          spans += ("ORG" -> new TokenSpan(s, t.positionInSection, 1))
        } else if (t.posLabel.categoryValue == "NNP") {
          spans += ("MISC" -> new TokenSpan(s, t.positionInSection, 1))
        }
      }
    }
    spans.toSeq
  }

  def getPronounSpans(doc: Document): Seq[TokenSpan] = {
    doc.tokens.filter(_.posLabel.isPersonalPronoun).map(t => new TokenSpan(t.section, t.positionInSection, 1)).toSeq
  }
  val PersonLexicon = new lexicon.UnionLexicon("MentionEntityTypePerson", lexicon.PersonPronoun, lexicon.PosessiveDeterminer)

  def getMentionEntityTypeLabelForPronoun(s: TokenSpan) : String = {
     if(PersonLexicon.contains(s))
       "PERSON"
     else
       "O"
  }

  def  getNerMentions(document: Document): Seq[Mention] = {
    getNerSpans(document).map(labelSpan => {
      val label = labelSpan._1
      val mappedLabel = if(label == "PER") "PERSON" else label    //this is important if you do conll NER, since MentionEntityType expects Ontonotes NER Labels
      val s = labelSpan._2
      val m = new Mention(s, s.length-1)
      m.attr += new MentionType(m, "NAM")
      m.attr += new MentionEntityType(m,mappedLabel)
      m
    })
  }

  def process(document: Document) = {
    val nerMentions = getNerMentions(document)
    val pronounMentions = getPronounSpans(document).map(s => {
      val m = new Mention(s, 0)
      val label = getMentionEntityTypeLabelForPronoun(m)
      m.attr += new MentionType(m, "PRO")
      m.attr += new MentionEntityType(m,label)
      m
    })
    document.attr += new NerMentionList() ++= (nerMentions ++ pronounMentions).sortBy(m => (m.head.stringStart, m.length))

    document
  }
}
