package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.mention._
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.app.nlp.ner.{NerSpan, NerLabel}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.Span

class NerPronounPhraseList extends MentionList

/** Find noun phrases by a combination of named-entity recognition and lexicon-based pronoun finding.
    @author Alexandre Passos */
object NounPhraser2 extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[NerLabel], classOf[PennPosLabel])
  def postAttrs = Seq(classOf[NerMentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r
  def getNerSpans(doc: Document): Seq[(String,TokenSpan)] = {
    val spans = ArrayBuffer[(String,NounPhrase)]()
    for (s <- doc.sections;t <- s.tokens) {
      if (t.attr[NerLabel].categoryValue != "O") {
        val attr = t.attr[NerLabel].categoryValue.split("-")
        if (attr(0) == "U") {
          val lab = attr(1)
          spans += (lab -> new NounPhrase(s, t.positionInSection, 1))
        } else if (attr(0) == "B") {
          val lab = attr(1)
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.attr[NerLabel].categoryValue.matches("(I|L)-" + attr(1))) lookFor = lookFor.next
            spans += (lab -> new NounPhrase(s, t.positionInSection, lookFor.positionInSection - t.positionInSection))
          } else {
            spans += (lab -> new NounPhrase(s, t.positionInSection, 1))
          }
        }
      } else {
        if ( t.string.length > 2 && !t.containsLowerCase && upperCase.findFirstIn(t.string).nonEmpty && (t.getNext ++ t.getPrev).exists(i => i.containsLowerCase)) {
          spans += ("ORG" -> new NounPhrase(s, t.positionInSection, 1))
        } else if (t.posLabel.categoryValue == "NNP") {
          spans += ("PER" -> new NounPhrase(s, t.positionInSection, 1))
        }
      }
    }
    spans.toSeq
  }

  def getPronounSpans(doc: Document): Seq[TokenSpan] = {
    doc.tokens.filter(_.posLabel.isPersonalPronoun).map(t => new TokenSpan(t.section, t.positionInSection, 1)).toSeq
  }
  val PersonLexicon = new lexicon.UnionLexicon("MentionEntityTypePerson", lexicon.PersonPronoun, lexicon.PosessiveDeterminer) // TODO change lexicon name to "EntityTypePerson"? -akm

  def getMentionEntityTypeLabelForPronoun(s: TokenSpan) : String = {
     if(PersonLexicon.contains(s))
       "PERSON"
     else
       "O"
  }

  def process(document: Document) = {
    val nerPhrases = getNerSpans(document).map(labelSpan => {
      val label = labelSpan._1
      val mappedLabel = if (label == "PER") "PERSON" else label    //this is important if you do conll NER, since MentionEntityType expects Ontonotes NER Labels
      val s = labelSpan._2
      val m = new NounPhrase(s.section, s.start, s.length, s.length-1)
      m.attr += new NounPhraseType(m, "NAM")
      m.attr += new NounPhraseEntityType(m, mappedLabel)
      m
    })
    val pronounPhrases = getPronounSpans(document).map(span => {
      val m = new NounPhrase(span.section, span.start, span.length, 0)
      val label = getMentionEntityTypeLabelForPronoun(m)
      m.attr += new NounPhraseType(m, "PRO")
      m.attr += new NounPhraseEntityType(m, label)
      m
    })
    document.attr += new NounPhraseList() ++= (nerPhrases ++ pronounPhrases).sortBy(m => (m.head.stringStart, m.length))
    document
  }
}