package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.phrase.{Phrase,NounPhraseType,OntonotesPhraseEntityType}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.ner._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.Span

// TODO Remove this
class NerMentionList(spans:Iterable[Mention]) extends MentionList(spans)

/** Trait for objects that return a list of Phrases given a Document 
    whose annotations includes those classes listed in prereqAttrs.
    This is not a DocumentAnnotator because it does not add its results to the Document.attr; 
    invocations to its apply method simple return a collection of Phrases.
    
    This design was chosen because these phrases are often used for coreference
    in which there are many coreference-specific choices of what mentions are filtered
    or included, and we didn't want to polute the Document.attr with a tremendous number
    of postAttrs that are specific to individual coreference solutions.
    
    If you really want a DocumentAnnotator that saves its results, it is easy to
    create one uses a PhraseFinder.
     
    @author Andrew McCallum
    */
trait MentionFinder {
  def prereqAttrs: Seq[Class[_]]
  //def phrasePostAttrs: Seq[Class[_]]
  def addMentions(coref:WithinDocCoref): Unit
}


/** Apply returns a list of pronoun phrases, given PennPosTags.
    @author Andrew McCallum */
object PronounMentionFinder extends MentionFinder {
  def prereqAttrs = Seq(classOf[PennPosTag])
  def addMentions(coref:WithinDocCoref): Unit = { 
    val phrases = coref.document.tokens.filter(_.posTag.isPersonalPronoun).map(t => new Phrase(t.section, start=t.positionInSection, length=1, headTokenOffset=0))
    val mentions = phrases.map(coref.addMention(_))
  }
}

object NerMentionPhrases {
  def prereqAttrs = Seq(classOf[PennPosTag])
  def apply(doc:Document): Seq[Phrase] = {
    val result = new ArrayBuffer[Phrase]
    
    result
  }
}

/** Apply returns a list of named entity phrases, given CilouConllNerTags.
    @author Alexandre Passos and Andrew McCallum */
object NerAndPronounMentionFinder extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[BilouConllNerTag], classOf[PennPosTag])
  def postAttrs = Seq(classOf[NerMentionList], classOf[OntonotesPhraseEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.phrase.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.phrase.attr[NounPhraseType].categoryValue+":"+ m.phrase.attr[OntonotesPhraseEntityType].categoryValue +":" +m.phrase.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r
  def getNerSpans(doc: Document): Seq[(String,TokenSpan)] = {
    val spans = ArrayBuffer[(String,TokenSpan)]()
    for (s <- doc.sections;t <- s.tokens) {
      if (t.attr[BilouConllNerTag].categoryValue != "O") {
        val attr = t.attr[BilouConllNerTag].categoryValue.split("-")
        if (attr(0) == "U") {
          val lab = attr(1)
          spans += (lab -> new TokenSpan(s, t.positionInSection, 1))
        } else if (attr(0) == "B") {
          val lab = attr(1)
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.attr[BilouConllNerTag].categoryValue.matches("(I|L)-" + attr(1))) lookFor = lookFor.next
            spans += (lab -> new TokenSpan(s, t.positionInSection, lookFor.positionInSection - t.positionInSection))
          } else {
            spans += (lab -> new TokenSpan(s, t.positionInSection, 1))
          }
        }
      } else {
        if ( t.string.length > 2 && !t.containsLowerCase && upperCase.findFirstIn(t.string).nonEmpty && (t.getNext ++ t.getPrev).exists(i => i.containsLowerCase)) {
          spans += ("ORG" -> new TokenSpan(s, t.positionInSection, 1))
        } else if (t.posTag.categoryValue == "NNP") {
          spans += ("MISC" -> new TokenSpan(s, t.positionInSection, 1))
        }
      }
    }
    spans.toSeq
  }

  def getPronounSpans(doc: Document): Seq[TokenSpan] = {
    doc.tokens.filter(_.posTag.isPersonalPronoun).map(t => new TokenSpan(t.section, t.positionInSection, 1)).toSeq
  }
  val PersonLexicon = new lexicon.UnionLexicon("MentionEntityTypePerson", lexicon.PersonPronoun, lexicon.PosessiveDeterminer)

  def getMentionEntityTypeLabelForPronoun(s: TokenSpan) : String = {
     if(PersonLexicon.contains(s))
       "PERSON"
     else
       "O"
  }

  def  getNerMentions(document: Document): Seq[Mention] = {
    val coref = document.getCoref
    getNerSpans(document).map(labelSpan => {
      val label = labelSpan._1
      val mappedLabel = if(label == "PER") "PERSON" else label    //this is important if you do conll NER, since MentionEntityType expects Ontonotes NER Labels
      val s = labelSpan._2
      val m = coref.addMention(new Phrase(s, s.length-1))
      m.phrase.attr += new NounPhraseType(m.phrase, "NAM")
      m.phrase.attr += new OntonotesPhraseEntityType(m.phrase, mappedLabel)
      m
    })
  }

  def process(document: Document) = {
    val nerMentions = getNerMentions(document)
    val coref = document.getCoref
    val pronounMentions = getPronounSpans(document).map(s => {
      val m = coref.addMention(new Phrase(s, 0))
      val label = getMentionEntityTypeLabelForPronoun(m.phrase)
      m.phrase.attr += new NounPhraseType(m.phrase, "PRO")
      m.phrase.attr += new OntonotesPhraseEntityType(m.phrase, label)
      m
    })
    document.attr += new NerMentionList((nerMentions ++ pronounMentions).sortBy(m => (m.phrase.head.stringStart, m.phrase.length)))
    document
  }
}
