package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp._
import scala.reflect.ClassTag
import cc.factorie.app.nlp.lexicon.{LexiconMention, TriePhraseLexicon}
import java.io.{BufferedReader, InputStreamReader}
import cc.factorie._

/**
 * A very simple document annotator that labels tokens (and spans) in a document
 * using string matching based on a list of phrases. Each list corresponds to
 * a label. BILOU domain is generated on-the-fly from the given list of labels.
 */
abstract class StringMatchingLabeler[Span <: NerSpan : ClassTag, Tag <: NerTag : ClassTag](tagLexicons:Iterable[(String, TriePhraseLexicon)]) extends NerAnnotator[Span, Tag] {
  val prereqAttrs = Seq(classOf[Token])

  def annotateTokens(document: Document) = {

    def nerTag(t:Token):Tag = t.attr.exactly[Tag]

    tagLexicons foreach {
      case (label, trie) =>
        trie.trie.findMentions(document.tokens.map(_.lemmaString)) foreach { case LexiconMention(mention, start, end) =>
          val range = document.tokens.slice(start, end)
          if(range.forall(t => nerTag(t) != null) && range.forall(t => nerTag(t).categoryValue == "O")) {
            nerTag(range.head).domain match {
              case _:BILOU =>
                if(start == end -1) { // todo is this a single token?
                  nerTag(range.head).setCategory(s"U-$label")(null)
                } else {
                  nerTag(range.head).setCategory(s"B-$label")(null)
                  range.slice(1, range.size -1).foreach(t => nerTag(t).setCategory(s"I-$label")(null))
                  nerTag(range.last).setCategory(s"L-$label")(null)
                }
              case _:BIO =>
                nerTag(range.head).setCategory(s"B-$label")(null)
                range.drop(1).foreach(t => nerTag(t).setCategory(s"I-$label")(null))
            }
          }
        }
    }
    document
  }
}

object EventStringMatchingLabeler extends StringMatchingLabeler[BBNEventNerSpan, BBNEventNerTag](Seq("CHARGES", "JOB_TITLE").map{lexiconName =>
  val lexicon = new TriePhraseLexicon(lexiconName)
  new BufferedReader(new InputStreamReader(getClass.getResourceAsStream("/event/lexicons/" + lexiconName))).toIterator foreach lexicon.+=
  lexiconName -> lexicon
}) {
  def newSpan(sec: Section, start: Int, length: Int, category: String) = new BBNEventNerSpan(sec, start, length, category)
  def newBuffer = new BBNEventNerSpanBuffer
}

