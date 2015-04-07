package cc.factorie.app.nlp.ner

import org.scalatest.{Matchers, FlatSpec}
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.{DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter}
import cc.factorie.app.nlp.lemma.{LowercaseLemmatizer, TokenLemma}

/**
 * @author John Sullivan
 */
class TestStringMatchingNer extends FlatSpec with Matchers {

  def f = new {
    val text = "Caitlin Cellier, pickpocket entomologist, was indicted today on Federal RICO charges for her violent murder of Minsc and Imoen."
    val doc = new Document(text)
    val goldTags = Seq("O","O","O","U-JOB_TITLE","U-JOB_TITLE","O","O","O","O","O","O","B-CHARGES","L-CHARGES","O","O","O","U-CHARGES","O","O","O","O","O")
  }

  "EventStringMatchingLabeler" should "retrieve charges" in {
    val doc = f.doc
    DeterministicNormalizingTokenizer.process(doc)
    DeterministicSentenceSegmenter.process(doc)
    doc.tokens foreach { token =>
      token.attr += new BilouBBNEventNerTag(token, "O")
      token.attr += new TokenLemma(token, LowercaseLemmatizer.lemmatize(token.string))
    }
    BilouEventStringMatchingLabeler.process(doc)

    assert(doc.tokens.map(_.nerTag.categoryValue).zip(f.goldTags).forall{case (pred, gold) => pred == gold})
  }

}
