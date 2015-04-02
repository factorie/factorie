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
    //val pipe = new CompoundDocumentAnnotator(Seq(DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter, BilouEventStringMatchingLabeler))
    //pipe.process(doc)

    doc.tokens foreach { token =>
      println(Seq(token.string, token.nerTag).mkString("\t"))
    }
  }

}
