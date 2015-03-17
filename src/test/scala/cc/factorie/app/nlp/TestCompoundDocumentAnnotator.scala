package cc.factorie.app.nlp

import org.scalatest.{FlatSpec, Matchers}
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}

/**
 * @author John Sullivan
 */
class TestCompoundDocumentAnnotator extends FlatSpec with Matchers {
  def fix = new {
    val doc = new Document("Better to sleep with a sober cannibal than a drunken Christian.")
  }

  "CompoundDocumentAnnotator" should "work properly" in {
    val f = fix
    import f._

    val annos = Seq(DeterministicTokenizer, DeterministicSentenceSegmenter)

    val compAnno = new CompoundDocumentAnnotator(annos)
    compAnno process doc

    assert(doc.annotators.keySet contains classOf[Token])
    assert(doc.annotators.keySet contains classOf[Sentence])
  }
}
