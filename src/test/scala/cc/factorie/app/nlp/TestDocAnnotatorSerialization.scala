package cc.factorie.app.nlp

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.pos.PennPosTag

/**
 * @author John Sullivan
 */
class TestDocAnnotatorSerialization extends FlatSpec with Matchers {

  def docFixture = new {
    val doc = new Document("Tom ate the dog.")
    val doc2 = new Document("Tom ate the dog.")
    DeterministicTokenizer.process(doc)
    DeterministicSentenceSegmenter.process(doc)
    RandPOSAnnotator.process(doc)

    DeterministicTokenizer.process(doc2)
    DeterministicSentenceSegmenter.process(doc2)
  }


  "PennPosSerializer" should "serialize properly" in {
    val f = docFixture
    import f._

    val cubbie = PennPosCubbieSerializer.serialize(doc)
    assert(cubbie.annotation.value == classOf[PennPosTag].toString)
    assert(cubbie.annotator.value == RandPOSAnnotator.getClass.toString)
    assert(cubbie.data.value == doc.tokens.map(_.posTag.intValue).toSeq)
  }

  it should "deserialize properly" in {
    val f = docFixture
    import f._

    val cubbie = PennPosCubbieSerializer.serialize(doc)

    PennPosCubbieSerializer.deserialize(cubbie, doc2)
    assert(doc.tokens.zip(doc2.tokens).forall{ case (t1, t2) =>
      t1.posTag.intValue == t2.posTag.intValue
    })
  }
}
