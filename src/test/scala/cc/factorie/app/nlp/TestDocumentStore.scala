package cc.factorie.app.nlp

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.pos.{PennPosTag, PennPosDomain}
import cc.factorie.app.nlp.parse._

/** Test serialization of Document to BSON.
    @author John Sullivan, Andrew McCallum
 */
class TestDocumentStore extends FlatSpec with Matchers {

  "DocumentCubbie" should "serialize and deserialize properly" in {
    val doc1 = new Document("If it's your job to eat a frog, it's best to do it first thing in the morning. And If it's your job to eat two frogs, it's best to eat the biggest one first.")
    DocumentAnnotatorPipeline(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter).process(doc1)
    for (token <- doc1.tokens) token.attr += new PennPosTag(token, token.positionInSentence % PennPosDomain.size)
    for (sentence <- doc1.sentences) sentence.attr += new ParseTree(sentence, Range(0, sentence.length).toArray, Range(0, sentence.length).map(_ % 10).toArray)
    doc1.annotators(classOf[PennPosTag]) = this.getClass
    doc1.annotators(classOf[ParseTree]) = this.getClass

    val cubbie = new StandardDocumentCubbie() := doc1
    val doc2 = cubbie.document
    
    assert(doc1.tokens.toSeq.map(_.string) == doc2.tokens.toSeq.map(_.string))
    assert(doc1.tokens.toSeq.map(_.posTag.categoryValue) == doc2.tokens.toSeq.map(_.posTag.categoryValue))
  }

}
