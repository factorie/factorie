package cc.factorie.util

import org.scalatest.{FlatSpec, Matchers}
import cc.factorie.util.JsonCubbieConverter._
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosTag}
import cc.factorie.app.nlp.parse.{ParseTreeLabelDomain, ParseTree}

/**
 * @author John Sullivan
 */
class TestJsonCubbieConverter extends FlatSpec with Matchers {
  class BasicTestCubbie extends Cubbie {
    val double = new DoubleSlot("double")
    val int = new IntSlot("int")
    val string = new StringSlot("string")
  }

  def fix = new {
    val doc1 = new Document("If it's your job to eat a frog, it's best to do it first thing in the morning. And If it's your job to eat two frogs, it's best to eat the biggest one first.")
    DocumentAnnotatorPipeline(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter).process(doc1)
    for (token <- doc1.tokens) token.attr += new PennPosTag(token, token.positionInSentence % PennPosDomain.size)
    for (sentence <- doc1.sentences) sentence.attr += new ParseTree(sentence, Range(0, sentence.length).toArray, Range(0, sentence.length).map(_ % ParseTreeLabelDomain.length).toArray)
    doc1.annotators(classOf[PennPosTag]) = this.getClass
    doc1.annotators(classOf[ParseTree]) = this.getClass
  }

  "JsonCubbieConverter" should "serialize and deserialize primitive values properly" in {
    val c = new BasicTestCubbie()

    c.double set 3.2043
    c.int set 5
    c.string set "test string"

    val json = toJson(c)
    val expectedJson = JObject(JField("double", JDouble(3.2043)), JField("int", JInt(5)), JField("string", JString("test string")))

    assert(json == expectedJson, "expected %s but got %s".format(compact(render(expectedJson)), compact(render(json))))

    val deserialized = toCubbie(json, {() => new BasicTestCubbie})
    assert(deserialized._map == c._map, "expected %s but got %s".format(c._map, deserialized._map))
  }

  it should "serialize and deserialize DocumentCubbies properly" in {
    val f = fix
    import f._

    val c = new StandardDocumentCubbie() := doc1
    val json = toJson(c)
    val doc2 = toCubbie(json, {() => new StandardDocumentCubbie()}).document
    assert(doc1.tokens.toSeq.map(_.string) == doc2.tokens.toSeq.map(_.string))
    assert(doc1.tokens.toSeq.map(_.posTag.categoryValue) == doc2.tokens.toSeq.map(_.posTag.categoryValue))
  }
}
