package cc.factorie.app.nlp.segment

import org.junit.Test
import org.junit.Assert._

/**
 * User: apassos
 * Date: 8/19/13
 * Time: 1:22 PM
 */
class TestPhraseTokenizer {
  @Test def testPhrases() {
    val phrases = Seq(Seq("of", "cards"), Seq("New", "York", "City"), Seq("New", "York"))
    val phraseTokenizer = new PhraseTokenizer(phrases)
    val sampleDocument = new cc.factorie.app.nlp.Document("I built myself a house of cards in New York City, New York State.")
    Tokenizer1.process(sampleDocument)
    phraseTokenizer.process(sampleDocument)
    val oldLength = sampleDocument.sections.length
    assert(oldLength > 0)
    val result = sampleDocument.attr[PhraseSectionList]
    assertEquals(oldLength, result.length)
    val tokens = result.head.tokens.map(_.string)
    val expected = Seq("I", "built", "myself", "a", "house", "of cards", "in", "New York City", ",", "New York", "State", ".")
    // println(tokens)
    assertEquals(expected.length, tokens.length)
    for ((e, t) <- expected.zip(tokens)) {
      assertEquals(e, t)
    }
  }
}
