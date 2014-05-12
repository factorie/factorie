/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
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
    DeterministicTokenizer.process(sampleDocument)
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
