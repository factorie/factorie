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

/**
 * User: apassos
 * Date: 8/19/13
 * Time: 2:24 PM
 */
class TestBigramStatistics {
  @Test def testBigramStatistics() {
    val gpl = new cc.factorie.app.nlp.Document(cc.factorie.tutorial.WordSegmenter.data.mkString("\n"))
    DeterministicTokenizer.process(gpl)
    val bg = new BigramStatistics
    bg.process(gpl)
    val phrases = bg.getLikelyPhrases(5, 40)
    assert(phrases.exists(p => p(0) == "free" && p.length > 1 && p(1) == "software"))
    assert(!phrases.exists(p => p(0) == "you" && p.length > 1 && p(1) == "may"))
  }
}
