/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp

import cc.factorie.app.nlp.segment.{DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter}
import org.scalatest.{FlatSpec, Matchers}

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

    val annos = Seq(DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter)

    val compAnno = new CompoundDocumentAnnotator(annos)
    compAnno process doc

    assert(doc.annotators.keySet contains classOf[Token])
    assert(doc.annotators.keySet contains classOf[Sentence])
  }
}
