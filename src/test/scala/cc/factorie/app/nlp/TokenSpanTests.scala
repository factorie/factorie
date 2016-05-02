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

import org.scalatest._

/**
 * @author John Sullivan
 */
class TokenSpanTests extends FlatSpec with Matchers {

  def fixture = new {
    val doc = new Document()
    "Jaques and Jill went up the hill to fetch a pail of water .".split(' ').foreach { s =>
      new Token(doc, s)
    }

    val span = new TokenSpan(doc.asSection, 5, 2)

  }


  "TokenSpan" should "calculate context windows properly" in {
    val f = fixture
    import f._
    assert(Seq("went", "up", "to", "fetch") == span.contextWindow(2).map(_.string))
    assert(Seq.empty[String] == span.contextWindow(0))
    assert("Jaques and Jill went up to fetch a pail of water .".split(' ').toSeq == span.contextWindow(10).map(_.string))
  }
}
