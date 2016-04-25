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

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author John Sullivan
 */
class TokenTests extends FlatSpec with Matchers {

  def fixture = new {
    val doc = new Document()
    "Jaques and Jill went up the hill to fetch a pail of water .".split(' ').foreach { s =>
      new Token(doc, s)
    }

    val span = new TokenSpan(doc.asSection, 5, 2)
  }

  "Token" should "calculate context bags properly" in {
    val f = fixture
    import f._
    assert(doc.tokens.toSeq(2).string == "Jill")
    assert(doc.tokens.toSeq(2).contextBag(3).map(_.string).toSet == "Jaques and went up the".split(" ").toSet)
  }
}
