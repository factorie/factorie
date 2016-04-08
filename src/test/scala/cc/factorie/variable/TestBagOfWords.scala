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
package cc.factorie.variable

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author John Sullivan
 */
class TestBagOfWords extends FlatSpec with Matchers {

  "BagOfWords" should "work subtract properly with new values" in {
    val b1 = new BagOfWords()
    b1 -=("foo", 1.0)
    assert(b1.asHashMap.keySet == Set("foo"))
    assert(b1.asHashMap("foo") == -1.0)
  }

  "BagOfWords" should "initialize from initialWords string" in {
    val b = new BagOfWords("the quick brown fox jumps over the lazy dog".split(" "))
    assert(b.size == 8)
    assert(b.asHashMap.size == 8)

    assert(b.asHashMap("the") == 2.0)
    assert(b.asHashMap("fox") == 1.0)
    assert(b("the") == 2.0)
    assert(b("fox") == 1.0)

    assert(b.contains("the"))
    assert(!b.contains("abcd"))
  }
}
