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
