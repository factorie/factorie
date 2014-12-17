package cc.factorie.variable

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author John Sullivan
 */
class BagOfWordsTests extends FlatSpec with Matchers {

  "BagOfWords" should "work subtract properly with new values" in {
    val b1 = new BagOfWords()
    b1 -= ("foo", 1.0)
    assert(b1.asHashMap.keySet == Set("foo"))
    assert(b1.asHashMap("foo") == -1.0)
  }
}
