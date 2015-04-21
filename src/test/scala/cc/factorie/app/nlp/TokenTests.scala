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
