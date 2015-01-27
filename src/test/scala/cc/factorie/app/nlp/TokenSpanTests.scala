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
