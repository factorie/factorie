package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._

class SimplifyDigitsLemmatizer extends DocumentProcessor {
  def process(document:Document): Document = {
    for (token <- document.tokens) token.setLemmaString(cc.factorie.app.strings.simplifyDigits(token.string))
    document
  }
}
