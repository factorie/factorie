package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._

class CollapseDigitsLemmatizer extends DocumentAnnotator with Lemmatizer {
  def lemmatize(word:String): String = cc.factorie.app.strings.collapseDigits(word)
  def process1(document:Document): Document = {
    for (token <- document.tokens) token.attr += new SimplifyDigitsTokenLemma(token, lemmatize(token.string))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[CollapseDigitsTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[CollapseDigitsTokenLemma])
}
object CollapseDigitsLemmatizer extends SimplifyDigitsLemmatizer

class CollapseDigitsTokenLemma(token:Token, s:String) extends TokenLemma(token, s)
