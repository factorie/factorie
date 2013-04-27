package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._

class SimplifyDigitsLemmatizer extends DocumentAnnotator {
  def process1(document:Document): Document = {
    for (token <- document.tokens) token.attr += new SimplifyDigitsTokenLemma(token, cc.factorie.app.strings.simplifyDigits(token.string))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[SimplifyDigitsTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[SimplifyDigitsTokenLemma])
}
object SimplifyDigitsLemmatizer extends SimplifyDigitsLemmatizer

class SimplifyDigitsTokenLemma(token:Token, s:String) extends TokenLemma(token, s)
