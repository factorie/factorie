package cc.factorie.app.nlp.lemma
import cc.factorie.app.nlp._

class LowercaseLemmatizer extends DocumentAnnotator with Lemmatizer {
  def lemmatize(word:String): String = word.toLowerCase
  def process(document:Document): Document = {
    for (token <- document.tokens) token.attr += new LowercaseTokenLemma(token, lemmatize(token.string))
    document
  }
  override def tokenAnnotationString(token:Token): String = { val l = token.attr[LowercaseTokenLemma]; l.value }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[LowercaseTokenLemma])
}
object LowercaseLemmatizer extends LowercaseLemmatizer

class LowercaseTokenLemma(token:Token, s:String) extends TokenLemma(token, s)
