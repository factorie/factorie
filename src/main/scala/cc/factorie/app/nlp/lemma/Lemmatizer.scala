package cc.factorie.app.nlp.lemma

trait Lemmatizer {
  def lemmatize(word:String): String
}

object NoopLemmatizer extends Lemmatizer {
  def lemmatize(word:String): String = word
}
