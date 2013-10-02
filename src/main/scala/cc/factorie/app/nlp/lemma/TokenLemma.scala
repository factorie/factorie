package cc.factorie.app.nlp.lemma
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable.StringVariable

/** Used as an attribute of Token to hold the lemma of the Token.string.
    For example for string value "ran" might have lemma "run". */
class TokenLemma(val token:Token, s:String) extends StringVariable(s) {
  def lemma: String = value // for backward compatibility with Brian's old "Lemma" class
}
