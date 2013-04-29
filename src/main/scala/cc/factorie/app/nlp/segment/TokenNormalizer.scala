package cc.factorie.app.nlp.segment
import cc.factorie.app.nlp._

// TODO Look again at this more carefully to see what options we want.
// TODO Also consider putting much of this instead in the Tokenizer itself, but we do need a way to manipulate pre-tokenized data, such as CoNLL2003 and PTB.

/** Clean up Token.string according to various standard practices. */
class TokenNormalizer(
    val newTokenString: (Token,String) => TokenString, // potentially a specialized subclass of TokenString, to reflect different choices here.
    val asciiQuotes:Boolean = true, 
    val undoPTBParens:Boolean = true, 
    val unescapeSlash:Boolean = true,
    val unescapeAsterisk:Boolean = true,
    val twoOneDash:Boolean = true,
    val americanize:Boolean = false
  ) extends DocumentAnnotator {
  private def matches(source:String, targets:String*): Boolean = false
  def process1(document:Document): Document = {
    for (token <- document.tokens) { 
      if (asciiQuotes && (token.string == "``" || token.string == "''")) token.attr += new TokenString(token, "\"")
      else if (undoPTBParens && token.string == "-LRB-") token.attr += new TokenString(token, "(")
      else if (undoPTBParens && token.string == "-RRB-") token.attr += new TokenString(token, ")")
      else if (undoPTBParens && token.string == "-LCB-") token.attr += new TokenString(token, "{")
      else if (undoPTBParens && token.string == "-RCB-") token.attr += new TokenString(token, "}")
      else if (undoPTBParens && token.string == "-LSB-") token.attr += new TokenString(token, "[")
      else if (undoPTBParens && token.string == "-RSB-") token.attr += new TokenString(token, "]")
      else if (unescapeSlash && token.string.contains("\\/")) token.attr += new TokenString(token, token.string.replace("\\/", "/"))
      else if (unescapeAsterisk && token.string == "\\*") token.attr += new TokenString(token, "*")
      else if (unescapeAsterisk && token.string == "\\*\\*") token.attr += new TokenString(token, "**")
      else if (twoOneDash && token.string == "--") token.attr += new TokenString(token, "-")
      else if (twoOneDash && token.string == "Ñ") token.attr += new TokenString(token, "-") // replace m-dash with dash
      else if (americanize && BritishToAmerican.contains(token.string)) token.attr += new TokenString(token, BritishToAmerican(token.string))
    }
    document
  }
  override def tokenAnnotationString(token:Token): String = null
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = Nil
}

class SimplifyPTBTokenString(token:Token, str:String) extends TokenString(token, str)
object SimplifyPTBTokenNormalizer extends TokenNormalizer((t:Token, s:String) => new SimplifyPTBTokenString(t,s), true, true, true, true, true, false)


object BritishToAmerican extends scala.collection.mutable.HashMap[String,String] {
  this("colour") = "color"
  // TODO Add more
}
