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
  def processToken(token:Token): Unit = {
    if (asciiQuotes && (token.string == "``" || token.string == "''")) token.attr += newTokenString(token, "\"")
    else if (undoPTBParens && token.string == "-LRB-") token.attr += newTokenString(token, "(")
    else if (undoPTBParens && token.string == "-RRB-") token.attr += newTokenString(token, ")")
    else if (undoPTBParens && token.string == "-LCB-") token.attr += newTokenString(token, "{")
    else if (undoPTBParens && token.string == "-RCB-") token.attr += newTokenString(token, "}")
    else if (undoPTBParens && token.string == "-LSB-") token.attr += newTokenString(token, "[")
    else if (undoPTBParens && token.string == "-RSB-") token.attr += newTokenString(token, "]")
    else if (unescapeSlash && token.string.contains("\\/")) token.attr += newTokenString(token, token.string.replace("\\/", "/"))
    else if (unescapeAsterisk && token.string == "\\*") token.attr += newTokenString(token, "*")
    else if (unescapeAsterisk && token.string == "\\*\\*") token.attr += newTokenString(token, "**")
    else if (twoOneDash && token.string == "--") token.attr += newTokenString(token, "-")
    else if (twoOneDash && token.string == "Ñ") token.attr += newTokenString(token, "-") // replace m-dash with dash
    else if (americanize && BritishToAmerican.contains(token.string)) token.attr += newTokenString(token, BritishToAmerican(token.string))
  }
  def process(document:Document): Document = {
    document.tokens.foreach(processToken(_))
    document
  }
  override def tokenAnnotationString(token:Token): String = null
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[SimplifyPTBTokenString])
}

class SimplifyPTBTokenString(token:Token, str:String) extends TokenString(token, str)
object SimplifyPTBTokenNormalizer extends TokenNormalizer((t:Token, s:String) => new SimplifyPTBTokenString(t,s), true, true, true, true, true, false)


object BritishToAmerican extends scala.collection.mutable.HashMap[String,String] {
  this("colour") = "color"
  // TODO Add more
}
