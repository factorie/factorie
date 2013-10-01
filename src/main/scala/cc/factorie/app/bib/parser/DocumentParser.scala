package cc.factorie.app.bib.parser

import languageFeature.postfixOps

private[parser] object AST {
  // these types all have very generic names, so wrap them in an "AST" prefix
  sealed trait Entry
  sealed trait Value

  final case class Document(entries: List[Entry])

  final case class StringEntry(abbrev: String, value: Value) extends Entry
  final case class PreambleEntry(content: Value) extends Entry
  final case class CommentEntry(comment: String) extends Entry
  final case class RegularEntry(
    ty: String, citationKey: String, tags: List[(String, Value)]) extends Entry

  final case class Literal(content: String) extends Value
  final case class Abbrev(name: String) extends Value
  final case class Concat(left: Value, right: Value) extends Value

}

private[parser] object DocumentParser {

  import AST._

  // this should return an Either or a custom error object instead of a useless "None"
  def parseString(input: String): Either[String, Document] = {
    val res = Impl.parseAll(Impl.bibTex, input)
    res.map(r => Right(Document(r))).getOrElse(Left(res.toString))
  }

  object Impl extends SharedParsers {

    lazy val bibTex =
      ((freeComment | WS2) ~> anyEntry <~ (freeComment | WS2)).+ ^^
      (_ flatMap { case x => List(x): List[Entry] })

    // FIXME: lines starting with %%% are comments
    lazy val freeComment = "[^@]*".? ^^ (s => CommentEntry(s.getOrElse("")))
    lazy val WS2 = WS ^^ (CommentEntry(_))

    lazy val anyEntry = AT ~> (commentEntry | stringEntry | preambleEntry | regularEntry)

    lazy val commentEntry =
      COMMENT ~> (WS ~> (('{' ~> "[^}]*" <~ '}') | ('(' ~> "[^)]*" <~ ')')) | "[^@\r\n]*") ^^
      (CommentEntry(_))

    lazy val stringEntry = STRING ~> WS ~> entryBody { tag } ^^ (StringEntry(_, _)).tupled

    lazy val preambleEntry = PREAMBLE ~> WS ~> entryBody { value } ^^ (PreambleEntry(_))

    lazy val regularEntry =
      (SYMBOL <~ WS) ~ entryBody { SYMBOL_CAN_START_WITH_NUMBER ~ rep((COMMA_WS | WS) ~> tag) <~ COMMA_WS.? } ^^ {
        case ty ~ (key ~ tags) => RegularEntry(ty, key, tags)
      }

    def entryBody[T](parser: => Parser[T]): Parser[T] = {
      lazy val p = parser
      ("\\{\\s*" ~> p <~ "\\s*\\}") |
      ("\\(\\s*" ~> p <~ "\\s*\\)")
    }

    lazy val tag = (SYMBOL <~ "\\s*=\\s*") ~ value ^^ {
      case sym ~ v => (sym, v)
    }

    lazy val value: Parser[Value] = literalOrSymbol ~ ("\\s*#\\s*" ~> value).? ^^ {
      case left ~ Some(right) => Concat(left, right)
      case left ~ _ => left
    }

    lazy val literalOrSymbol = (SYMBOL ^^ (Abbrev(_))) | literal

    lazy val literal = numericLiteral | braceDelimitedNoOuterLiteral | quoteDelimitedLiteral

    lazy val numericLiteral = "\\d+(\\.\\d+)?" ^^ (Literal(_))
    lazy val quoteDelimitedLiteral =
      '"' ~> (BRACE_DELIMITED_STRING | """\\.""" | """[^"]""").* <~ '"' ^^ (xs => Literal(xs.mkString))
    lazy val braceDelimitedNoOuterLiteral = BRACE_DELIMITED_STRING_NO_OUTER ^^ (Literal(_))

    lazy val AT = c('@')
    lazy val COMMA_WS = r("\\s*,\\s*")
    lazy val COMMENT = r("(c|C)(o|O)(m|M)(m|M)(e|E)(n|N)(t|T)")
    lazy val STRING = r("(s|S)(t|T)(r|R)(i|I)(n|N)(g|G)")
    lazy val PREAMBLE = r("(p|P)(r|R)(e|E)(a|A)(m|M)(b|B)(l|L)(e|E)")
    // anything except can't start with number, quotes, braces/parens, '#', commas, whitespace, or '='
    lazy val SYMBOL = r("[^0-9\"}{)(,\\s#=][^\"}{)(,\\s#=]*")
    // can start with (or even be entirely) a number
    lazy val SYMBOL_CAN_START_WITH_NUMBER = r("[^\"}{)(,\\s#=][^\"}{)(,\\s#=]*")
  }
}
