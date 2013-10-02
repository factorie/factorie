package cc.factorie.app.bib.parser

import scala.util.parsing.combinator._
import scala.language.implicitConversions

private[parser] trait SharedParsers extends RegexParsers {
  override val skipWhitespace = false
  // FIXME: it's more readable if this is '+', not '*' - go find places that rely on it being '+' and add a '?'
  lazy val WS = r("\\s*")
  lazy val BRACE_DELIMITED_STRING_NO_OUTER: Parser[String] =
    BRACE_DELIMITED_STRING ^^ (s => s.substring(1, s.length - 1))
  lazy val BRACE_DELIMITED_STRING: Parser[String] =
    '{' ~> (BRACE_DELIMITED_STRING | """\\.""" | """[^}{]""").* <~ '}' ^^
    ("{" + _.mkString + "}")

  implicit def c(x: Char): Parser[Char] = accept(x)
  implicit def r(reg: String): Parser[String] = regex(reg.r)
}