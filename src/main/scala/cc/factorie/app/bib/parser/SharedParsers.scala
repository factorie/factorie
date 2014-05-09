/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
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