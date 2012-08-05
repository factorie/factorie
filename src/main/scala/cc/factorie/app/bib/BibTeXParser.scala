package cc.factorie.app.bib

/**
Luke's BibTeX parser consolidated into a single file.
*/

// Names.scala //
object Names {

  final case class Name(
    first: String,
    von: String,
    last: String,
    jr: String)

  private[bib] sealed trait Token
  private[bib] case object AND extends Token
  private[bib] case object COMMA extends Token
  private[bib] case object HYPHEN extends Token
  private[bib] case class TOKENLIST(ts: List[Token]) extends Token
  private[bib] final case class FRAGMENT(text: String) extends Token

  def stringToNames(names: String): List[Name] =
    fragmentsToNames(lexNameFragments(names))

  private def fragmentsToNames(fragments: List[Token]): List[Name] =
    splitOn(fragments)(AND ==).map(fragmentsToName(_))

  private def fragmentsToName(fragments: List[Token]): Name = {
    val sectionTokens = splitOn(fragments)(COMMA ==)
    if (sectionTokens.flatten.length == 0) sys.error("Name must have at least one fragment!")
    val sections = sectionTokens.map(_ map {
      case FRAGMENT(f) => f
      case _ => sys.error("Only fragments should be left over after processing!")
    })
    val isVon: String => Boolean = _.filter(_.isLetter).headOption.map(_.isLower).getOrElse(false)
    sections match {
      case List(firstVonLast) if firstVonLast.exists(isVon) =>
        val (tsal, noVtsrif) = partitionTakeWhile(firstVonLast.reverse, 1)(!isVon(_))
        val (first, von) = partitionTakeWhile(noVtsrif.reverse)(!isVon(_))
        segmentListsToName(first, von, tsal.reverse, Nil)
      case List(firstLast) =>
        val (first, last) = partitionTakeRight(firstLast, 1)
        segmentListsToName(first, Nil, last, Nil)
      case List(vonLast, first) =>
        val (tsal, nov) = partitionTakeWhile(vonLast.reverse, 1)(!isVon(_))
        segmentListsToName(first, nov.reverse, tsal.reverse, Nil)
      case List(vonLast, jr, first) =>
        val (tsal, nov) = partitionTakeWhile(vonLast.reverse, 1)(!isVon(_))
        segmentListsToName(first, nov.reverse, tsal.reverse, jr)
      case _ => sys.error("too many commas in name!")
    }
  }

  private def segmentListsToName(
    first: List[String], von: List[String], last: List[String], jr: List[String]) =
    Name(first.mkString(" "), von.mkString(" "), last.mkString(" "), jr.mkString(" "))

  import annotation.tailrec

  private def splitOn[T](xs: List[T])(pred: T => Boolean): List[List[T]] = {
    @tailrec def loop(
      segments: List[List[T]] = List(Nil),
      remaining: List[T] = xs
      ): List[List[T]] = remaining match {
      case Nil => segments
      case current :: rest =>
        if (pred(current)) loop(Nil :: segments, rest)
        else loop((current :: segments.head) :: segments.tail, rest)
    }
    loop().map(_.reverse).reverse
  }

  private def partitionTakeWhile[T](
    xs: List[T], minToTake: Int = 0)(pred: T => Boolean): (List[T], List[T]) = {
    if (minToTake > xs.length) sys.error("minToTake is greater than length of list!")
    val segment = xs.takeWhile(pred)
    val left = if (segment.length < minToTake) xs.take(minToTake) else segment
    (left, xs.drop(math.max(segment.length, minToTake)))
  }

  private def partitionTakeRight[T](xs: List[T], toTake: Int): (List[T], List[T]) =
    (xs.dropRight(toTake), xs.takeRight(toTake))

  private def lexNameFragments(namesString: String): List[Names.Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  private def flattenTokenLists(ts: List[Token]): List[Token] = ts flatMap {
    case TOKENLIST(inner) => flattenTokenLists(inner)
    case other => List(other)
  }

  // check out http://www.tug.org/TUGboat/tb27-2/tb87hufflen.pdf
  // I should write a prose description of the rules of how it parses names, as they are extremely complicated
  // FIXME: should hyphens make it to the later phase? what makes them different from plain old spaces?
  object NameLexer extends Parser.BibtexParser {

    def nameLexer =
      WS ~> ((initial | fragment_comma_or_ws) <~ WS).? ~
      ((and_ws | initial | hyphen | fragment_comma_or_ws) <~ WS).* ~
      (fragment | initial).? ^^ {
        case pre ~ xs ~ post => flattenTokenLists(pre.toList ++ xs ++ post.toList).filterNot(HYPHEN ==)
      }

    def fragment_comma_or_ws =
      fragment ~ ((WS ~> comma <~ WS) | "\\s+") ^^ {
        case frag ~ COMMA => TOKENLIST(List(frag, COMMA))
        case frag ~ _ => frag
      }

    def and_ws = and <~ "\\s+"
    def and = "and" ^^ (_ => AND)
    def comma = "," ^^ (_ => COMMA)
    def hyphen = ("-" | "~") ^^ (_ => HYPHEN)
    def initial = "[A-Za-z]\\." ^^ (FRAGMENT(_))

    // if its just one fragment with curly braces, its a literal, so leave out the braces
    def fragment =
      (BRACE_DELIMITED_STRING_NO_OUTER ?) ~ ("[^\\s,}{-~]+" | BRACE_DELIMITED_STRING).* ^^ {
        case Some(bds) ~ Nil => bds
        case Some(bds) ~ rest => (("{" + bds + "}") :: rest).mkString
        case None ~ rest => rest.mkString
      } ^^ (FRAGMENT(_))

  }
}

// Parser.scala //
object AST {

  private[bib] sealed trait Entry
  private[bib] sealed trait Value

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

object Parser {

  import AST._

  // this should return an Either or a custom error object instead of a useless "None"
  def parseString(input: String): Option[Document] = {
    val res = ParserImpl.parseAll(ParserImpl.bibTex, input)
    res.map(r => Some(Document(r))).getOrElse(None)
  }

  import scala.util.parsing.combinator._

  trait BibtexParser extends RegexParsers {
    override val skipWhitespace = false
    // FIXME: this should be '+' not '*' - go find places that rely on it being '+' and add a '?'
    def WS = r("\\s*")
    def BRACE_DELIMITED_STRING_NO_OUTER: Parser[String] =
      BRACE_DELIMITED_STRING ^^ (s => s.substring(1, s.length - 1))
    def BRACE_DELIMITED_STRING: Parser[String] =
      '{' ~> (BRACE_DELIMITED_STRING | """\\.""" | """[^}{]""").* <~ '}' ^^
      ("{" + _.mkString + "}")

    implicit def c(x: Char): Parser[Char] = accept(x)
    implicit def r(reg: String): Parser[String] = regex(reg.r)
  }

  private[bib] object ParserImpl extends BibtexParser {

    def bibTex =
      (freeComment ~! anyEntry ~! freeComment).+ ^^
      (_ flatMap { case x ~ y ~ z => List(x, y, z): List[Entry] })

    // fixme: lines starting with %%% are comments
    def freeComment = "[^@]*" ^^ (CommentEntry(_))

    def anyEntry = AT ~> (commentEntry | stringEntry | preambleEntry | regularEntry)

    def commentEntry =
      COMMENT ~> WS ~> (('{' ~> "[^}]*" <~ '}') | ('(' ~> "[^\\)]*" <~ ')')) ^^
      (CommentEntry(_))

    def stringEntry = STRING ~> WS ~> entryBody { tag } ^^ (StringEntry(_, _)).tupled

    def preambleEntry = PREAMBLE ~> WS ~> entryBody { value } ^^ (PreambleEntry(_))

    def regularEntry =
      (SYMBOL <~ WS) ~ entryBody { SYMBOL ~ rep(COMMA_WS ~> tag) <~ (COMMA_WS ?) } ^^ {
        case ty ~ (key ~ tags) => RegularEntry(ty, key, tags)
      }

    def entryBody[T](parser: => Parser[T]): Parser[T] = {
      lazy val p = parser
      ("\\{\\s*" ~> p <~ "\\s*\\}") |
      ("\\(\\s*" ~> p <~ "\\s*\\)")
    }

    def tag = (SYMBOL <~ "\\s*=\\s*") ~ value ^^ {
      case sym ~ value => (sym, value)
    }

    def value: Parser[Value] = literalOrSymbol ~ ("\\s*#\\s*" ~> value).? ^^ {
      case left ~ Some(right) => Concat(left, right)
      case left ~ _ => left
    }

    def literalOrSymbol = (SYMBOL ^^ (Abbrev(_))) | literal

    def literal = (numericLiteral | braceDelimitedNoOuterLiteral | quoteDelimitedLiteral)

    def numericLiteral = "\\d+(\\.\\d+)?" ^^ (Literal(_))
    def quoteDelimitedLiteral =
      '"' ~> (BRACE_DELIMITED_STRING | """[^"]""" | """\\.""").* <~ '"' ^^ (xs => Literal(xs.mkString))
    def braceDelimitedNoOuterLiteral = BRACE_DELIMITED_STRING_NO_OUTER ^^ (Literal(_))

    def AT = c('@')
    def COMMA_WS = r("\\s*,\\s*")
    def COMMENT = r("(c|C)(o|O)(m|M)(m|M)(e|E)(n|N)(t|T)")
    def STRING = r("(s|S)(t|T)(r|R)(i|I)(n|N)(g|G)")
    def PREAMBLE = r("(p|P)(r|R)(e|E)(a|A)(m|M)(b|B)(l|L)(e|E)")
    // can't start with a number, and no quotes, braces/parens, '#', commas, whitespace, or '='
    def SYMBOL = r("[^0-9\"}{)(,\\s#=][^\"}{)(,\\s#=]*")
  }
}

// Dom.scala //
object Dom {

  final case class Document(
    comments: List[String],
    preambles: List[String],
    entries: Map[String, Entry])

  final case class Entry(
    ty: String,
    citationKey: String,
    crossReference: Option[Entry],
    authors: Option[List[Names.Name]],
    editors: Option[List[Names.Name]],
    otherFields: Map[String, String])

  import annotation.tailrec

  def stringToDom(str: String, expandAbbreviations: Boolean = true): Option[Document] =
    Parser.parseString(str).map(astToDom(_, expandAbbreviations))

  def astToDom(astDoc: AST.Document, expandAbbreviations: Boolean = true): Document = {

    // NOTE: map has a default entry that passes things through unchanged
    // many fields (acknowledgement, etc) don't quote their string inputs so we should just pass thru
    // month abbreviations also are not really useful to expand
    val standardEnvironment = Map.empty[String, String].withDefault(identity)

    val emptyDocument = Document(Nil, Nil, Map.empty)

    def evalValue(value: AST.Value, env: Map[String, String]): String = value match {
      case AST.Literal(str) => str
      case AST.Abbrev(id) => if (expandAbbreviations) env(id) else id
      case AST.Concat(l, r) => evalValue(l, env) + evalValue(r, env)
    }
    // can crossref reference entries that haven't been created yet?
    // if so, we need to topological sort first (or do 2 passes)
    @tailrec def loop(
      currentDoc: Document = emptyDocument,
      astEntries: List[AST.Entry] = astDoc.entries,
      env: Map[String, String] = standardEnvironment
      ): Document = astEntries match {
      case Nil => currentDoc
      case entry :: rest => entry match {

        case AST.StringEntry(name, value) =>
          loop(currentDoc, rest, env + (name -> evalValue(value, env)))

        case AST.CommentEntry(comment) =>
          val newComments =
            if (comment.trim.isEmpty) currentDoc.comments
            else comment :: currentDoc.comments
          loop(currentDoc.copy(comments = newComments), rest, env)

        case AST.PreambleEntry(pre) =>
          loop(currentDoc.copy(preambles = evalValue(pre, env) :: currentDoc.preambles), rest, env)

        case AST.RegularEntry(ty, citationKey, tags) =>
          val evaldTags = tags.toMap.mapValues(evalValue(_, env))
          // FIXME: should "crossref"/"author"/"editor" all be case-insensitive?
          val crossRefEntry = for {
            referenceName <- evaldTags.get("crossref")
            referenceEntry <- currentDoc.entries.get(referenceName)
          } yield referenceEntry
          def namesForField(fieldName: String) =
            evaldTags.get(fieldName).map(Names.stringToNames(_)).toList.flatten
          val remainingTags = evaldTags - "crossref" - "author" - "editor"
          val authorNames = Some(namesForField("author"))
          val editorNames = Some(namesForField("editor"))
          val entry = Entry(ty, citationKey, crossRefEntry, authorNames, editorNames, remainingTags)
          loop(currentDoc.copy(entries = currentDoc.entries + (entry.citationKey -> entry)), rest, env)
      }
    }
    loop()
  }
}

// ParserTests.scala //
object ParserTests {

  import Parser.ParserImpl

  // check out this site for test data: http://ftp.math.utah.edu/pub/bibnet/subjects/
  // (BibNet is public domain)
  def main(args: Array[String]): Unit = {

    // I know, I know - these aren't real unit tests. Soon!
    println(ParserImpl.parseAll(ParserImpl.braceDelimitedNoOuterLiteral, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literal, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.value, "{Something Great}"))

    println(ParserImpl.parseAll(ParserImpl.quoteDelimitedLiteral, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.literal, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.value, "\"Something Great\""))

    println(ParserImpl.parseAll(ParserImpl.numericLiteral, "123"))
    println(ParserImpl.parseAll(ParserImpl.literal, "123"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "123"))
    println(ParserImpl.parseAll(ParserImpl.value, "123"))

    println(ParserImpl.parseAll(ParserImpl.SYMBOL, "asda5"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "asda5"))
    println(ParserImpl.parseAll(ParserImpl.value, "asda5"))

    println(ParserImpl.parseAll(ParserImpl.tag, "asda5 = { 132 as qwe  asd }"))

    println(ParserImpl.parseAll(ParserImpl.value, "asda5 # asda5"))

    println(ParserImpl.parseAll(ParserImpl.commentEntry, "comment{wooooo!}"))

    println(ParserImpl.parseAll(ParserImpl.preambleEntry, "preamble{wooooo}"))

    println(ParserImpl.parseAll(ParserImpl.stringEntry, "string{wooooo = 1231}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{wooooo = 1231}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{  wooooo  = {asd} }"))

    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{  wooooo  = {asd} }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@preamble{  wooooo}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@comment{  wooooo }"))

    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123 }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123, }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg =\"sdf13\", z = 123, }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry,
      """@florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.freeComment, "i am the king of the owrld!!"))
    println(ParserImpl.parseAll(ParserImpl.freeComment, """i am the king of the

    owrld!!"""))

    println(ParserImpl.parseAll(ParserImpl.WS ~> ParserImpl.anyEntry,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll((ParserImpl.WS ~> ParserImpl.anyEntry) +,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

      """
    ))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """
       Hi, everybody!

       @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

 free comments are coool
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }


      """))


    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """
          @article {mrx05,
          auTHor = "Mr. X",
          Title = {Something Great},
          publisher = "nob" # "ody",
          YEAR = 2005
          }
      """))

    println(ParserImpl.parseAll(
      ParserImpl.braceDelimitedNoOuterLiteral,
      "{Interannual Variability of planet-encircling dust activity on {M}ars}"))

    // this sample is from: http://amath.colorado.edu/documentation/LaTeX/reference/faq/bibstyles.html
    val coloradoSample = Parser.parseString(
      """

@string{jgr = "J.~Geophys.~Res."}

@MISC{primes,
   author = "Charles Louis Xavier Joseph de la Vall{\'e}e Poussin",
   note = "A strong form of the prime number theorem, 19th century",
   year = 1879
   }

@INBOOK{chicago,
   title = "The Chicago Manual of Style",
   publisher = "University of Chicago Press",
   edition = "Thirteenth",
   year = 1982,
   pages = "400--401",
   key = "Chicago"
   }

@BOOK{texbook,
   author = "Donald E. Knuth",
   title= "The {{\TeX}book}",
   publisher = "Addison-Wesley",
   year = 1984
   }

@BOOK{latexbook,
   author = "Leslie Lamport",
   title = "{\LaTeX \rm:} {A} Document Preparation System",
   publisher = "Addison-Wesley",
   year = 1986
   }

@UNPUBLISHED{btxdoc,
   author = "Oren Patashnik",
   title = "{Using BibTeX}",
   note = "Documentation for general BibTeX users",
   month = jan,
   year = 1988
   }

@UNPUBLISHED{btxhak,
   author = "Oren Patashnik",
   title = "Designing BibTeX Styles",
   note = "The part of BibTeX's documentation
                            that's not meant for general users",
   month = jan,
   year = 1988
   }

@BOOK{strunk,
   author = "Strunk, Jr., William and E. B. White",
   title = "The Elements of Style",
   publisher = "Macmillan",
   edition = "Third",
   year = 1979
   }

@book{vanleunen,
   title = "A Handbook for Scholars",
   author = "Mary-Claire van Leunen",
   publisher = "Knopf",
   year = "1979"
   }

@ARTICLE{Zurek:1993,
   AUTHOR  = {Zurek, R. W. and Martin, L. J.},
   TITLE   = {Interannual Variability of planet-encircling dust activity on {M}ars},
   YEAR    = {1993},
   JOURNAL = jgr,
   VOLUME  = {98},
   NUMBER  = {E2},
   PAGES   = {3247--3259}
}

@Article{Narendra_1990,
  author =       {K.S.Narendra and K.S.Parthsarathy},
  title =        {Identification and Control of Dynamical System
                  using Neural Networks},
  journal =      "IEENN",
  year =         {1990},
  volume =    {1},
  number =    {1},
  month =     {},
  pages =     {4-27},
  note =      {},
  annote =    {}
}


      """)
    println(coloradoSample)
    println(Dom.astToDom(coloradoSample.get))

    println(Names.stringToNames("Ludwig von Beethoven"))
    println(Names.stringToNames("von Beethoven, Ludwig"))
    println(Names.stringToNames("Jones, John Paul"))
    println(Names.stringToNames("John Paul Jones"))

    println(Names.stringToNames("John Paul Jones and Jones, John Paul"))
    println(Names.stringToNames("John Paul Jones and Ludwig von Beethoven"))

    println(Names.stringToNames("Charles Louis Xavier Joseph de la Vallee Poussin"))

    println(Names.stringToNames("{Barnes} {and} {Noble,} {Inc.}"))

    println(Names.stringToNames("Ralph Alpher and Bethe, Hans and George Gamow"))
    println(Names.stringToNames("K.S.Narendra"))

    println(Names.stringToNames("{\\e'}cole"))

    println(Names.stringToNames("John-Paul Jones and Bill Thompson"))

    println(Names.NameLexer.parseAll(Names.NameLexer.fragment_comma_or_ws +, "Bethe, Hans "))

    // here's a really tricky one (not a french word, i know)
    println(Names.stringToNames("{\\e'}col{\\e'}"))

    println(Names.NameLexer.parseAll(Names.NameLexer.fragment, "{\\e'}col{\\e'}"))

    println(Names.stringToNames("{hey ho lotsa stu\\}ff}"))
    println(Names.stringToNames("{Jean} {de la Fontaine du} {Bois Joli}"))
    println(Names.stringToNames("Jean de la Fontaine du Bois Joli"))


    val clx1 = Names.stringToNames("Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin").head
    println(clx1)
    val clx2 = Dom.stringToDom("@thing{asdf, author = \"Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin\"}")
      .get.entries.head._2.authors.get.head
    println(clx2)
    val clx3 = Dom.stringToDom("@thing{asdf, author = {Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin}}")
      .get.entries.head._2.authors.get.head
    println(clx3)

    println(clx1 == clx2 && clx2 == clx3)

    val ksn1 = Names.stringToNames("K.S.Narendra").head
    println(ksn1)
    val ksn2 = Dom.stringToDom("@thing{asdf, author = \"K.S.Narendra\"}")
      .get.entries.head._2.authors.get.head
    println(ksn2)
    val ksn3 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra}}")
      .get.entries.head._2.authors.get.head
    println(ksn3)
    val ksn4 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra and Hugh Jass}}")
      .get.entries.head._2.authors.get.head
    println(ksn4)

    println(ksn1 == ksn2 && ksn2 == ksn3 && ksn3 == ksn4)

    val fileText = scala.io.Source.fromFile("inputs/case-based-reasoning.bib.txt").mkString
    val res = Dom.stringToDom(fileText, false)
    //println(res)

    def timed[T](showTime: Long => String)(body: => T) = {
      val start = System.currentTimeMillis
      val result = body
      val time = showTime(System.currentTimeMillis - start)
      println(time)
      (result, time)
    }

    val filePath2 = "inputs/domain-decomp.bib.txt"
    val file2 = scala.io.Source.fromFile(filePath2).toArray
    val fileText2 = file2.mkString

    val numLines = file2.length
    val numMb = new java.io.File(filePath2).length / 1024.0 / 1024.0

    val (result, time) =
      timed(t =>
        "domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
        (numMb, numLines, t, (1000.0 * numMb) / t, (1000.0 * numLines) / t)) {
        Dom.stringToDom(fileText2, false)
      }

    //    println(result)
    println(time)
    val sizeMult = 10
    val bigtext = List.range(0, sizeMult).map(_ => fileText2).mkString
    val (bigresult, bigtime) =
      timed(t =>
        "%d times domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
        (sizeMult, numMb * sizeMult, numLines * sizeMult, t, (1000.0 * numMb * sizeMult) / t, (1000.0 * numLines * sizeMult) / t)) {
        Dom.stringToDom(bigtext, false)
      }
  }
}