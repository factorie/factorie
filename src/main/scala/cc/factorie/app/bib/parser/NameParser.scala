package cc.factorie.app.bib.parser

import scala.language.postfixOps

private[parser] object NameParser {

  import Dom.Name

  sealed trait Token
  case object AND extends Token
  case object COMMA extends Token
  case object HYPHEN extends Token
  case class TOKENLIST(ts: List[Token]) extends Token
  final case class FRAGMENT(text: String) extends Token

  def stringToNames(names: String): List[Name] =
    fragmentsToNames(lexNameFragments(names), names)

  private def fragmentsToNames(fragments: List[Token], origName: String): List[Name] =
    try {
      splitOn(fragments)(AND ==).map(fragmentsToName(_))
    } catch {
      case re: Exception => sys.error("Error parsing name \"%s\": %s" format (origName, re.toString))
    }

  private def fragmentsToName(fragments: List[Token]): Name = {
    val sectionTokens = splitOn(fragments)(COMMA ==)
    if (sectionTokens.flatten.length == 0)
      sys.error("\"%s\" must have at least one fragment between commas!" format fragments)
    val sections = sectionTokens.map(_ map {
      case FRAGMENT(f) => f
      case _ => sys.error("Only fragments should be left over after processing!")
    })
    val isVon: String => Boolean = _.filter(_.isLetter).headOption.exists(_.isLower)
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

  private def lexNameFragments(namesString: String): List[Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  private def flattenTokenLists(ts: List[Token]): List[Token] = ts flatMap {
    case TOKENLIST(inner) => flattenTokenLists(inner)
    case other => List(other)
  }

  // check out http://www.tug.org/TUGboat/tb27-2/tb87hufflen.pdf
  // TODO: write a prose description of the rules of name-parsing, as they are extremely complicated
  // FIXME: should hyphens make it to the later phase? what makes them different from plain old spaces?
  object NameLexer extends SharedParsers {

    lazy val nameLexer =
      WS ~> ((initial | fragment_comma_hyphen_or_ws) <~ WS).? ~
      ((and_ws | initial | hyphen | fragment_comma_hyphen_or_ws) <~ WS).* ~
      (fragment | initial).? ^^ {
        case pre ~ xs ~ post => flattenTokenLists(pre.toList ++ xs ++ post.toList).filterNot(HYPHEN ==)
      }

    lazy val fragment_comma_hyphen_or_ws =
      fragment ~ ((WS ~> (comma | hyphen) <~ WS) | "\\s+") ^^ {
        case frag ~ (_: String) => frag
        case frag ~ (punctuation: Token) => TOKENLIST(List(frag, punctuation))
      }

    lazy val and_ws = and <~ "\\s+"
    lazy val and = "and" ^^ (_ => AND)
    lazy val comma = "," ^^ (_ => COMMA)
    lazy val hyphen = ("-" | "~") ^^ (_ => HYPHEN)
    lazy val initial = "[A-Za-z]\\." ^^ (FRAGMENT(_))

    // if its just one fragment with curly braces, its a literal, so leave out the braces
    lazy val fragment =
      BRACE_DELIMITED_STRING_NO_OUTER.? ~ ("""\\.""" | "[^\\s,}{\\-~]" | BRACE_DELIMITED_STRING).* ^^ {
        case Some(bds) ~ Nil => bds
        case Some(bds) ~ rest => (("{" + bds + "}") :: rest).mkString
        case None ~ rest => rest.mkString
      } ^^ (FRAGMENT(_))

  }
}
