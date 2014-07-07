package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp._
import scala.util.parsing.combinator.{ImplicitConversions, Parsers}
import cc.factorie.app.nlp.pos.PennPosTag
import scala.util.parsing.input.{Reader, Position}
import java.util.GregorianCalendar
import scala.collection.mutable.ArrayBuffer

/**
 * Finds and parses all kinds of dates in a document, Basic formats were taken from http://en.wikipedia.org/wiki/Calendar_date.
 * DeterministicTokenizer was used as tokenizer as basis for the implementation.
 * Implementation is based on scalas Parsers Combinators
 * @author Dirk Weissenborn
 */
object DatePhraseFinder extends DocumentAnnotator with Parsers with ImplicitConversions {
  type Elem = Token

  def prereqAttrs = List(classOf[PennPosTag])

  def postAttrs: Iterable[Class[_]] = List()

  implicit val err: (Elem) => String = _.lemmaString + "was unexpected!"

  implicit def toInt(tokenAndInt: (Token, Int)) = tokenAndInt._2

  implicit def toToken(tokenAndInt: (Token, Int)) = tokenAndInt._1

  val monthToNr = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec".toLowerCase.split("\\|").zipWithIndex.toMap.mapValues(_ + 1)
  val weekDayNr = "Mon|Tue|Wed|Thu|Fri|Sat|Sun".toLowerCase.split("\\|").zipWithIndex.toMap

  val nrToMonth = Array.ofDim[String](monthToNr.size);
  monthToNr.foreach(el => nrToMonth(el._2 - 1) = el._1)
  val nrToWeekDay = Array.ofDim[String](weekDayNr.size);
  weekDayNr.foreach(el => nrToWeekDay(el._2) = el._1)

  val dayOfMonthRegex = "([1-3]0|[0-3]?1(st)?|[0-2]?(2(nd)?|3(rd)?|[4-9](th)?))"
  val monthAbbrRegex: String = "(Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?"
  val monthRegex = "(January|February|March|April|May|June|July|August|September|October|November|December)"

  val monthAbbr: Parser[Token] = acceptIf(_.string.toLowerCase.matches(monthAbbrRegex.toLowerCase))(err)

  val monthParser: Parser[Token] = acceptIf(_.string.toLowerCase.matches(monthRegex.toLowerCase))(err)
  val monthNumber: Parser[Token] = "0?[1-12]"
  val monthDayNumber: Parser[(Token, Int)] = hasString(dayOfMonthRegex) ^^ { case dayToken =>
    (dayToken, dayToken.string.replaceAll("[sthrnd]+", "").toInt)
  }

  val weekDayAbbr: Parser[Token] = acceptIf(_.string.toLowerCase.matches("(Mon|Tue|Tues|Wed|Thu|Thurs|Fri)\\.?".toLowerCase))(err)
  val weekDay: Parser[(Token, Int)] = (weekDayAbbr | acceptIf(_.string.toLowerCase.matches("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday".toLowerCase))(err)) ^^ {
    case weekDayToken => (weekDayToken, weekDayNr(weekDayToken.string.take(3).toLowerCase))
  }

  val temporalPreps: Parser[Token] = hasLemma("in|from|to|until|since")
  val digits: Parser[(Token, Int)] = acceptIf(_.isDigits)(err) ^^ { case t => (t, t.string.toInt)}
  val bcAd: Parser[Token] = "B\\.?C\\.?|A\\.?D\\.?"
  val year: Parser[(Token, Int)] = bcAd.? ~ digits ~ bcAd.? ^^ { case bcAdOpt1 ~ y ~ bcAdOpt2 =>
    val bcAdOpt = if (bcAdOpt1.isDefined) bcAdOpt1 else bcAdOpt2
    (y._1,
      if (bcAdOpt.isDefined)
        if (bcAdOpt.exists(_.string.startsWith("B"))) -y
        else y._2
      else normalizeYear(y._2))
  }

  def hasLemma(lemmaRegex: String): Parser[Token] = acceptIf(_.lemmaString.matches(lemmaRegex))(err)

  implicit def hasString(stringRegex: String): Parser[Token] = acceptIf(_.string.matches(stringRegex))(err)

  val simpleSep = hasString("[\\-/]")


  val yearOnly = (temporalPreps | hasLemma("year")) ~> year <~ acceptIf(!_.posTag.isNoun)(err) ^^ { case y => new DatePhrase(y._1, year = y)}
  val onlyMonth: Parser[(Token, Int)] = (monthParser | monthAbbr) ^^ { case monthToken => (monthToken, monthToNr(monthToken.string.substring(0, 3).toLowerCase))}
  val monthOnly: Parser[DatePhrase] = onlyMonth ^^ { case m => new DatePhrase(m._1, month = m._2)}


  def l(t1: Token, t2: Token) = t2.positionInSection - t1.positionInSection + 1

  // Real date parsers with Date as output
  val monthYear = onlyMonth ~ hasLemma("of|,|/").? ~ year ^^ { case m ~ _ ~ y => new DatePhrase(m._1, length = l(m._1, y._1), month = m, year = y)}
  val yearMonth = year ~ hasLemma(",").? ~ onlyMonth ^^ { case y ~ _ ~ m => new DatePhrase(y._1, length = l(y._1, m._1), month = m, year = y)}

  def normalizeYear(y: Int) = {
    if (y < 100)
      if (y < 20) y + 2000
      else y + 1900
    else y
  }

  //DeterministicTokenizer adaptions
  //2003-11-09
  val yearMonthDayFromTokenNr = "(19|20)?[0-9]{2}[\\-/][0-3]?[0-9][\\-/][0-3]?[0-9]" ^^ { case ymdToken =>
    val split = ymdToken.string.split("\\-|/")
    val y = normalizeYear(split(0).toInt)
    new DatePhrase(ymdToken, year = y, month = split(1).toInt, day = split(2).toInt)
  }
  //the US way
  //11-29-2003
  val monthDayYearFromTokenNr = "0?[1-12][\\-/][0-3]?[0-9][\\-/][0-9]{2,}" ^^ { case mdyToken =>
    val split = mdyToken.string.split("\\-|/")
    val y = normalizeYear(split(2).toInt)
    new DatePhrase(mdyToken, year = y, month = split(0).toInt, day = split(1).toInt)
  }

  //2003 November 9, 2003-Nov-9, 2003-Nov-09, 2003-Nov-9, Sunday
  val yearMonthDay = year ~ simpleSep.? ~ onlyMonth ~ simpleSep.? ~ monthDayNumber ~ ("," ~ weekDay).? ^^ { case y ~ _ ~ m ~ _ ~ d ~ wOpt =>
    val w = wOpt.fold(-1)(_._2._2)
    new DatePhrase(y._1, length = l(y._1, wOpt.fold(d._1)(_._2._1)), month = m, day = d, year = y, weekDay = w)
  }
  //due to tokenization of DeterministicTokenizer (factorie) a small adaption
  val yearMonthDayAdap = year ~ "-".? ~ s"($monthRegex|$monthAbbrRegex)-?$dayOfMonthRegex" ~ ("," ~ weekDay).? ^^ { case y ~ _ ~ mdToken ~ wOpt =>
    val w = wOpt.fold(-1)(_._2._2)
    val split = if (mdToken.string.contains("-")) {
      val Array(a, b) = mdToken.string.split("-"); (a, b)
    } else mdToken.string.splitAt(mdToken.string.indexWhere(_.isDigit))
    val m = monthToNr(split._1.substring(0, 3).toLowerCase)
    val d = split._2.toInt
    new DatePhrase(y._1, length = l(y._1, wOpt.fold(mdToken)(_._2._1)), month = m, day = d, year = y, weekDay = w)
  }

  //2003Nov9, 2003Nov09
  val yearMonthDayString = year ~ hasString(monthAbbrRegex + dayOfMonthRegex) ^^ { case y ~ mdToken =>
    val dStart = mdToken.string.indexWhere(_.isDigit)
    val m = monthToNr(mdToken.string.substring(0, dStart).take(3).toLowerCase)
    new DatePhrase(y._1, length = 2, year = y._2, month = m, day = mdToken.string.substring(dStart).toInt)
  }

  //Sunday, November 9, 2003; November 9, 2003; Nov. 9, 2003; November 9
  val monthDayYear = (weekDay ~ ",".?).? ~ onlyMonth ~ monthDayNumber ~ (",".? ~ year).? ^^ {
    case wdOpt ~ m ~ d ~ yOpt =>
      val startToken = wdOpt.fold(m._1)(_._1._1)
      val endToken = yOpt.fold(d._1)(_._2._1)
      new DatePhrase(startToken, length = l(startToken, endToken), day = d, month = m, year = yOpt.fold(Int.MinValue)(_._2._2), weekDay = wdOpt.fold(-1)(_._1._2))
  }

  //08-Nov-2003, [The] 8th [of] November 2003, 08/Nov/2003, Sunday, 8 November 2003
  val dayMonthYear = (weekDay ~ ",".?).? ~ "the".? ~ monthDayNumber ~ "of".? ~ onlyMonth ~ (",".? ~ year).? ^^ {
    case wdOpt ~ _ ~ d ~ _ ~ m ~ yOpt =>
      val startToken = wdOpt.fold(d._1)(_._1._1)
      val endToken = yOpt.fold(m._1)(_._2._1)
      new DatePhrase(startToken, length = l(startToken, endToken), day = d, month = m, year = yOpt.fold(Int.MinValue)(_._2._2), weekDay = wdOpt.fold(-1)(_._1._2))
  }

  //due to tokenization of DeterministicTokenizer (factorie) a small adaption
  val dayMonthYearAdap = monthDayNumber ~ "-".? ~ s"($monthRegex|$monthAbbrRegex)-?[0-9]{2,}" ~ ("," ~ weekDay).? ^^ { case d ~ _ ~ myToken ~ wOpt =>
    val w = wOpt.fold(-1)(_._2._2)
    val split = if (myToken.string.contains("-")) {
      val Array(a, b) = myToken.string.split("-"); (a, b)
    } else myToken.string.splitAt(myToken.string.indexWhere(_.isDigit))
    val m = monthToNr(split._1.substring(0, 3).toLowerCase)
    val y = split._2.toInt
    new DatePhrase(d._1, length = l(d._1, wOpt.fold(myToken)(_._2._1)), month = m, day = d._2, year = y, weekDay = w)
  }

  //9Nov2003
  val dayMonthYearString = monthDayNumber ~ hasString(dayOfMonthRegex + monthAbbrRegex + "[0-9]{4}") ^^ { case d ~ myToken =>
    val yStart = myToken.string.indexWhere(_.isDigit)
    val y = normalizeYear(myToken.string.substring(yStart).toInt)
    val m = monthToNr(myToken.string.substring(0, yStart).take(3).toLowerCase)
    new DatePhrase(d, length = 2, year = y, month = m, day = d._2)
  }

  //order is important
  val parser: Parser[DatePhrase] = dayMonthYear | monthDayYear | yearMonthDay | yearMonthDayAdap | yearMonthDayString | dayMonthYearAdap | dayMonthYearString | monthDayYearFromTokenNr | yearMonthDayFromTokenNr | yearOnly | monthOnly

  def reader(ts: Iterable[Token]): scala.util.parsing.input.Reader[Token] = new scala.util.parsing.input.Reader[Token] {
    override def first: Token = ts.head

    override def atEnd: Boolean = ts.isEmpty

    override def pos: Position = new Position {
      override def column: Int = if (atEnd) Int.MaxValue else first.position

      override def line: Int = 0

      override protected def lineContents: String = ""
    }

    override def rest: Reader[Token] = if (atEnd) reader(ts) else reader(ts.tail)
  }

  def process(document: Document) = {
    val mentions = parseAll(document.tokens)
    document.attr += new DatePhraseList(mentions)
    document
  }


  override def tokenAnnotationString(token: Token): String = token.document.attr[DatePhraseList].find(phrase => phrase.contains(token)).fold("")("Date: " + _.asInstanceOf[DatePhrase].toString())


  /** A collection of Phrases that are noun phrases.  Typically used as an attribute of a Section or a Document. */
  class DatePhraseList(phrases: Iterable[DatePhrase]) extends PhraseList(phrases)

  class DatePhrase(startToken: Token, length: Int = 1, val day: Int = -1, val month: Int = -1, val year: Int = Int.MinValue, val weekDay: Int = -1)
    extends Phrase(startToken.section, startToken.positionInSection, length, -1) {

    def toJavaDate: java.util.Date = new GregorianCalendar(year, month, day).getTime

    override def toString(): String = {
      var s = ""
      if (weekDay >= 0) s += nrToWeekDay(weekDay) + ", "
      if (day >= 0) s += day + " "
      if (month >= 0) s += nrToMonth(month - 1) + " "
      if (year >= 0) s += year
      s.trim
    }
  }

  def parseAll(tokens: Iterable[Token]) = {
    var r = reader(tokens)
    val mentions = ArrayBuffer[DatePhrase]()

    while (r != null && !r.atEnd)
      parser.apply(r) match {
        case Success(s, rest) =>
          mentions += s
          r = rest
        case Failure(_, rest) => r = rest.rest
        case Error(_, rest) => r = rest.rest
      }

    mentions
  }

}
