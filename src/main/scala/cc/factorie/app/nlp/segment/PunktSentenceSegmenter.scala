package cc.factorie.app.nlp.segment

import io.Source

sealed case class PunktToken(left: String, offset: Int, right: String, ty: TokenType)

sealed trait TokenType
case object S extends TokenType // Sentence boundary marker
case object A extends TokenType // Abbreviation marker
case object AS extends TokenType // Abbreviation at end of sentence marker
case object U extends TokenType // Unknown

class PunktSentenceSegmenter(text: String, abvSet: Set[String] = Set(), sentenceStarterSet: Set[String] = Set()) {

  val (tokens, tokenStrings) = getTokens(text)

  val TOKEN_MAP = tokens.groupBy(x => x.left.toLowerCase)
  /**
   * Stores the case-insensitive count of each distinct word
   * It counts a word and a word ending in a period as distinct
   */
  val wordCount = TOKEN_MAP.mapValues(_.length)
  val N_words = tokens.length

  def getTokens(text: String): (List[PunktToken], List[String]) = {
    val regex = """\w+'[\.\-\w]+|\w+[\.\-\w]*"""
    val matchIter = regex.r.findAllIn(text)
    val matchOffsets = scala.collection.mutable.ArrayBuffer[Int]()
    val matches = scala.collection.mutable.ArrayBuffer[String]()
    while (matchIter.hasNext) {
      matches += matchIter.next()
      matchOffsets += matchIter.start
    }
    val tokens = scala.collection.mutable.ArrayBuffer[PunktToken]()
    val tokenStrings = scala.collection.mutable.ArrayBuffer[String]()
    var m = 0
    while (m < matches.length) {
      val currentMatchUnsubbed = matches(m)
      val currentMatch = if (currentMatchUnsubbed.charAt(0).isDigit) replaceNumeric(currentMatchUnsubbed) else currentMatchUnsubbed
      tokens += PunktToken(currentMatch, matchOffsets(m),
        if (m == matches.length - 1) "" else matches(m + 1),
        if (abvSet(currentMatch)) A else U)
      tokenStrings += currentMatch
      m += 1
    }
    (tokens.toList, tokenStrings.toList)
  }

  /**
   * If the input string is a number it replaces the string with<br>
   * <b>##number##</b> maintaining any trailing period.
   * @param str A string that is a candidate for being a number
   * @return    The string <b>##number##</b> if the str is numeric.<br>
   *            Otherwise, str itself.
   */
  def replaceNumeric(str: String): String = {
    var temp = str
    try {
      if (temp.endsWith(".")) {
        temp = temp.substring(0, temp.length - 1)
        Some(temp.toDouble)
        "##number##."
      } else {
        Some(temp.toDouble)
        "##number##"
      }
    } catch {
      case _: java.lang.NumberFormatException => str
    }
  }

  def stripTrailingDot(str: String) = if (str.endsWith(".")) str.substring(0, str.length - 1) else str

  /**
   * <p> The type-based classiﬁcation stage employs
   * three characteristic properties of abbreviations:
   * <ol>
   * <li>Strong collocational dependency: Abbreviations always occur with a ﬁnal period.
   * <li>Brevity: Abbreviations tend to be short.
   * <li>Internal periods: Many abbreviations contain additional internal periods.
   * </ol>
   * </p>
   * <p>As these three characteristics do not change for
   * each individual instance of a type, we combine them in
   * a type-based approach to abbreviation detection.
   * </p>
   */
  class TypeBasedClassifier {
    val periodCount = wordCount.filter(_._1.endsWith(".")).foldLeft(0)((acc, kv) => acc + kv._2)
    val N = N_words + periodCount
    val p = periodCount.asInstanceOf[Double] / N

    val distinctWords = tokens.filterNot(_.ty == A).map(t => stripTrailingDot(t.left).toLowerCase).toSet

    val likelihoodRatios = distinctWords.filterNot(_.equals("##number##")).flatMap(w => {
      val i1 = wordCount.getOrElse(w, 0) //count of word without period
      val i2 = wordCount.getOrElse(w + ".", 0) //count of word with period
      if (i2 != 0) {
        val n1 = i1 + i2
        val k1 = i2
        val p1 = 0.99
        val fLength = 1 / math.exp(w.replaceAll( """\.""", "").length)
        val fPeriods = """\.""".r.findAllIn(w).length + 1 //number of internal periods
        //val fPenalty = 1.0/math.pow(w.replaceAll("""\.""","").length,i1)
        Seq(w -> logLikehoodRatioModified(n1, k1, p1, p) * fLength * fPeriods)
      }
      else Nil
    })

    def createTokens: List[PunktToken] = {
      val preProcessedList = tokens.filter(_.ty == A) //Words tagged as abvs using the precompiled list
      val numList = TOKEN_MAP.get("##number##.").map(_.map(_.copy(ty = U))).flatten.toList
      preProcessedList ::: numList ::: likelihoodRatios.flatMap(kv => {
        TOKEN_MAP.get(kv._1 + ".").get.map(_.copy(ty = if (kv._2 >= 0.3) A else S))
      }).toList
    }
  }

  class TokenBasedClassifier(tokenList: List[PunktToken]) {
    val UNDECIDED = 0
    val SENTENCE_BOUNDARY = 1
    val NO_SENTENCE_BOUNDARY = 2
    val sCount = tokenList.filter(_.ty == S).map(_.right).groupBy(x => x).mapValues(_.length)
    val aCount = tokenList.filter(_.ty == A).map(_.right).groupBy(x => x).mapValues(_.length)
    val tokenStringsCount = tokenStrings.groupBy(x => x).mapValues(_.length)
    val internalWords = tokenStringsCount.map(kv => {
      kv._1 -> (kv._2 - sCount.getOrElse(kv._1, 0) - aCount.getOrElse(kv._1, 0))
    })

    def decideOrthographic(token: String): Int = {
      if (token.charAt(0).isUpper)
        if (tokenStringsCount.getOrElse(token.toLowerCase, 0) != 0 && internalWords.getOrElse(token, 0) == 0)
          SENTENCE_BOUNDARY
        else
          UNDECIDED
      else if (tokenStringsCount.getOrElse(token.charAt(0).toUpper + token.substring(1), 0) != 0 || sCount.getOrElse(token, 0) == 0)
        NO_SENTENCE_BOUNDARY
      else
        UNDECIDED
    }

    def collocationHeuristic(tokenList: List[PunktToken]) = {
      val wordBigramCounts = tokens.groupBy({case PunktToken(l, _, r, _) => (stripTrailingDot(l), r)}).mapValues(_.length)
      tokenList.filter({
        case PunktToken(rawLeft, _, rawRight, A) =>
          val left = stripTrailingDot(rawLeft)
          val right = stripTrailingDot(rawRight)
          val i2 = wordCount.getOrElse(right.toLowerCase, 0) + wordCount.getOrElse(right.toLowerCase + ".", 0)
          val k1 = wordBigramCounts((left, rawRight))
          val k2 = i2 - k1
          val n1 = wordCount.getOrElse(left.toLowerCase, 0) + wordCount.getOrElse(left.toLowerCase + ".", 0)
          val n2 = N_words - n1
          val p1 = k1.asInstanceOf[Double] / n1
          val p2 = k2.asInstanceOf[Double] / n2
          val p = i2.asInstanceOf[Double] / N_words
          p1 > p && logLikehoodRatioDunning(n1, k1, p1, n2, k2, p2, p) >= 7.88
        case _ => false
      })
    }

    def freqSentenceStarterHeuristic(tokenList: List[PunktToken]) = {
      val N_S = tokenList.filter(_.ty == S).length
      val k1Cache = tokenList.groupBy(_.right).mapValues(_.filter(_.ty == S).length)
      tokenList.map(_.right).filter(rawRight => {
        val w = stripTrailingDot(rawRight)
        val i2 = wordCount.getOrElse(w.toLowerCase, 0) + wordCount.getOrElse(w.toLowerCase + ".", 0)
        val k1 = k1Cache(rawRight)
        val k2 = i2 - k1
        val p1 = k1.asInstanceOf[Double] / N_S
        val p2 = k2.asInstanceOf[Double] / N_words
        val p = i2.asInstanceOf[Double] / (N_S + N_words)
        p1 > p && logLikehoodRatioDunning(N_S, k1, p1, N_words, k2, p2, p) >= 30
      }) ++ sentenceStarterSet
    }

    def annotatedTokens: (Set[String], Iterable[PunktToken]) = {
      val mutableTokenList = collection.mutable.Map(tokenList.map(token => token.offset -> token): _*)
      var filteredList = tokenList.filterNot(token => token.left.equals("#number##.") || token.left.matches( """\p{L}\."""))
      val freqSentenceStarters = freqSentenceStarterHeuristic(filteredList).toSet
      collocationHeuristic(filteredList).filterNot(kv => freqSentenceStarters(kv.right)).foreach(colToken => {
        mutableTokenList.update(colToken.offset, colToken)
      })
      filteredList.foreach(token => {
        if (token.ty == A) {
          val decision = decideOrthographic(token.right)
          if (decision == SENTENCE_BOUNDARY || (token.right.charAt(0).isUpper && freqSentenceStarters(token.right)))
            mutableTokenList.update(token.offset, token.copy(ty = AS))
        }
      })
      filteredList = tokenList.filter(token => token.left.equals("##number##.") || token.left.matches( """\p{L}\."""))
      filteredList.foreach(token => {
        val decision = decideOrthographic(token.right)
        if (decision == NO_SENTENCE_BOUNDARY)
          mutableTokenList.update(token.offset, token.copy(ty = A))
        else if (!token.left.equals("##number##.") && decision == UNDECIDED && token.right.charAt(0).isUpper)
          mutableTokenList.update(token.offset, token.copy(ty = A))
      })
      val boundaries = mutableTokenList.values.toList.sortBy(_.offset)
      (freqSentenceStarters, boundaries)
    }
  }

  def logBino(n: Int, k: Int, p: Double): Double =
    if (p == 0 || p == 1) 0 else k * math.log(p) + (n - k) * math.log(1 - p)

  def logLikehoodRatioDunning(n1: Int, k1: Int, p1: Double, n2: Int, k2: Int, p2: Double, p: Double): Double =
    2 * (logBino(n1, k1, p1) + logBino(n2, k2, p2) - logBino(n1, k1, p) - logBino(n2, k2, p))

  def logLikehoodRatioModified(n1: Int, k1: Int, p1: Double, p: Double): Double =
    2 * (logBino(n1, k1, p1) - logBino(n1, k1, p))

  def createTypeBasedClassifier = new TypeBasedClassifier
  def createTokenBasedClassifier(tokenList: List[PunktToken]) = new TokenBasedClassifier(tokenList)
}

object PunktSentenceSegmenter {
  def findSentenceBoundaries(text: String, abvSet: Set[String] = Set[String]()): Iterable[(Int, TokenType)] = {
    val detector = new PunktSentenceSegmenter(text, abvSet)
    val tokenList = detector.createTypeBasedClassifier.createTokens
    val sentenceBoundaries = detector.createTokenBasedClassifier(tokenList)
      .annotatedTokens._2
      .filter(t => t.ty == AS || t.ty == S)
      .map(t => (t.offset + t.left.length, t.ty))
    Seq((0, S)) ++ sentenceBoundaries
  }

  def findCommonAbbreviations(text: String, abvSet: Set[String] = Set[String]()): Set[String] = {
    val detector = new PunktSentenceSegmenter(text, abvSet)
    val tokenList = detector.createTypeBasedClassifier.createTokens
    val abbrevs = detector.createTokenBasedClassifier(tokenList)
      .annotatedTokens._2
      .filter(t => t.ty == AS || t.ty == A)
      .map(_.left).toSet
    abbrevs
  }

  def findCommonSentenceStarters(text: String): Set[String] = {
    val detector = new PunktSentenceSegmenter(text)
    val tokenList = detector.createTypeBasedClassifier.createTokens
    detector.createTokenBasedClassifier(tokenList).annotatedTokens._1
  }

  def main(args: Array[String]): Unit = {
    val text = Source.fromFile( """C:\Users\Luke\Documents\Code\IESL\SentenceBoundaryDetector\wsj_text.txt""").getLines().mkString(" ")
    val start = System.currentTimeMillis()
    for (i <- 1 until 2)
      findSentenceBoundaries(text).foreach(println(_))
    println(System.currentTimeMillis() - start)
  }
}
