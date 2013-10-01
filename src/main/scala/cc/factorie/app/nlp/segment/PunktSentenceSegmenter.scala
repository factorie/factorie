package cc.factorie.app.nlp.segment

import io.Source
import annotation.tailrec
import java.util.regex.Pattern
import scala.collection._

sealed trait TokenType
case object S extends TokenType
// Sentence boundary marker
case object A extends TokenType
// Abbreviation marker
case object AS extends TokenType
// Abbreviation at end of sentence marker
case object U extends TokenType
// Unknown

object PunktSentenceSegmenter {

  object Punkt {

    val ORTHO_BEG_UC = 1
    val ORTHO_MID_UC = 1 << 2
    val ORTHO_UNK_UC = 1 << 3
    val ORTHO_BEG_LC = 1 << 4
    val ORTHO_MID_LC = 1 << 5
    val ORTHO_UNK_LC = 1 << 6
    val ORTHO_UC = ORTHO_BEG_UC | ORTHO_MID_UC | ORTHO_UNK_UC
    val ORTHO_LC = ORTHO_BEG_LC | ORTHO_MID_LC | ORTHO_UNK_LC

    def hasFlag(flagSet: Int, testFlag: Int): Boolean = (flagSet & testFlag) != 0

    sealed trait OrthoContext
    case object Initial extends OrthoContext
    case object Internal extends OrthoContext
    case object Unknown extends OrthoContext

    sealed trait Case
    case object Upper extends Case
    case object Lower extends Case
    case object Non extends Case

    val orthoMap = Map[(OrthoContext, Case), Int](
      (Initial, Upper) -> ORTHO_BEG_UC
      , (Internal, Upper) -> ORTHO_MID_UC
      , (Unknown, Upper) -> ORTHO_UNK_UC
      , (Initial, Lower) -> ORTHO_BEG_LC
      , (Internal, Lower) -> ORTHO_MID_LC
      , (Unknown, Lower) -> ORTHO_UNK_LC)

    class PunktLanguageVars {
      val sentenceEndChars = Set(".", "?", "!")
      def sentenceEndCharsRegex = "[%s]".format(Pattern.quote(sentenceEndChars.mkString))
      val internalPunctuation = ",:;"
      val boundaryRealignmentRegex = """(?s)["')\]}]+?(?:\s+|(?=--)|$)""".r
      val wordStartRegex = """[^\("\`{\[:;&\#\*@\)}\]\-,]"""
      val nonWordChars = """(?:[?!)";}\]\*:@'\({\[])"""
      val multiCharPunctuationRegex = """(?:\-{2,}|\.{2,}|(?:\.\s){2,}\.)"""
      val wordTokenizeTemplate = """(?x)(
            %2$s
                                   |
            (?=%3$s)\S+? # Accept word characters until end is found
            (?=          # Sequences marking a word's end
                \s|      # White-space
                $|       # End-of-string
                %1$s|%2$s| # Punctuation
                ,(?=$|\s|%1$s|%2$s) # Comma if at end of word
            )
                                   |
            \S
        )""" //.replaceAll("\\s+", "")

      lazy val wordTokenizerRegex = {
        val re = wordTokenizeTemplate.format(nonWordChars, multiCharPunctuationRegex, wordStartRegex)
//        println(re)
        re.r
      }

      def wordTokenize(s: String) = wordTokenizerRegex.findAllIn(s)

      val periodContextTemplate = """
            [^\s]*
            %2$s
            (?=(
                %1$s
                                    |
                \s+([^\s]+)
            ))""".replaceAll("\\s+", "")

      lazy val periodContextRegex = {
        val re = periodContextTemplate.format(nonWordChars, sentenceEndCharsRegex)
//        println(re)
        re.r
      }
    }

    val nonPunctuationRegex = """[^\W\d]""".r

    def iteratePairs[T](it: Iterable[T]): Iterable[(T, T)] = it.toSeq.sliding(2).filter(_.length > 1).map({case Seq(x, y) => (x, y)}).toIterable

    class PunktParameters {
      var abbrevTypes = mutable.Set[String]()
      var collocations = mutable.Set[(String, String)]()
      var sentenceStarters = mutable.Set[String]()
      var orthoContext = makeOrthoContext

      def makeOrthoContext = new mutable.HashMap[String, Int]() {
        override def default(key: String) = 0
      }

      def clearAbbrevs() = abbrevTypes = mutable.Set[String]()
      def clearCollocations() = collocations = mutable.Set[(String, String)]()
      def clearSentenceStarters() = sentenceStarters = mutable.Set[String]()
      def clearOrthoContext() = orthoContext = makeOrthoContext
      def addOrthoContext(typ: String, flag: Int) = orthoContext.update(typ, orthoContext(typ) | flag)
    }

    object PunktToken {
      val ellipsisRegex = """\.\.+$""".r
      val numericRegex = """^-?[\.,]?\d[\d,\.-]*\.?$""".r
      val initialRegex = """[^\W\d]\.$""".r
      val alphaRegex = """[^\W\d]+$""".r
    }

    class PunktToken(
      val token: String,
      var paraStart: Boolean = false,
      var lineStart: Boolean = false,
      var sentenceBreak: Boolean = false,
      var abbr: Boolean = false,
      var ellipsis: Boolean = false) {

      import PunktToken._

      val periodFinal = token.endsWith(".")

      def getType(tk: String) =
        if ( {val fst = tk(0); fst == '.' || fst == '-' || fst.isDigit})
          numericRegex.replaceAllIn(tk.toLowerCase, "##number##")
        else
          tk.toLowerCase

      val ty = getType(token)

      def typeNoPeriod = if (ty.length > 1 && ty.last == '.') ty.dropRight(1) else ty
      def typeNoSentPeriod = if (sentenceBreak) typeNoPeriod else ty
      def firstUpper = token(0).isUpper
      def firstLower = token(0).isLower
      def firstCase = if (firstUpper) Upper else if (firstLower) Lower else Non
      def isEllipsis = ellipsisRegex.pattern.matcher(token).matches
      def isNumber = ty.startsWith("##number##")
      def isInitial = initialRegex.pattern.matcher(token).matches
      def isAlpha = alphaRegex.pattern.matcher(token).matches
      def isNonPunctuation = nonPunctuationRegex.findFirstIn(ty).isDefined

      def serialize: String = sys.error("unimplemented")

      override def toString: String = {
        var res = token
        if (abbr) res += "<A>"
        if (ellipsis) res += "<E>"
        if (sentenceBreak) res += "<S>"
        res
      }
    }

    abstract class PunktBase(
      val languageVars: PunktLanguageVars = new PunktLanguageVars(),
      parms: PunktParameters = new PunktParameters()) {

      private[this] var p = parms

      def params_=(parms: PunktParameters) = p = parms
      def params = p

      def tokenizeWords(plainText: String): mutable.ArrayBuffer[PunktToken] = {
        val tokens = new mutable.ArrayBuffer[PunktToken]()
        var paraStart = false
        val lineIter = plainText.split('\n').iterator
        while (lineIter.hasNext) {
          val line = lineIter.next()
          val stripped = line.trim
          if (stripped.isEmpty) {
            paraStart = true
          } else {
            val lineTokens = languageVars.wordTokenize(line)
            val firstToken = lineTokens.next()
            if (firstToken != "")
              tokens += new PunktToken(firstToken, paraStart = paraStart, lineStart = true)
            paraStart = false
            while (lineTokens.hasNext) {
              val tk = lineTokens.next()
              if (tk != "")
                tokens += new PunktToken(tk)
            }
          }
        }
        tokens
      }

      def annotateFirstPass(tokens: Iterable[PunktToken]): Unit =
        tokens.foreach(firstPassAnnotation(_))

      def firstPassAnnotation(pt: PunktToken) = {
        val tok = pt.token
        if (languageVars.sentenceEndChars.contains(tok))
          pt.sentenceBreak = true
        else if (pt.isEllipsis)
          pt.ellipsis = true
        else if (pt.periodFinal && !tok.endsWith(".."))
          if (params.abbrevTypes.contains(tok.dropRight(1).toLowerCase) ||
              params.abbrevTypes.contains(tok.dropRight(1).toLowerCase.split("-").last))
            pt.abbr = true
          else
            pt.sentenceBreak = true
      }
    }

    class UnigramFreqDist extends mutable.HashMap[String, Int] {
      override def default(key: String) = 0
      def thresholdFreq(threshold: Int): UnigramFreqDist = {
        val res = new UnigramFreqDist
        var numRemoved = 0
        for ((tok, count) <- this) {
          if (count > threshold) numRemoved += 1
          else res(tok) += count
        }
        res(null) += numRemoved
        res
      }
    }

    class BigramFreqDist extends mutable.HashMap[(String, String), Int] {
      override def default(key: (String, String)) = 0
      def thresholdFreq(threshold: Int): BigramFreqDist = {
        val res = new BigramFreqDist
        var numRemoved = 0
        for ((tok, count) <- this) {
          if (count > threshold) numRemoved += 1
          else res(tok) += count
        }
        res(null) += numRemoved
        res
      }
    }

    class PunktTrainer(
      val trainText: Option[String] = None,
      val verbose: Boolean = false,
      languageVars: PunktLanguageVars = new PunktLanguageVars(),
      params: PunktParameters = new PunktParameters())
      extends PunktBase(languageVars, params) {

      var typeFreqDist = new UnigramFreqDist()
      var sentenceStarterFreqDist = new UnigramFreqDist()
      var collocationFreqDist = new BigramFreqDist()

      var numPeriodTokens = 0
      var sentenceBreakCount = 0
      var finalized = false

      val ABBREV = 0.3
      var IGNORE_ABBREV_PENALTY = false
      var ABBREV_BACKOFF = 5
      var COLLOCATION = 7.88
      var SENT_STARTER = 30
      var INCLUDE_ALL_COLLOCS = false
      var INCLUDE_ABBREV_COLLOCS = false
      var MIN_COLLOC_FREQ = 1

      if (trainText.isDefined) train(trainText.get, verbose, finalize = true)

      def train(text: String, verbose: Boolean = false, finalize: Boolean = true) = {
        trainTokensLogic(tokenizeWords(text), verbose)
        if (finalize) finalizeTraining(verbose)
      }

      def trainTokens(tokens: mutable.ArrayBuffer[PunktToken], verbose: Boolean = false, finalize: Boolean = true) = {
        trainTokensLogic(tokens, verbose)
        if (finalize) finalizeTraining(verbose)
      }

      private def trainTokensLogic(tokens: mutable.ArrayBuffer[PunktToken], verbose: Boolean = false, finalize: Boolean = true) = {
        finalized = false

        val tokIter = tokens.iterator
        while (tokIter.hasNext) {
          val tok = tokIter.next()
          typeFreqDist(tok.ty) += 1
          if (tok.periodFinal) numPeriodTokens += 1
        }

        val uniqueTypes = this.uniqueTypes(tokens)
        val reclassIter = reclassifyAbbrevTypes(uniqueTypes.toList).iterator
        while (reclassIter.hasNext) {
          val (abbr, score, isAdd) = reclassIter.next()
          if (score >= ABBREV) {
            if (isAdd) {
              params.abbrevTypes += abbr
              if (verbose) println(" Abbreviation (isAdd: %s): [%6.4f] %s" format(isAdd, score, abbr))
            }
          } else if (!isAdd) {
            params.abbrevTypes -= abbr
            if (verbose) println(" Removed abbreviation: [%6.4f] %s" format(score, abbr))
          }
        }

        annotateFirstPass(tokens)
        annotateOrthographyData(tokens)
        sentenceBreakCount += getSentenceBreakCount(tokens)

        val pairIter = iteratePairs(tokens).iterator
        while (pairIter.hasNext) {
          val (tok1, tok2) = pairIter.next()
          if (tok1.periodFinal) {
            if (isRareAbbrevType(tok1, tok2)) {
              params.abbrevTypes += tok1.typeNoPeriod
              if (verbose) println(" Rare Abbrev: %s" format tok1.ty)
            }
            if (isPotentialSentenceStarter(tok1, tok2))
              sentenceStarterFreqDist(tok2.ty) += 1
            if (isPotentialCollocation(tok1, tok2))
              collocationFreqDist((tok1.typeNoPeriod, tok2.typeNoSentPeriod)) += 1
          }
        }
      }

      def finalizeTraining(verbose: Boolean = false): Unit = {
        params.clearSentenceStarters()
        for ((ty, ll) <- findSentenceStarters()) {
          params.sentenceStarters += ty
          if (verbose) println(" Sent Starter: [%6.4f] %s" format(ll, ty))
        }
        params.clearCollocations()
        for (((ty1, ty2), ll) <- findCollocations()) {
          params.collocations += ((ty1, ty2))
          if (verbose) println(" Collocation: [%6.4f] %s+%s" format(ll, ty1, ty2))
        }
        finalized = true
      }

      def freqThreshold(orthoThreshold: Int = 2, typeThreshold: Int = 2, collocThreshold: Int = 2, sentenceStartThreshold: Int = 2) = {
        if (orthoThreshold > 1) {
          val oldOc = params.orthoContext
          params.clearOrthoContext()
          for ((tok, count) <- typeFreqDist; if count >= orthoThreshold)
            params.orthoContext(tok) = oldOc(tok)
        }

        typeFreqDist = typeFreqDist.thresholdFreq(typeThreshold)
        collocationFreqDist = collocationFreqDist.thresholdFreq(collocThreshold)
        sentenceStarterFreqDist = sentenceStarterFreqDist.thresholdFreq(sentenceStartThreshold)
      }

      def annotateOrthographyData(tokens: mutable.ArrayBuffer[PunktToken]): Unit = {
        var context: OrthoContext = Internal
        val tokenIter = tokens.iterator
        while (tokenIter.hasNext) {
          val tok = tokenIter.next()
          if (tok.paraStart && context != Unknown) context = Initial
          if (tok.lineStart && context == Internal) context = Unknown
          val flag = orthoMap.getOrElse((context, tok.firstCase), 0)
          if (flag != 0) params.addOrthoContext(tok.typeNoSentPeriod, flag)
          if (tok.sentenceBreak)
            if (!(tok.isNumber || tok.isInitial)) context = Initial
            else context = Unknown
          else if (tok.ellipsis || tok.abbr) context = Unknown
          else context = Internal
        }
      }

      def isRareAbbrevType(tok1: PunktToken, tok2: PunktToken): Boolean = {
        if (tok1.abbr || !tok1.sentenceBreak) return false
        val typ = tok1.typeNoSentPeriod
        val count = typeFreqDist(typ) + typeFreqDist(typ.dropRight(1))
        if (params.abbrevTypes.contains(typ) || count >= ABBREV_BACKOFF)
          return false
        if (languageVars.internalPunctuation.contains(tok2.token.take(1))) {
          return true
        } else if (tok2.firstLower) {
          val typ2 = tok2.typeNoSentPeriod
          val typ2OrthoContext = params.orthoContext(typ2)
          if (hasFlag(typ2OrthoContext, ORTHO_BEG_UC) && !hasFlag(typ2OrthoContext, ORTHO_MID_UC))
            return true
        }
        false
      }

      def isPotentialSentenceStarter(tok1: PunktToken, tok2: PunktToken): Boolean =
        tok1.sentenceBreak && !(tok1.isNumber || tok1.isInitial) && tok2.isAlpha

      def isPotentialCollocation(tok1: PunktToken, tok2: PunktToken): Boolean = {
        (INCLUDE_ALL_COLLOCS ||
         (INCLUDE_ABBREV_COLLOCS && tok1.abbr) ||
         (tok1.sentenceBreak &&
          (tok1.isNumber || tok1.isInitial))) &&
        tok1.isNonPunctuation &&
        tok2.isNonPunctuation
      }

      def findCollocations(): mutable.ArrayBuffer[((String, String), Double)] = {
        val collocations = new mutable.ArrayBuffer[((String, String), Double)]()
        val typeFreqDistN = sum(typeFreqDist.values)
        for (((typ1, typ2), colCount) <- collocationFreqDist; if !params.sentenceStarters.contains(typ2)) {
          val typ1Count = typeFreqDist(typ1) + typeFreqDist(typ1 + ".")
          val typ2Count = typeFreqDist(typ2) + typeFreqDist(typ2 + ".")
          if (typ1Count > 1 && typ2Count > 1 &&
              MIN_COLLOC_FREQ < colCount &&
              colCount <= math.min(typ1Count, typ2Count)) {
            val ll = colLogLikelihood(typ1Count, typ2Count, colCount, typeFreqDistN)
            if (ll >= COLLOCATION &&
                (typeFreqDistN: Double) / typ1Count > (typ2Count: Double) / colCount)
              collocations += (((typ1, typ2), ll))
          }
        }
        collocations
      }

      def sum(xs: Iterable[Int]): Int = {
        val iter = xs.iterator
        var sum = 0
        while (iter.hasNext) sum += iter.next()
        sum
      }

      def reclassifyAbbrevTypes(uniques: List[String]): List[(String, Double, Boolean)] = {
        val typeFreqDistN = sum(typeFreqDist.values)
        @tailrec def loop(
          uniques: List[String] = uniques,
          output: List[(String, Double, Boolean)] = List()): List[(String, Double, Boolean)] = uniques match {
          case curTokenType :: rest =>
            val isAdd = curTokenType.endsWith(".")
            if (!nonPunctuationRegex.findFirstIn(curTokenType).isDefined ||
                curTokenType == "##number##" ||
                (isAdd && params.abbrevTypes.contains(curTokenType)) ||
                (!isAdd && !params.abbrevTypes.contains(curTokenType)))
              loop(rest, output)
            else {
              val typ = if (isAdd) curTokenType.dropRight(1) else curTokenType
              val numPeriods = typ.count(".".==) + 1
              val numNonPeriods = typ.length - numPeriods + 1
              val countWithPeriod = typeFreqDist(typ + ".")
              val countWithoutPeriod = typeFreqDist(typ)
              val ll = dunningLogLikelihood(
                countWithPeriod + countWithoutPeriod,
                numPeriodTokens,
                countWithPeriod,
                typeFreqDistN)
              val fLength = math.exp(-numNonPeriods)
              val fPeriods = numPeriods
              val fPenalty = if (IGNORE_ABBREV_PENALTY) 1 else math.pow(numNonPeriods, -countWithoutPeriod)
              val score = ll * fLength * fPeriods * fPenalty
              loop(rest, (typ, score, isAdd) :: output)
            }
          case _ => output
        }
        loop()
      }

      def dunningLogLikelihood(countA: Int, countB: Int, countAB: Int, N: Int) = {
        val p1 = (countB: Double) / N
        val p2 = 0.99
        val nullHypo = (countAB: Double) * math.log(p1) + (countA - countAB) * math.log(1.0 - p1)
        val altHypo = (countAB: Double) * math.log(p2) + (countA - countAB) * math.log(1.0 - p2)
        val likelihood = nullHypo - altHypo
        -2.0 * likelihood
      }

      def colLogLikelihood(countA: Int, countB: Int, countAB: Int, N: Int) = {
        val p = (countB: Double) / N
        val p1 = (countAB: Double) / countA
        val p2 = (countB - countAB: Double) / (N - countA)
        val summand1 = countAB * math.log(p) + (countA - countAB) * math.log(1.0 - p)
        val summand2 = (countB - countAB) * math.log(p) + (N - countA - countB + countAB) * math.log(1.0 - p)
        val summand3 =
          if (countA == countAB) 0
          else countAB * math.log(p1) + (countA - countAB) * math.log(1.0 - p1)
        val summand4 =
          if (countB == countAB) 0
          else (countB - countAB) * math.log(p2) + (N - countA - countB + countAB) * math.log(1.0 - p2)
        val likelihood = summand1 + summand2 - summand3 - summand4
        -2.0 * likelihood
      }

      def findAbbrevTypes() = {
        params.clearAbbrevs()
        val tokens = typeFreqDist.keys.filter(ty => ty != null && ty.endsWith(".")).toList
        for ((abbr, score, isAdd) <- reclassifyAbbrevTypes(tokens); if score >= ABBREV)
          params.abbrevTypes += abbr
      }

      def uniqueTypes(tokens: Iterable[PunktToken]) = {
        val uniques = new mutable.HashSet[String]()
        val iter = tokens.iterator
        while (iter.hasNext)
          uniques += iter.next().ty
        uniques
      }

      def findSentenceStarters(): Iterable[(String, Double)] = {
        val typeFreqDistN = sum(typeFreqDist.values)
        for {
          (typ, typAtBreakCount) <- sentenceStarterFreqDist
          if typ != null
          typCount = typeFreqDist(typ) + typeFreqDist(typ + ".")
          if typCount >= typAtBreakCount
          ll = colLogLikelihood(sentenceBreakCount, typCount, typAtBreakCount, typeFreqDistN)
          if ll >= SENT_STARTER && (typeFreqDistN: Double) / sentenceBreakCount > (typCount: Double) / typAtBreakCount
        } yield (typ, ll)
      }

      def getSentenceBreakCount(tokens: Iterable[PunktToken]) = tokens.count(_.sentenceBreak)
    }

    class PunktSentenceTokenizer(
      val trainText: Option[String] = None,
      val verbose: Boolean = false,
      languageVars: PunktLanguageVars = new PunktLanguageVars,
      parms: PunktParameters = new PunktParameters()) extends PunktBase(languageVars, parms) {

      val PUNCTUATION = Set(";", ":", ",", ".", "!", "?")

      if (trainText != None) super.params_=(train(trainText.get, verbose))

      def train(trainText: String, verbose: Boolean = false) =
        new PunktTrainer(Some(trainText), verbose, languageVars, params).params

      def sentencesFromText(text: String, realignBoundaries: Boolean = false) = {
        var sents = slicesFromText(text).map({case (s1, s2, _) => text.substring(s1, s2)})
        if (realignBoundaries) sents = this.realignBoundaries(sents)
        sents
      }

      def annotateTokens(tokens: Iterable[PunktToken]): Iterable[PunktToken] = {
        annotateFirstPass(tokens)
        //        println(tokens)
        //        println(tokens.map(_.ty))
        annotateSecondPass(tokens)
        //        println(tokens)
        //        println(tokens.map(_.ty))
        tokens
      }

      def buildSentenceList(text: String, tokens: mutable.ArrayBuffer[PunktToken]): mutable.ArrayBuffer[String] = {
        val output = new mutable.ArrayBuffer[String]()
        var pos = 0
        val wsRegex = """\s*""".r
        var sentence = ""
        for (token <- tokens) {
          var tok = token.token
          val wsMatcher = wsRegex.pattern.matcher(text.substring(pos))
          val ws = if (wsMatcher.matches) wsMatcher.group(0) else ""
          pos += ws.length
          if (text.substring(pos, pos + tok.length) != tok) {
            val pat = tok.map(c => Pattern.quote(c.toString)).mkString( """\s*""")
            val m = pat.r.pattern.matcher(text.substring(pos))
            if (m.matches) tok = m.group(0)
          }

          pos += tok.length
          sentence += (if (sentence != "") ws + tok else tok)
          if (token.sentenceBreak) {
            output += sentence
            sentence = ""
          }
        }
        if (sentence != "") output += sentence
        output
      }

      def annotateSecondPass(tokens: Iterable[PunktToken]): Unit =
        for ((t1, t2) <- iteratePairs(tokens)) secondPassAnnotation(t1, t2)

      def secondPassAnnotation(tok1: PunktToken, tok2: PunktToken): Unit = {
        if (!tok1.periodFinal) return
        val typ = tok1.typeNoPeriod
        val nextType = tok2.typeNoSentPeriod
        val tokIsInitial = tok1.isInitial

        if (params.collocations.contains((typ, nextType))) {
          tok1.sentenceBreak = false
          tok1.abbr = true
          return
        }

        if ((tok1.abbr || tok1.ellipsis) && !tokIsInitial) {
          val isSentenceStarter = orthoHeuristic(tok2)
          if (isSentenceStarter.isDefined && isSentenceStarter.get) {
            tok1.sentenceBreak = true
            return
          }
          if (tok2.firstUpper && params.sentenceStarters.contains(nextType)) {
            tok1.sentenceBreak = true
            return
          }
        }

        if (tokIsInitial || typ == "##number##") {
          val isSentenceStarter = orthoHeuristic(tok2)
          if (isSentenceStarter.isDefined && !isSentenceStarter.get) {
            tok1.sentenceBreak = false
            tok1.abbr = true
            return
          }
          if (!isSentenceStarter.isDefined && tokIsInitial &&
              tok2.firstUpper &&
              !hasFlag(params.orthoContext(nextType), ORTHO_LC)) {
            tok1.sentenceBreak = false
            tok1.abbr = true
          }
        }
      }

      def orthoHeuristic(tok: PunktToken): Option[Boolean] = {
        if (PUNCTUATION.contains(tok.token))
          Some(false)
        else {
          val orthoContext = params.orthoContext(tok.typeNoSentPeriod)
          if (tok.firstUpper && hasFlag(orthoContext, ORTHO_LC) && !hasFlag(orthoContext, ORTHO_MID_UC))
            Some(true)
          else if (tok.firstLower && (hasFlag(orthoContext, ORTHO_UC) || !hasFlag(orthoContext, ORTHO_BEG_LC)))
            Some(false)
          else
            None
        }
      }

      def textContainsSentenceBreak(text: String): Option[PunktToken] = {
        val annotated = annotateTokens(tokenizeWords(text))
        //      println(annotated)
        annotated.dropRight(1).find(_.sentenceBreak)
      }

      def slicesFromText(text: String): mutable.ArrayBuffer[(Int, Int, TokenType)] = {
        var lastBreak = 0
        val output = new mutable.ArrayBuffer[(Int, Int, TokenType)]()
        val mIter = languageVars.periodContextRegex.findAllIn(text).matchData
        while (mIter.hasNext) {
          val m = mIter.next()
          val context = m.group(0) + m.group(1)
          //          println(context)
          val break = textContainsSentenceBreak(context)
          if (break.isDefined) {
            output += ((lastBreak, m.end, if (break.get.abbr) AS else S))
            lastBreak = if (m.groupNames.length > 2) m.start(2) else m.end
          }
        }
        output += ((lastBreak, text.length, S))
        output
      }

      def realignBoundaries(sents: mutable.ArrayBuffer[String]): mutable.ArrayBuffer[String] = {
        var realign = 0
        val output = new mutable.ArrayBuffer[String]()
        for ((s1Unfixed, s2) <- iteratePairs(sents)) {
          val s1 = s1Unfixed.substring(realign, s1Unfixed.length)
          val m = languageVars.boundaryRealignmentRegex.findFirstMatchIn(s2)
          if (m.isDefined) {
            output += (s1 + m.get.group(0).trim)
            realign = m.get.end
          } else {
            realign = 0
            output += s1
          }
        }
        output
      }

      def tokenize(text: String, realignBoundaries: Boolean = false) = sentencesFromText(text, realignBoundaries)

      def spanTokenize(text: String) = slicesFromText(text)
    }
  }

  import Punkt._

  def findSentenceBoundaries(text: String, abvSet: Set[String] = Set[String](), sentStarters: Set[String] = Set[String]()): Iterable[(Int, TokenType)] = {
    val params = new PunktParameters
    params.abbrevTypes ++= abvSet
    params.sentenceStarters ++= sentStarters
    val tokenizer = new PunktSentenceTokenizer(trainText = Some(text), verbose = true, parms = params)
    val sentenceBoundaries = tokenizer.slicesFromText(text).map({case (_, b, t) => (b, t)})
    Seq((0, S)) ++ sentenceBoundaries
  }

  def findCommonAbbreviations(text: String, abvSet: Set[String] = Set[String](), sentStarters: Set[String] = Set[String]()): Set[String] = {
    val params = new PunktParameters
    params.abbrevTypes ++= abvSet
    params.sentenceStarters ++= sentStarters
    val trainer = new PunktTrainer(trainText = Some(text), params = params)
    trainer.params.abbrevTypes
  }

  def findCommonSentenceStarters(text: String, abvSet: Set[String] = Set[String](), sentStarters: Set[String] = Set[String]()): Set[String] = {
    val params = new PunktParameters
    params.abbrevTypes ++= abvSet
    params.sentenceStarters ++= sentStarters
    val trainer = new PunktTrainer(trainText = Some(text), params = params)
    trainer.params.sentenceStarters
  }

  def main(args: Array[String]): Unit = {
    val text = scala.io.Source.fromFile( """C:\wsj_processed.txt""").getLines().mkString
    val params = new PunktParameters
    params.abbrevTypes ++= Set(
      "inc", "corp", "dec", "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "oct", "nov", "ala",
      "ariz", "ark", "colo", "conn", "del", "fla", "ill", "ind", "kans", "kan", "ken", "kent", "mass", "mich",
      "minn", "miss", "mont", "nebr", "neb", "nev", "dak", "okla", "oreg", "tenn", "tex", "virg", "wash", "wis",
      "wyo", "mr", "ms", "mrs", "calif", "oct", "vol", "rev", "ltd", "dea", "est", "capt", "hev", "gen", "ltd", "etc", "sci",
      "comput", "univ", "ave", "cent", "col", "comdr", "cpl", "dept", "dust,", "div", "est", "gal", "gov", "hon",
      "grad", "inst", "lib", "mus", "pseud", "ser", "alt", "Inc", "Corp", "Dec", "Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Ala", "Ariz", "Ark", "Colo", "Conn", "Del", "Fla", "Ill",
      "Ind", "Kans", "Kan", "Ken", "Kent", "Mass", "Mich", "Minn", "Miss", "Mont", "Nebr", "Neb", "Nev", "Dak",
      "Okla", "Oreg", "Tenn", "Tex", "Virg", "Wash", "Wis", "Wyo", "Mrs", "Calif", "Oct", "Vol", "Rev", "Ltd",
      "Dea", "Est", "Capt", "Hev", "Gen", "Ltd", "Etc", "Sci", "Comput", "Univ", "Ave", "Cent", "Col", "Comdr",
      "Cpl", "Dept", "Dust,", "Div", "Est", "Gal", "Gov", "Hon", "Grad", "Inst", "Lib", "Mus", "Pseud", "Ser", "Alt",
      "Mr", "Ms")
    val start = System.currentTimeMillis()
    for (i <- 1 to 5) {
      val tokenizer = new PunktSentenceTokenizer(trainText = Some(text), verbose = false, parms = params)
      //    tokenizer.params.abbrevTypes.foreach(println(_))
      val sfromt = tokenizer.sentencesFromText(text)
//      println(sfromt.length)
//      sfromt.foreach(println(_))
//      println(tokenizer.params.abbrevTypes)
    }
    println(System.currentTimeMillis() - start)
    //
    //    val text = Source.fromFile( """C:\Users\Luke\Documents\Code\IESL\SentenceBoundaryDetector\wsj_text.txt""").getLines().mkString(" ")
    //    val start = System.currentTimeMillis()
    //    for (i <- 1 until 2)
    //      findSentenceBoundaries(text).foreach(println(_))
    //    println(System.currentTimeMillis() - start)
  }
}
