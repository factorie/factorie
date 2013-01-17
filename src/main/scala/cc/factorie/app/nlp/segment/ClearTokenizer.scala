package cc.factorie.app.nlp.segment

import scala.collection.mutable
import java.util.regex.Pattern
import jregex.{Replacer, TextBuffer, MatchResult, Substitution}
import cc.factorie.app.nlp.{DocumentProcessor, Document, Token, Sentence}

object ClearTokenizer extends EnglishTokenizer(EnglishTokenizerConfig.default)

object ClearSegmenter extends EnglishSegmenter(ClearTokenizer)

abstract class AbstractSegmenter(tokenizer: AbstractTokenizer) extends DocumentProcessor {
  def process(d: Document): Document = {
    val sentences = getSentences(d.string)
    var offset = 0
    for (ct <- sentences.flatten) {
      val substrStart = d.string.indexOf(ct.text, offset)
      val substrEnd = substrStart + ct.text.length
      val tok = new Token(d, substrStart, substrEnd)
      offset = substrEnd
    }
    var start = 0
    for (cs <- sentences; if cs.length > 0) {
      new Sentence(d, start, cs.length)
      start = start + cs.length
    }
    d
  }
  /**
   * Returns a list of sentences, which are arrays of string tokens, from the specific reader.
   * @param fin the reader to retrieve sentences from.
   * @return a list of sentences, which are arrays of string tokens, from the specific reader.
   */
  def getSentences(fin: String): mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]]
}

/**
 * @since 1.1.0
 * @author Jinho D. Choi ({ @code jdchoi77@gmail.com})
 */
class EnglishSegmenter(val tokenizer: AbstractTokenizer) extends AbstractSegmenter(tokenizer) {
  /** Patterns of terminal punctuation. */
  protected val P_TERMINAL_PUNCTUATION = Pattern.compile("^(\\.|\\?|!)+$")
  protected val L_BRACKETS = Array("\"", "(", "{", "[")
  protected val R_BRACKETS = Array("\"", ")", "}", "]")


  def getSentences(fin: String): mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]] = {
    val sentences = new mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]]()
    val tokens = tokenizer.getTokenList(fin)
    val brackets = new Array[Int](R_BRACKETS.length)
    var bIdx, size = tokens.size
    var isTerminal = false
    var curr: ClearToken = null

    def body(i: Int): Unit = {
      curr = tokens(i)
      countBrackets(curr.text, brackets)
      if (isTerminal || P_TERMINAL_PUNCTUATION.matcher(curr.text).find()) {
        if (i + 1 < size && isFollowedByBracket(tokens(i + 1).text, brackets)) {
          isTerminal = true
          return
        }
        sentences += tokens.slice(bIdx, {bIdx = i + 1; bIdx})
        isTerminal = false
      }
    }

    (0 until size).foreach(body)
    sentences
  }

  private def countBrackets(str: String, brackets: Array[Int]): Unit =
    if (str.equals("\""))
      brackets(0) += (if (brackets(0) == 0) 1 else -1)
    else {
      val size = brackets.length
      for (i <- 1 until size)
        if (str.equals(L_BRACKETS(i)))
          brackets(i) += 1
        else if (str.equals(R_BRACKETS(i)))
          brackets(i) -= 1
    }

  private def isFollowedByBracket(str: String, brackets: Array[Int]): Boolean = {
    val size = R_BRACKETS.length
    for (i <- 0 until size)
      if (brackets(i) > 0 && str.equals(R_BRACKETS(i)))
        return true
    false
  }
}

abstract class AbstractTokenizer extends DocumentProcessor {
  def process(d: Document): Document = {
    var offset = 0
    for (ct <- getTokenList(d.string)) {
      val substrStart = d.string.indexOf(ct.text, offset)
      val substrEnd = substrStart + ct.text.length
      new Token(d, substrStart, substrEnd)
      offset = substrEnd
    }
    d
  }

  def getTokenList(str: String): mutable.ArrayBuffer[ClearToken]
}

class ClearToken(var text: String, var protect: Boolean) {}

class CompoundSpan(val start: Int, val end: Int) {}

/**
 * @since 1.1.0
 * @author Jinho D. Choi ({ @code jdchoi77@gmail.com})
 */
class EnglishTokenizer(zin: EnglishTokenizerConfig) extends AbstractTokenizer {
  val WHITE_SPAN = Pattern.compile("\\s+")
  val URL_SPAN = new jregex.Pattern("((([A-Za-z]{3,9}:(?:\\/\\/)?)(?:[-;:&=\\+\\$,\\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\\+\\$,\\w]+@)[A-Za-z0-9.-]+)((?:\\/[\\+~%\\/.\\w-_]*)?\\??(?:[-\\+=&;%@.\\w_]*)#?(?:[.\\!\\/\\\\w]*))?|(\\w+\\.)+(com|edu|gov|int|mil|net|org|biz)$)");
  val FILE_EXTS = Pattern.compile("\\S+\\.(3gp|7z|ace|ai(f){0,2}|amr|asf|asp(x)?|asx|avi|bat|bin|bmp|bup|cab|cbr|cd(a|l|r)|chm|dat|divx|dll|dmg|doc|dss|dvf|dwg|eml|eps|exe|fl(a|v)|gif|gz|hqx|(s)?htm(l)?|ifo|indd|iso|jar|jsp|jp(e)?g|lnk|log|m4(a|b|p|v)|mcd|mdb|mid|mov|mp(2|3|4)|mp(e)?g|ms(i|wmm)|ogg|pdf|php|png|pps|ppt|ps(d|t)?|ptb|pub|qb(b|w)|qxd|ra(m|r)|rm(vb)?|rtf|se(a|s)|sit(x)?|sql|ss|swf|tgz|tif|torrent|ttf|txt|vcd|vob|wav|wm(a|v)|wp(d|s)|xls|xml|xtm|zip)$")

  protected val S_DELIM = " "
  protected val S_PROTECTED = "PR0T_"
  protected val S_D0D = "_DPPD_"
  protected val S_HYPHEN = "_HYYN_"
  protected val S_AMPERSAND = "_APSD_"
  protected val S_APOSTROPHY = "_AOOR_"
  protected val N_PROTECTED = S_PROTECTED.length()

  protected val P_DELIM = Pattern.compile(S_DELIM)
  protected val P_HYPHEN = Pattern.compile("-")
  protected val P_ABBREVIATION = Pattern.compile("^(\\p{Alpha}\\.)+\\p{Alpha}?$")
  protected val A_D0D = Array(".", ",", ":", "-", "/", "'")

  protected var R_URL: Replacer = null
  protected var R_ABBREVIATION: Replacer = null
  protected var R_PERIOD_LIKE: Replacer = null
  protected var R_MARKER: Replacer = null
  protected var R_APOSTROPHY: Replacer = null
  protected var R_USDOLLAR: Replacer = null
  protected var R_AMPERSAND: Replacer = null
  protected var R_WAW: Replacer = null
  protected var R_PUNCTUATION_PRE: Replacer = null
  protected var R_PUNCTUATION_POST: Replacer = null
  protected var R_D0D: Array[Replacer] = null
  protected var R_UNIT: Array[Replacer] = null

  protected var T_EMOTICONS: mutable.Set[String] = null
  protected var T_ABBREVIATIONS: mutable.Set[String] = null
  protected var P_HYPHEN_LIST: Pattern = null
  protected var M_D0D: mutable.HashMap[String, Int] = null
  protected var M_COMPOUNDS: mutable.HashMap[String, Int] = null
  protected var L_COMPOUNDS: mutable.ArrayBuffer[Array[CompoundSpan]] = null
  protected var P_RECOVER_D0D: Array[Pattern] = null
  protected var P_RECOVER_DOT: Pattern = null
  protected var P_RECOVER_HYPHEN: Pattern = null
  protected var P_RECOVER_APOSTROPHY: Pattern = null
  protected var P_RECOVER_AMPERSAND: Pattern = null

  initReplacers()
  initMapsD0D()
  initPatterns()
  initDictionaries(zin)

  def getTokenList(str: String): mutable.ArrayBuffer[ClearToken] = {
    var lTokens = tokenizeWhiteSpaces(str)

    protectEmoticons(lTokens)
    lTokens = tokenizePatterns(lTokens, R_URL)
    lTokens = tokenizePatterns(lTokens, R_ABBREVIATION)
    lTokens = tokenizePatterns(lTokens, R_PERIOD_LIKE)
    lTokens = tokenizePatterns(lTokens, R_MARKER)
    lTokens = tokenizePatterns(lTokens, R_USDOLLAR)
    for (r <- R_D0D) replaceProtects(lTokens, r)
    replaceHyphens(lTokens)
    lTokens = tokenizePatterns(lTokens, R_PUNCTUATION_PRE)
    protectAbbreviations(lTokens)
    protectFilenames(lTokens)


    lTokens = tokenizeCompounds(lTokens)
    lTokens = tokenizePatterns(lTokens, R_APOSTROPHY)
    replaceProtects(lTokens, R_AMPERSAND)
    replaceProtects(lTokens, R_WAW)
    for (r <- R_UNIT) lTokens = tokenizePatterns(lTokens, r)
    lTokens = tokenizePatterns(lTokens, R_PUNCTUATION_POST)

    val size = P_RECOVER_D0D.length
    for (i <- 0 until size) recoverPatterns(lTokens, P_RECOVER_D0D(i), A_D0D(i))
    recoverPatterns(lTokens, P_RECOVER_HYPHEN, "-")
    recoverPatterns(lTokens, P_RECOVER_APOSTROPHY, "'")
    recoverPatterns(lTokens, P_RECOVER_AMPERSAND, "&")

    lTokens
  }

  private def initReplacers(): Unit = {
    R_URL = URL_SPAN.replacer(new SubstitutionOne())
    R_ABBREVIATION = new jregex.Pattern("(^(\\p{Alpha}\\.)+)(\\p{Punct}*$)").replacer(new SubstitutionOnePlus())
    R_PERIOD_LIKE = new jregex.Pattern("(\\.|\\?|\\!){2,}").replacer(new SubstitutionOne())
    R_MARKER = new jregex.Pattern("\\-{2,}|\\*{2,}|\\={2,}|\\~{2,}|\\,{2,}|\\`{2,}|\\'{2,}").replacer(new SubstitutionOne())
    R_APOSTROPHY = new jregex.Pattern("(?i)((\\')(s|d|m|ll|re|ve|nt)|n(\\')t)$").replacer(new SubstitutionOne())
    R_USDOLLAR = new jregex.Pattern("^US\\$").replacer(new SubstitutionOne())
    R_AMPERSAND = replacerAmpersand
    R_WAW = replacerWAWs

    R_PUNCTUATION_PRE = new jregex.Pattern("\\(|\\)|\\[|\\]|\\{|\\}|<|>|\\,|\\:|\\;|\\\"").replacer(new SubstitutionOne())
    R_PUNCTUATION_POST = new jregex.Pattern("\\.|\\?|\\!|\\`|\\'|\\-|\\/|\\@|\\#|\\$|\\%|\\&|\\|").replacer(new SubstitutionOne())

    initReplacersD0Ds()
  }

  def replacerAmpersand: Replacer =
    new jregex.Pattern("(\\p{Upper})(\\&)(\\p{Upper})").replacer(new Substitution {
      def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
        dest.append(m.group(1))
        dest.append(S_AMPERSAND)
        dest.append(m.group(3))
      }
    })

  def replacerWAWs: Replacer =
    new jregex.Pattern("(\\w)(\\')(\\w)").replacer(new Substitution {
      def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
        dest.append(m.group(1))
        dest.append(S_APOSTROPHY)
        dest.append(m.group(3))
      }
    })

  private def initReplacersD0Ds(): Unit = {
    val regex = Array("(^|\\p{Alnum})(\\.)(\\d)", "(\\d)(,|:|-|\\/)(\\d)", "(^)(\\')(\\d)", "(\\d)(\\')(s)")
    val size = regex.length
    R_D0D = new Array[Replacer](size)
    for (i <- 0 until size)
      R_D0D(i) = new jregex.Pattern(regex(i)).replacer(new SubstitutionD0D())
  }

  def initMapsD0D(): Unit = {
    M_D0D = new mutable.HashMap[String, Int]()
    val size = A_D0D.length
    for (i <- 0 until size)
      M_D0D.put(A_D0D(i), i)
  }

  def initPatterns(): Unit = {
    val size = A_D0D.length
    P_RECOVER_D0D = new Array[Pattern](size)

    for (i <- 0 until size)
      P_RECOVER_D0D(i) = Pattern.compile(S_D0D + i + "_")

    P_RECOVER_HYPHEN = Pattern.compile(S_HYPHEN)
    P_RECOVER_APOSTROPHY = Pattern.compile(S_APOSTROPHY)
    P_RECOVER_AMPERSAND = Pattern.compile(S_AMPERSAND)
  }

  def initDictionaries(cfg: EnglishTokenizerConfig): Unit = {
    T_EMOTICONS = cfg.T_EMOTICONS
    T_ABBREVIATIONS = cfg.T_ABBREVIATIONS
    P_HYPHEN_LIST = Pattern.compile(cfg.P_HYPHEN_LIST)
    M_COMPOUNDS = cfg.M_COMPOUNDS
    L_COMPOUNDS = cfg.L_COMPOUNDS
    R_UNIT = cfg.R_UNIT.map(new jregex.Pattern(_)).map(_.replacer(new SubstitutionTwo()))
  }

  protected def tokenizeWhiteSpaces(str: String): mutable.ArrayBuffer[ClearToken] = {
    val tokens = new mutable.ArrayBuffer[ClearToken]()
    for (text <- WHITE_SPAN.split(str))
      tokens += new ClearToken(text, false)
    tokens
  }

  protected def protectEmoticons(tokens: Seq[ClearToken]): Unit =
    for (token <- tokens)
      if (T_EMOTICONS.contains(token.text))
        token.protect = true

  protected def protectAbbreviations(tokens: Seq[ClearToken]): Unit = {
    var lower: String = null
    for (token <- tokens) {
      lower = token.text.toLowerCase
      if (T_ABBREVIATIONS.contains(lower) || P_ABBREVIATION.matcher(lower).find())
        token.protect = true
    }
  }

  protected def protectFilenames(tokens: Seq[ClearToken]): Unit = {
    var lower: String = null
    for (token <- tokens) {
      lower = token.text.toLowerCase
      if (FILE_EXTS.matcher(lower).find())
        token.protect = true
    }
  }

  // TODO need to make replacers change start/end indices?
  protected def replaceProtects(tokens: Seq[ClearToken], rep: Replacer): Unit = {
    for (token <- tokens)
      if (!token.protect)
        token.text = rep.replace(token.text)
  }

  protected def replaceHyphens(tokens: Seq[ClearToken]): Unit = {
    for (token <- tokens)
      if (!token.protect && P_HYPHEN_LIST.matcher(token.text.toLowerCase).find())
        token.text = P_HYPHEN.matcher(token.text).replaceAll(S_HYPHEN)
  }

  protected def recoverPatterns(tokens: Seq[ClearToken], p: Pattern, replacement: String): Unit = {
    var lastToken = null: ClearToken
    for (token <- tokens)
      token.text = p.matcher(token.text).replaceAll(replacement)
  }

  protected def tokenizeCompounds(oTokens: Seq[ClearToken]): mutable.ArrayBuffer[ClearToken] = {
    val nTokens = new mutable.ArrayBuffer[ClearToken]()
    var idx = 0
    for (oToken <- oTokens)
      if (oToken.protect || {idx = M_COMPOUNDS.getOrElse(oToken.text.toLowerCase, 0) - 1; idx < 0})
        nTokens += oToken
      else for (p <- L_COMPOUNDS(idx)) {
        nTokens += new ClearToken(oToken.text.substring(p.start, p.end), true)
      }
    nTokens
  }

  protected def tokenizePatterns(oTokens: mutable.ArrayBuffer[ClearToken], rep: Replacer): mutable.ArrayBuffer[ClearToken] = {
    val nTokens = new mutable.ArrayBuffer[ClearToken]()
    for (oToken <- oTokens)
      if (oToken.protect) nTokens += oToken
      else tokenizePatternsAux(nTokens, rep, oToken)
    nTokens
  }

  private def tokenizePatternsAux(tokens: mutable.ArrayBuffer[ClearToken], rep: Replacer, oldToken: ClearToken): Unit = {
    for (token <- P_DELIM.split(rep.replace(oldToken.text).trim()))
      if (token.startsWith(S_PROTECTED))
        tokens += new ClearToken(token.substring(N_PROTECTED), true)
      else if (!token.isEmpty)
        tokens += new ClearToken(token, false)
  }

  private class SubstitutionOne extends Substitution {
    def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
      dest.append(S_DELIM)
      dest.append(S_PROTECTED)
      dest.append(m.group(0))
      dest.append(S_DELIM)
    }
  }

  private class SubstitutionTwo extends Substitution {
    def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
      dest.append(m.group(1))
      dest.append(S_DELIM)
      dest.append(m.group(2))
    }
  }

  private class SubstitutionD0D extends Substitution {
    def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
      dest.append(m.group(1))
      dest.append(S_D0D + M_D0D(m.group(2)) + "_")
      dest.append(m.group(3))
    }
  }

  private class SubstitutionOnePlus extends Substitution {
    def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = {
      dest.append(S_DELIM)
      dest.append(S_PROTECTED)
      dest.append(m.group(1))
      dest.append(S_DELIM)
      dest.append(m.group(3))
    }
  }
}