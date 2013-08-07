package cc.factorie.app.nlp.segment

import scala.collection.mutable
import java.util.regex.Pattern
import jregex.{Replacer, TextBuffer, MatchResult, Substitution}
import cc.factorie.app.nlp.{DocumentAnnotator, Document, Token, Sentence, Section}

object ClearTokenizer extends EnglishTokenizer(EnglishTokenizerConfig.default)

object ClearSegmenter extends EnglishSegmenter(ClearTokenizer)

trait AbstractSegmenter extends DocumentAnnotator {
  def tokenizer: AbstractTokenizer
  def getSentences(fin: String): mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]]
  def process1(d: Document): Document = {
    for (section <- d.sections) {
      val sentences = getSentences(section.string)
      TokenizerHelper.addTokensToDoc(sentences.flatten, section)
      var start = 0
      for (cs <- sentences; if cs.length > 0) {
        new Sentence(section, start, cs.length)(null)
        start = start + cs.length
      }
    }
    d
  }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Token], classOf[Sentence])
}

trait AbstractTokenizer extends DocumentAnnotator {
  def getTokenList(str: String): mutable.ArrayBuffer[ClearToken]
  def process1(d: Document): Document = {
    for (section <- d.sections) TokenizerHelper.addTokensToDoc(getTokenList(section.string), section)
    d
  }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Token])
}

class EnglishSegmenter(val tokenizer: AbstractTokenizer) extends AbstractSegmenter {
  /** Patterns of terminal punctuation. */
  private val P_TERMINAL_PUNCTUATION = Pattern.compile("^(\\.|\\?|!)+$")
  private val L_BRACKETS = Array("\"", "(", "{", "[")
  private val R_BRACKETS = Array("\"", ")", "}", "]")

  def tokenAnnotationString(token: Token) = null
  def getSentences(fin: String): mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]] = {
    val sentences = new mutable.ArrayBuffer[mutable.ArrayBuffer[ClearToken]]()
    val tokens = tokenizer.getTokenList(fin)
    val brackets = new Array[Int](R_BRACKETS.length)
    val size = tokens.size
    var isTerminal = false
    var bIdx = 0

    for (i <- 0 until size) {
      val curr = tokens(i)
      countBrackets(curr.text, brackets)
      if (isTerminal || P_TERMINAL_PUNCTUATION.matcher(curr.text).find())
        if (i + 1 < size && isFollowedByBracket(tokens(i + 1).text, brackets))
          isTerminal = true
        else {
          sentences += tokens.slice(bIdx, {bIdx = i + 1; bIdx})
          isTerminal = false
        }
    }
    if (bIdx != size) {
      sentences += tokens.slice(bIdx, size)
    }
    sentences
  }

  private def countBrackets(str: String, brackets: Array[Int]): Unit =
    if (str == "\"")
      brackets(0) += (if (brackets(0) == 0) 1 else -1)
    else
      for (i <- 1 until brackets.length)
        if (str == L_BRACKETS(i))
          brackets(i) += 1
        else if (str == R_BRACKETS(i))
          brackets(i) -= 1

  private def isFollowedByBracket(str: String, brackets: Array[Int]): Boolean =
    (0 until R_BRACKETS.length).exists(i => brackets(i) > 0 && str == R_BRACKETS(i))
}

object TokenizerHelper {
  def addTokensToDoc(tokens: Seq[ClearToken], section: Section): Unit = {
    var offset = 0
    for (ct <- tokens) {
      val substrStart = section.string.indexOf(ct.text, offset)
      val substrEnd = substrStart + ct.text.length
      new Token(section, substrStart+section.stringStart, substrEnd+section.stringStart)
      offset = substrEnd
    }
  }
}

class ClearToken(var text: String, var protect: Boolean) {}

class CompoundSpan(val start: Int, val end: Int) {}

class EnglishTokenizer(cfg: EnglishTokenizerConfig) extends AbstractTokenizer {
  private val WHITE_SPAN = Pattern.compile("\\s+")
  private val URL_SPAN = new jregex.Pattern(
    "((([A-Za-z]{3,9}:(?:\\/\\/)?)(?:[-;:&=\\+\\$,\\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\\+\\$,\\w]+@)[A-Za-z0-9.-]+)" +
    "((?:\\/[\\+~%\\/.\\w-_]*)?\\??(?:[-\\+=&;%@.\\w_]*)#?(?:[.\\!\\/\\\\w]*))?|(\\w+\\.)+(com|edu|gov|int|mil|net|org|biz)$)")
  private val FILE_EXTS = Pattern.compile(
    "\\S+\\.(3gp|7z|ace|ai(f){0,2}|amr|asf|asp(x)?|asx|avi|bat|bin|bmp|bup|cab|cbr|cd(a|l|r)|chm|dat|divx|dll|dmg|doc|dss" +
    "|dvf|dwg|eml|eps|exe|fl(a|v)|gif|gz|hqx|(s)?htm(l)?|ifo|indd|iso|jar|jsp|jp(e)?g|lnk|log|m4(a|b|p|v)|mcd|mdb|mid|mov|" +
    "mp(2|3|4)|mp(e)?g|ms(i|wmm)|ogg|pdf|php|png|pps|ppt|ps(d|t)?|ptb|pub|qb(b|w)|qxd|ra(m|r)|rm(vb)?|rtf|se(a|s)|sit(x)?|sql|" +
    "ss|swf|tgz|tif|torrent|ttf|txt|vcd|vob|wav|wm(a|v)|wp(d|s)|xls|xml|xtm|zip)$")

  private val S_DELIM = " "
  private val S_PROTECTED = "PR0T_"
  private val S_D0D = "_DPPD_"
  private val S_HYPHEN = "_HYYN_"
  private val S_AMPERSAND = "_APSD_"
  private val S_APOSTROPHY = "_AOOR_"
  private val N_PROTECTED = S_PROTECTED.length()
  private val ALT_APOSTROPHY = "\u2019" // TODO incorporate this, and other alternative quote and dash characters. -akm

  private val P_DELIM = Pattern.compile(S_DELIM)
  private val P_HYPHEN = Pattern.compile("-")
  private val P_ABBREVIATION = Pattern.compile("^(\\p{Alpha}\\.)+\\p{Alpha}?$")
  private val A_D0D = Array(".", ",", ":", "-", "/", "'")

  private val R_URL: Replacer = URL_SPAN.replacer(SubstitutionOne)
  private val R_ABBREVIATION: Replacer = newSubOnePlus("(^(\\p{Alpha}\\.)+)(\\p{Punct}*$)")
  private val R_PERIOD_LIKE: Replacer = newSubOne("(\\.|\\?|\\!){2,}")
  private val R_MARKER: Replacer = newSubOne("\\-{2,}|\\*{2,}|\\={2,}|\\~{2,}|\\,{2,}|\\`{2,}|\\'{2,}")
  private val R_APOSTROPHY: Replacer = newSubOne("(?i)((\\')(s|d|m|ll|re|ve|nt)|n(\\')t)$")
  private val R_USDOLLAR: Replacer = newSubOne("^US\\$")
  private val R_AMPERSAND: Replacer = newReplacer("(\\p{Upper})(\\&)(\\p{Upper})", m => m.group(1) + S_AMPERSAND + m.group(3))
  private val R_WAW: Replacer = newReplacer("(\\w)(\\')(\\w)", m => m.group(1) + S_APOSTROPHY + m.group(3))

  private val R_PUNCTUATION_PRE: Replacer = newSubOne("\\(|\\)|\\[|\\]|\\{|\\}|<|>|\\,|\\:|\\;|\\\"")
  private val R_PUNCTUATION_POST: Replacer = newSubOne("\\.|\\?|\\!|\\`|\\'|\\-|\\/|\\@|\\#|\\$|\\%|\\&|\\|")
  private val R_D0D: Array[Replacer] = Array("(^|\\p{Alnum})(\\.)(\\d)", "(\\d)(,|:|-|\\/)(\\d)", "(^)(\\')(\\d)", "(\\d)(\\')(s)").map(newSubD0D)
  private val R_UNIT: Array[Replacer] = cfg.R_UNIT.map(new jregex.Pattern(_)).map(_.replacer(SubstitutionTwo))

  private val T_EMOTICONS: mutable.Set[String] = cfg.T_EMOTICONS
  private val T_ABBREVIATIONS: mutable.Set[String] = cfg.T_ABBREVIATIONS
  private val P_HYPHEN_LIST: Pattern = Pattern.compile(cfg.P_HYPHEN_LIST)
  private val M_D0D: mutable.HashMap[String, Int] = {
    val map = new mutable.HashMap[String, Int]()
    for (i <- 0 until A_D0D.length) map(A_D0D(i)) = i
    map
  }
  private val M_COMPOUNDS: mutable.HashMap[String, Int] = cfg.M_COMPOUNDS
  private val L_COMPOUNDS: mutable.ArrayBuffer[Array[CompoundSpan]] = cfg.L_COMPOUNDS
  private val P_RECOVER_D0D: Array[Pattern] = (0 until A_D0D.length).map(i => Pattern.compile(S_D0D + i + "_")).toArray
  private val P_RECOVER_HYPHEN: Pattern = Pattern.compile(S_HYPHEN)
  private val P_RECOVER_APOSTROPHY: Pattern = Pattern.compile(S_APOSTROPHY)
  private val P_RECOVER_AMPERSAND: Pattern = Pattern.compile(S_AMPERSAND)


  def tokenAnnotationString(token: Token) = token.string + "\t"

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

  private def tokenizeWhiteSpaces(str: String): mutable.ArrayBuffer[ClearToken] =
    new mutable.ArrayBuffer[ClearToken] ++= WHITE_SPAN.split(str).map(new ClearToken(_, false))

  private def protectEmoticons(tokens: Seq[ClearToken]): Unit =
    for (token <- tokens; if T_EMOTICONS.contains(token.text)) token.protect = true

  private def protectAbbreviations(tokens: Seq[ClearToken]): Unit =
    for (token <- tokens; lower = token.text.toLowerCase)
      if (T_ABBREVIATIONS.contains(lower) || P_ABBREVIATION.matcher(lower).find())
        token.protect = true

  private def protectFilenames(tokens: Seq[ClearToken]): Unit =
    for (token <- tokens; if FILE_EXTS.matcher(token.text.toLowerCase).find())
      token.protect = true

  private def replaceProtects(tokens: Seq[ClearToken], rep: Replacer): Unit =
    for (token <- tokens; if !token.protect) token.text = rep.replace(token.text)

  private def replaceHyphens(tokens: Seq[ClearToken]): Unit =
    for (token <- tokens)
      if (!token.protect && P_HYPHEN_LIST.matcher(token.text.toLowerCase).find())
        token.text = P_HYPHEN.matcher(token.text).replaceAll(S_HYPHEN)

  private def recoverPatterns(tokens: Seq[ClearToken], p: Pattern, replacement: String): Unit =
    for (token <- tokens) token.text = p.matcher(token.text).replaceAll(replacement)

  private def tokenizeCompounds(oTokens: Seq[ClearToken]): mutable.ArrayBuffer[ClearToken] = {
    val nTokens = new mutable.ArrayBuffer[ClearToken]()
    var idx = 0
    for (oToken <- oTokens)
      if (oToken.protect || {idx = M_COMPOUNDS.getOrElse(oToken.text.toLowerCase, 0) - 1; idx < 0})
        nTokens += oToken
      else for (p <- L_COMPOUNDS(idx))
        nTokens += new ClearToken(oToken.text.substring(p.start, p.end), true)
    nTokens
  }

  private def tokenizePatterns(oTokens: mutable.ArrayBuffer[ClearToken], rep: Replacer): mutable.ArrayBuffer[ClearToken] = {
    val nTokens = new mutable.ArrayBuffer[ClearToken]()
    for (oToken <- oTokens)
      if (oToken.protect) nTokens += oToken
      else tokenizePatternsAux(nTokens, rep, oToken)
    nTokens
  }

  private def tokenizePatternsAux(tokens: mutable.ArrayBuffer[ClearToken], rep: Replacer, oldToken: ClearToken): Unit =
    for (token <- P_DELIM.split(rep.replace(oldToken.text).trim()))
      if (token.startsWith(S_PROTECTED))
        tokens += new ClearToken(token.substring(N_PROTECTED), true)
      else if (!token.isEmpty)
        tokens += new ClearToken(token, false)

  private class Sub(getResults: MatchResult => String) extends Substitution {
    def appendSubstitution(m: MatchResult, dest: TextBuffer): Unit = dest.append(getResults(m))
  }

  private object SubstitutionOne extends Sub(m => S_DELIM + S_PROTECTED + m.group(0) + S_DELIM)
  private object SubstitutionTwo extends Sub(m => m.group(1) + S_DELIM + m.group(2))
  private object SubstitutionD0D extends Sub(m => m.group(1) + S_D0D + M_D0D(m.group(2)) + "_" + m.group(3))
  private object SubstitutionOnePlus extends Sub(m => S_DELIM + S_PROTECTED + m.group(1) + S_DELIM + m.group(3))

  private def newSubOne(regex: String) = new jregex.Pattern(regex).replacer(SubstitutionOne)
  private def newSubOnePlus(regex: String) = new jregex.Pattern(regex).replacer(SubstitutionOnePlus)
  private def newSubD0D(regex: String) = new jregex.Pattern(regex).replacer(SubstitutionD0D)
  private def newReplacer(regex: String, getResults: MatchResult => String) = new jregex.Pattern(regex).replacer(new Sub(getResults))
}