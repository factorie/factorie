package cc.factorie.app.nlp.segment

import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.{DocumentAnnotator, Sentence, Document, Token}
import cc.factorie.app.strings.StringSegmentIterator

object DefaultRules {
  val contractionsAndPossessives = """((?i)'(s|d|m|l+|ve|re)\b)|((?i)n't\b)"""
  val singleLetterAcronyms = """[\p{L}]\.[\p{L}\.]*"""
  val allAbbrevs = """([\p{L}]+\.)"""
  val ordinals = "[0-9]{1,2}[sthnrd]+[\\-\\p{L}]+"
  val notEndingInDot = "[0-9\\-.\\:/,\\+\\=%><]+[0-9\\-:/,\\+\\=%><]"
  val possiblyEndingInDot = "[0-9\\-.\\:/,\\+\\=%]+"
  val email = """(?i)\b[\p{L}\p{Nd}._%+-]+@[\p{L}\p{Nd}.-]+\.[A-Z]{2,4}\b"""
  val url1 = """\b(https?|ftp|file)://[-\p{L}\p{Nd}+&@#/%?=~_|!:,.;]*[-\p{L}\p{Nd}+&@#/%=~_|]"""
  val url2 = """\b[wW]{3}.(([-\p{L}\p{Nd}+&@#/%?=~_|!:,;]+(?=\.))\.)+[A-Za-z]{2,4}(/[-\p{L}\p{Nd}+&@#/%?=~_|!:,;]*)?"""
  val finalPunctuation1 = """[.?!]["')}\]]?"""
  // why does this have square and curly brackets in it??
  val finalPunctuation2 = """["')}\]]?[.?!]"""
  val midSentenceQuotes = "[`'\"]+"
  val otherSymbols = """[,\-:;$?&@\(\)]+"""
  val alphanumericsAndHyphensPrecedingContractionsOrPossessives = """[\p{L}\p{N}\-]+(?=(?i)('(s|d|m|l+|ve|re))|(n't))"""
  val wordsWithSequencesOfSingleDashesInside = "[\\w]+(-[\\w]+)*"
  val wordWithNumberAndApostrophe = "[\\w']+"

  val commonAbbreviations = Set(
   "inc", "corp", "dec", "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "oct", "nov", "ala",
   "ariz", "ark", "colo", "conn", "del", "fla", "ill", "ind", "kans", "kan", "ken", "kent", "mass", "mich",
   "minn", "miss", "mont", "nebr", "neb", "nev", "dak", "okla", "oreg", "tenn", "tex", "virg", "wash", "wis",
   "wyo", "mr", "ms", "mrs", "calif", "oct", "vol", "rev", "ltd", "dea", "est", "capt", "hev", "gen", "ltd", "etc", "sci",
   "comput", "univ", "ave", "cent", "col", "comdr", "cpl", "dept", "dust,", "div", "est", "gal", "gov", "hon",
   "grad", "inst", "lib", "mus", "pseud", "ser", "alt", "Inc", "Corp", "Dec", "Jan", "Feb", "Mar", "Apr",
   "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Ala", "Ariz", "Ark", "Colo", "Conn", "Del", "Fla", "Ill",
   "Ind", "Kans", "Kan", "Ken", "Kent", "Mass", "Mich", "Minn", "Miss", "Mont", "Nebr", "Neb", "Nev", "Dak",
   "Okla", "Oreg", "Tenn", "Tex", "Virg", "Wash", "Wis", "Wyo", "Mrs", "Calif", "Oct", "Vol", "Rev", "Ltd",
   "Dea", "Est", "Capt", "Hev", "Gen", "Ltd", "Etc", "Sci", "Comput", "Univ", "Ave", "Cent", "Col", "Comdr",
   "Cpl", "Dept", "Dust,", "Div", "Est", "Gal", "Gov", "Hon", "Grad", "Inst", "Lib", "Mus", "Pseud", "Ser", "Alt",
   "Mr", "Ms")

  val commonSentenceStarters = Set("The")

  val defaultRuleset = Seq(
    contractionsAndPossessives
    , singleLetterAcronyms
    , allAbbrevs
    , ordinals
    , possiblyEndingInDot
    , email
    , url1
    , url2
    , finalPunctuation1
    , finalPunctuation2
    , midSentenceQuotes
    , otherSymbols
    , alphanumericsAndHyphensPrecedingContractionsOrPossessives
    , wordsWithSequencesOfSingleDashesInside
    , wordWithNumberAndApostrophe)

  val defaultRulesetNoSentenceBoundaries = Seq(
    contractionsAndPossessives
    , singleLetterAcronyms
    , ordinals
    , notEndingInDot
    , email
    , url1
    , url2
    , commonAbbreviations.mkString("|")
    , finalPunctuation1
    , finalPunctuation2
    , midSentenceQuotes
    , otherSymbols
    , alphanumericsAndHyphensPrecedingContractionsOrPossessives
    , wordsWithSequencesOfSingleDashesInside
    , wordWithNumberAndApostrophe)
}

sealed trait SentenceBoundaryInference
case object PerDocument extends SentenceBoundaryInference
case object JointlyAcrossDocuments extends SentenceBoundaryInference
case object Non extends SentenceBoundaryInference

object PunktTokenizer extends PunktTokenizer

class PunktTokenizer extends DocumentAnnotator {

  def tokenAnnotationString(token: Token) = token.string + "\t"

  def commonAbbreviations: Set[String] = DefaultRules.commonAbbreviations
  def commonSentenceStarters: Set[String] = DefaultRules.commonSentenceStarters
  def sentenceBoundaryInference: SentenceBoundaryInference = JointlyAcrossDocuments

  def ruleset: Seq[String] =
    if (sentenceBoundaryInference == Non) DefaultRules.defaultRulesetNoSentenceBoundaries
    else DefaultRules.defaultRuleset

  private[this] val regex = ruleset.mkString("|").r

//  def apply(s: String): StringSegmentIterator = new StringSegmentIterator {
//    val doc = new Document(s)
//    process(doc)
//    var i = 0
//    val len = doc.tokens.length
//    def hasNext = i < len - 1
//    def next: String = { val result = doc.tokens(i).string; i += 1; result }
//    def start = doc.tokens(i).stringStart
//    def end = doc.tokens(i).stringEnd
//    //doc.tokens.map(_.string).iterator
//  }

  def apply(s: String): StringSegmentIterator = new StringSegmentIterator {
    val tokenIterator = for (section <- process(new Document(s)).sections.iterator; token <- section.tokens.iterator) yield token
    var token: Token = null
    def hasNext = tokenIterator.hasNext
    def next(): String = { token = tokenIterator.next(); token.string }
    def start = token.stringStart
    def end = token.stringEnd
  }


  // TODO Fix this to fit better into the DocumentProcessor framework, e.g. setting postAttrs
  def process(documents: Seq[Document]): Unit = processLogic(documents, sentenceBoundaryInference)

  def process(document: Document): Document = { processLogic(Seq(document), sentenceBoundaryInference); document }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Token], classOf[Sentence])

  // TODO Fix to obey document.sections! -akm
  private[this] def processLogic(documents: Seq[Document], inference: SentenceBoundaryInference): Unit = inference match {
    case PerDocument => documents.foreach(d => processLogic(Seq(d), JointlyAcrossDocuments))
    case Non =>
      for (d <- documents; section <- d.sections) {
        val tokenIterator = regex.findAllIn(section.string)
        while (tokenIterator.hasNext) {
          tokenIterator.next()
          new Token(d, tokenIterator.start, tokenIterator.end)
        }
        new Sentence(section, 0, d.tokenCount)
      }
    case JointlyAcrossDocuments =>
      val docString = documents.map(_.string).mkString(" ")
      val sentenceSegmented = PunktSentenceSegmenter.findSentenceBoundaries(docString, abvSet = commonAbbreviations).toArray
      var tokensSoFar = 0
      var d = 0
      var currentDocument = documents(d)
      var docOffset = 0
      val segmentsIterator = sentenceSegmented.sliding(2)
      while (segmentsIterator.hasNext) {
        var Array((start, _), (end, endTy)) = segmentsIterator.next()
        val endIsAbbrev = endTy == AS || endTy == A
        /* end isn't an abbrev, so remove the period by making end -= 1 and then in the regex you can have all things containing '.' be abbrevs */
        if (!endIsAbbrev) {end -= 1}
        if (end > docOffset + currentDocument.string.length + 1) {
          d += 1
          docOffset += currentDocument.string.length + 1
          currentDocument = documents(d)
          tokensSoFar = 0
        }
        val currentDocumentOffset = start - docOffset
        val tokenIterator = regex.findAllIn(docString.substring(start, end))
        var numTokens = 0
        while (tokenIterator.hasNext) {
          tokenIterator.next()
          new Token(currentDocument, math.max(0, currentDocumentOffset + tokenIterator.start), currentDocumentOffset + tokenIterator.end) // really?
          numTokens += 1
        }
        if (!endIsAbbrev) {
          new Token(currentDocument.asSection, end - docOffset, end + 1 - docOffset)
          numTokens += 1
        }
        new Sentence(currentDocument.asSection, tokensSoFar, numTokens) // really?
        tokensSoFar += numTokens
      }
  }
}

