package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{DocumentAnnotator, Token, Document}
import cc.factorie.app.strings.RegexSegmenter

/** Split a String into Tokens.  Aims to adhere to CoNLL 2003 tokenization rules.
    Punctuation that ends a sentence should be placed alone in its own Token, hence this segmentation implicitly defines sentence segmentation also.
    @author martin 
    */
class Tokenizer1(caseInsensitive:Boolean = true, skipSgml:Boolean = true) extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = null
  
  val patterns = new scala.collection.mutable.ArrayBuffer[String]
  
  val sgml = "</?[A-Za-z!][^>]*>"; patterns += sgml
  val htmlSymbol = "&(HT|TL|UR|LR|QC|QL|QR|amp|odq|cdq|lt|gt|#[0-9]+);"; patterns += htmlSymbol
  val htmlAccentedLetter = "&[aeiouAEIOU](acute|grave|uml);"; patterns += htmlAccentedLetter
  val url = "https?://[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-]"; patterns += url
  val url2 = "((www\\.([^ \t\n\f\r\"<>|.!?(){},]+\\.)+[a-zA-Z]{2,4})|(([^ \t\n\f\r\"`'<>|.!?(){},-_$]+\\.)+(com|org|net|edu|cc|info)))(/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-])?"; patterns += url2
  val url3 = "[A-Z]*[a-z0-9]+\\.(com|org|net|edu)"; patterns += url3
  val email = "[\\p{L}\\p{Nd}.]+@[\\p{L}\\{Nd}\\.]+\\.[a-z]{2,4}"; patterns += email
  val usphone = "(\\+?1)?.(\\([0-9]{3}\\)[ \u00A0]?|[0-9]{3}[- \u00A0\\.])[0-9]{3}[\\- \u00A0\\.][0-9]{4}"; patterns += usphone
  val date = "\\p{N}{1,2}[\\-/]\\p{N}{1,2}[\\-/]\\p{N}{2,4}"; patterns += date
  val currency = "[A-Z]*\\$|\\p{Sc}"; patterns += currency
  val hashtag = "#[A-Za-z][A-Za-z0-9]+"; patterns += hashtag
  val atuser  = "@[A-Za-z][A-Za-z0-9]+"; patterns += atuser

  val month = "Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec"
  val day = "Mon|Tue|Tues|Wed|Thu|Thurs|Fri" 
  val state = "Ala|Alab|Ariz|Ark|Calif|Colo|Conn|Del|Fla|Ill|Ind|Kans|Kan|Ken|Kent|Mass|Mich|Minn|Miss|Mont|Nebr|Neb|Nev|Dak|Okla|Oreg|Tenn|Tex|Virg|Wash|Wis|Wyo"
  val state2 = "AK|AS|AZ|AR|CA|CO|CT|DE|FM|FL|GA|GU|HI|ID|IL|IA|KS|KY|LA|MH|MD|MA|MI|MN|MS|MO|MT|NE|NV|MP|PW|PA|PR|TN|TX|UT|VT|VI|VA|WA|WI|WY"
  // Removed two-word state abbreviations and also ME, OK, OR, OH
  val honorific = "Adm|Attys?|Brig|Capts?|Cols?|Comms?|Co?mdrs?|Cpls?|Cpts?|Det|Drs?|Hon|Gens?|Govs?|Lieuts?|Lts?|Majs?|Miss|Messrs|Mr|Mrs|Ms|Pfc|Pres|Profs?|Pvts?|Reps?|Revs?|Sens?|Sgts?|Spc|Supts?"
  val suffix = "Bros?|Esq|Jr|Ph\\.?[Dd]|Sr"
  val place = "Ave|Blvd|Ste?|Str|Ln|Mt"
  val org = "Alt|Assns?|Bancorp|Bhd|Cos?|Comm|Comp|Corps?|Depts?|Elec|Inc|Inst|Intl|Lib|Ltd|M[ft]g|Mus|Natl|Plc|Pty|Sci|Ser|Sys|Univ"
  val abbrev = "etc|vol|rev|dea|est|gal"
  val abbrevs = Seq(month, day, state, state2, honorific, suffix, place, org, abbrev).flatMap(_.split('|').map(_.trim).filter(_.length > 0).map(_ + "\\.")).mkString("|")
  patterns += abbrevs
    
  val ap = "(?:['\u0092\u2019]|&apos;)" // apostrophe and open single quotes
  val ap2 = s"(?:${ap}|[\u0091\u2018\u201B])" // also includes backwards apostrophe and single quotes
  val contraction = s"(?:n${ap}t|${ap}(d|D|s|S|m|M|re|RE|ve|VE|ll|LL))"; patterns += contraction
  val apword = s"${ap}n(${ap})?|${ap2}em|[Oo]${ap2}clock|[Cc]${ap2}mon|${ap2}cause|${ap}till?|ol${ap}|somethin${ap}|Dunkin${ap}|${ap}[1-9]0s|N${ap}|\\p{L}\\p{Ll}*[aeiou]${ap}[aeiou]\\p{Ll}*"; patterns += apword // words that include an apostrophe, like C'mon, 'n', Shi'ite, 20's, N'goma
  val initials = "[\\p{L}]\\.[\\p{L}\\.]*"; patterns += initials // A.  A.A.A.I.  etc.
  //val briefAbbrevs = "[A-Z][a-z]?\\."; patterns += briefAbbrevs // and initials; so includes A. and Mr. but not Mrs. Calif. or Institute.  Removed because otherwise we get "me." and "it."
  val ordinals = "[0-9]{1,2}(st|nd|rd|th)"; patterns += ordinals // like 1st and 22nd
  //val numerics = "[\\p{N}\\-.\\:/,\\+\\=%]+[\\p{N}\\-:/\\+\\=%]"; patterns += numerics // is there a better way to say, "doesn't end in '.'"?
  val quote = "``|''|[\u2018\u2019\u201A\u201B\u201C\u201D\u0091\u0092\u0093\u0094\u201A\u201E\u201F\u2039\u203A\u00AB\u00BB]{1,2}|[`\"\u201C\u201D\\p{Pf}]|$quot;|" + ap + "{1,2}"; patterns += quote
  // List of prefixes taken from http://en.wikipedia.org/wiki/English_prefixes with the addition of "e", "uh" and "x" from Ontonotes examples
  val prefixes = "(?i:a|anti|arch|be|co|counter|de|dis|en|em|ex|fore|hind|mal|mid|midi|mini|mis|out|over|post|pre|pro|re|self|step|trans|twi|un|under|up|with|Afro|ambi|amphi|an|ana|Anglo|ante|apo|astro|auto|bi|bio|circum|cis|con|com|col|cor|contra|cryo|crypto|de|demi|demo|deutero|deuter|di|dia|dis|dif|du|duo|eco|electro|e|en|epi|Euro|ex|extra|fin|Franco|geo|gyro|hetero|hemi|homo|hydro|hyper|hypo|ideo|idio|in|Indo|in|infra|inter|intra|iso|macro|maxi|mega|meta|micro|mono|multi|neo|non|omni|ortho|paleo|pan|para|ped|per|peri|photo|pod|poly|post|pre|preter|pro|pros|proto|pseudo|pyro|quasi|retro|semi|socio|sub|sup|super|supra|sur|syn|tele|trans|tri|uh|ultra|uni|vice|x)"
  val dashedWord = prefixes+"(-[\\p{L}\\p{N}]+)+(?![\\p{L}\\p{N}])"; patterns += dashedWord // Dashed words are tokenized as one word, like "co-ed" as long as the first component is 6 characters or less (but this misses "counter" and "eastern..."), but longer ones are split
  //val dashedWord = "[\\p{L}\\p{N}]{1,6}(-[\\p{L}\\p{N}]+)+(?![\\p{L}\\p{N}])"; patterns += dashedWord // Dashed words are tokenized as one word, like "co-ed" as long as the first component is 6 characters or less (but this misses "counter" and "eastern..."), but longer ones are split
  // common dashed words in Ontonotes include counter-, ultra-, eastern-, quasi-, trans-,  
  val fraction = "[\u00BC\u00BD\u00BE\u2153\u2154]|(\\p{N}{1,4}[- \u00A0])?\\p{N}{1,4}(\\\\?/|\u2044)\\p{N}{1,4}"; patterns += fraction
  val letter = "&[aeiouAEIOU](acute|grave|uml);"
  val contractedWord = s"[\\p{L}\\p{M}]+(?=(${contraction}))"; patterns += contractedWord // Includes any combination of letters and accent characters before a contraction
  val word = s"\\p{L}[\\p{L}\\p{M}\\p{Nd}]*"; patterns += word // Includes any combination of letters, accent characters and numbers
  val number = "[-\\+\\.,]?\\p{Nd}+([\\.:,]\\p{Nd}+)*"; patterns += number
  val repeatedPunc = "[-\\*=\\+\\.\\?!#]+"; patterns += repeatedPunc
  val dash = "&(mdash|ndash|MD);|[\u0096\u0097\\p{Pd}]+"; patterns += dash // I think \p{Pd} should include \u2013\u2014\u2015
  val punc = "\\p{P}"; patterns += punc // This matches any kind of punctuation as a single character, so any special handling of multiple punc together must be above, e.g. ``, ---
  val symbol = "\\p{S}"; patterns += symbol
  //val newline = "\n"; patterns += newline
  //val space = "\\p{Z}+"

  val tokenRegexString = patterns.filter(_.length != 0).mkString("|")
  val tokenRegex = if (caseInsensitive) ("(?i)"+tokenRegexString).r else tokenRegexString.r

  def process(document: Document): Document = {
    for (section <- document.sections) {
      var prevTokenPeriod = false
      val tokenIterator = tokenRegex.findAllIn(section.string)
      while (tokenIterator.hasNext) {
        tokenIterator.next()
        val string = document.string.substring(section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        if (prevTokenPeriod && java.lang.Character.isLowerCase(string(0)) && section.tokens(section.length-2).stringEnd == section.tokens(section.length-1).stringStart) {
          // If we have a pattern like "Abbrev. is" with token strings "Abbrev", ".", "is" (currently looking at "is")
          // then assume that the previous-previous word is actually an abbreviation; patch it up to become "Abbrev.", "is"
          val lastTwoTokens = section.takeRight(2).toIndexedSeq
          section.remove(section.length-1); section.remove(section.length-1)
          new Token(section, lastTwoTokens(0).stringStart, lastTwoTokens(1).stringEnd)
          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        } else if (!skipSgml || string(0) != '<' || string(string.length-1) != '>') {
          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        }
        if (string == ".") prevTokenPeriod = true else prevTokenPeriod = false
      }
    }
    document
  }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = List(classOf[Token])
}

object Tokenizer1 extends Tokenizer1(true, true) {
  def main(args: Array[String]): Unit = {
    val string = io.Source.fromInputStream(System.in).mkString
    val doc = new Document(string)
    RegexTokenizer.process(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}
