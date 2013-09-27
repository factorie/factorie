package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{DocumentAnnotator, Token, Document}
import cc.factorie.app.strings.RegexSegmenter

/** Split a String into Tokens.  Aims to adhere to CoNLL 2003 tokenization rules.
    Punctuation that ends a sentence should be placed alone in its own Token, hence this segmentation implicitly defines sentence segmentation also.
    @author martin 
    */
class Tokenizer1(caseSensitive:Boolean = false, tokenizeSgml:Boolean = false, tokenizeNewline:Boolean = false, tokenizeConnl03:Boolean = false) extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString
  
  val patterns = new scala.collection.mutable.ArrayBuffer[String]
  
  val html = "(<script[^>]*>([^\u0000](?!</script>))*[^\u0000]?</script>)|(<style[^>]*>([^\u0000](?!</style>))*[^\u0000]?</style>)"; if (!tokenizeSgml) patterns += html // The [^\u0000] ensures we match newline also
  val htmlComment = "(<|&lt;)!--([^\u0000](?!-->))*[^\u0000]?--(>|&gt;)"; patterns += htmlComment
  val sgml2 = "<%([^\u0000](?!%>))*[^\u0000]?%>"; patterns += sgml2 // Some HTML contains "<% blah %>" tags.
  val sgml = "</?[A-Za-z!]([^>]|%>)*(?<!%)>"; patterns += sgml // Closing with "%>" doesn't count
  val htmlSymbol = "&(HT|TL|UR|LR|QC|QL|QR|amp|copy|reg|trade|odq|nbsp|cdq|lt|gt|#[0-9A-Za-z]+);"; patterns += htmlSymbol // TODO Make this list complete
  val url = "https?://[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-]"; patterns += url
  val url2 = "((www\\.([^ \t\n\f\r\"<>|.!?(){},]+\\.)+[a-zA-Z]{2,4})|(([^ \t\n\f\r\"`'<>|.!?(){},-_$]+\\.)+(com|org|net|edu|gov|cc|info|uk|de|fr|ca)))(/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-])?"; patterns += url2
  val url3 = "[A-Z]*[a-z0-9]+\\.(com|org|net|edu|gov|co\\.uk|ac\\.uk|de|fr|ca)"; patterns += url3
  val email = "[\\p{L}\\p{Nd}.]+@[\\p{L}\\{Nd}\\.]+\\.[a-z]{2,4}"; patterns += email
  val usphone = "(\\+?1[-\\. \u00A0]?)?(\\([0-9]{3}\\)[ \u00A0]?|[0-9]{3}[- \u00A0\\.])[0-9]{3}[\\- \u00A0\\.][0-9]{4}"; patterns += usphone
  val date = "((((19|20)?[0-9]{2}[\\-/][0-3]?[0-9][\\-/][0-3]?[0-9])|([0-3]?[0-9][\\-/][0-3]?[0-9][\\-/](19|20)?[0-9]{2}))(?![0-9]))"; patterns += date // e.g. 3/4/1992 or 2012-04-05, but don't match just the first 8 chars of 12-25-1112
  val currency = "[A-Z]*\\$|&(euro|cent|pound);|\\p{Sc}|(USD|EUR|JPY|GBP|CHF|CAD|KPW|RMB|CNY|AD|GMT)(?![A-Z])"; patterns += currency
  val hashtag = "#[A-Za-z][A-Za-z0-9]+"; patterns += hashtag
  val atuser  = "@[A-Za-z][A-Za-z0-9]+"; patterns += atuser
  val emoticon = "[#<%\\*]?[:;!#\\$%@=\\|][-\\+\\*=o^<]?[\\(\\)oODPQX\\*3{}\\[\\]][#><\\)\\(]?|'\\.'"; patterns += emoticon
  val filename = "\\S+\\.(3gp|7z|ace|ai(f){0,2}|amr|asf|asp(x)?|asx|avi|bat|bin|bmp|bup|cab|cbr|cd(a|l|r)|chm|dat|divx|dll|dmg|doc|dss|dvf|dwg|eml|eps|exe|fl(a|v)|gif|gz|hqx|(s)?htm(l)?|ifo|indd|iso|jar|jsp|jp(e)?g|key|lnk|log|m4(a|b|p|v)|mcd|mdb|mid|mov|mp(2|3|4)|mp(e)?g|ms(i|wmm)|numbers|ogg|pages|pdf|php|png|pps|ppt|ps(d|t)?|Penn|pub|qb(b|w)|qxd|ra(m|r)|rm(vb)?|rtf|se(a|s)|sit(x)?|sql|ss|swf|tgz|tif|torrent|ttf|txt|vcd|vob|wav|wm(a|v)|wp(d|s)|xls|xml|xtm|zip)"; patterns += filename

  // Abbreviation handling
  val consonantNonAbbrevs = "(Ng|cwm|nth)(?=\\.)"; patterns += consonantNonAbbrevs // the "val abbrev" below matches all sequences of consonants followed by a period; these are exceptions to that rule
  val month = "Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec" // Note that "May" is not included because it is not an abbreviation
  val day = "Mon|Tue|Tues|Wed|Thu|Thurs|Fri" 
  val state = "Ala|Alab|Ariz|Ark|Calif|Colo|Conn|Del|Fla|Ill|Ind|Kans|Kan|Ken|Kent|Mass|Mich|Minn|Miss|Mont|Nebr|Neb|Nev|Dak|Okla|Oreg|Tenn|Tex|Virg|Wash|Wis|Wyo"
  val state2 = "Ak|As|Az|Ar|Ca|Co|Ct|De|Fm|Fl|Ga|Gu|Hi|Id|Il|Ia|Ks|Ky|La|Mh|Md|Ma|Mi|Mn|Ms|Mo|Mt|Ne|Nv|Mp|Pw|Pa|Pr|Tn|Tx|Ut|Vt|Vi|Va|Wa|Wi|Wy"
  // Removed two-word state abbreviations and also ME, OK, OR, OH
  val honorific = "Adm|Attys?|Brig|Capts?|Cols?|Comms?|Co?mdrs?|Cpls?|Cpts?|Det|Drs?|Hon|Gens?|Govs?|Lieuts?|Lts?|Majs?|Miss|Messrs|Mr|Mrs|Ms|Pfc|Pres|Profs?|Pvts?|Reps?|Revs?|Sens?|Sgts?|Spc|Supts?"
  val suffix = "Bros?|Esq|Jr|Ph\\.?[Dd]|Sr"
  val place = "Aly|Anx|Ave?|Avn|Blvd|Boul|Byp|Cir|Cor|Rd|Squ|Sta|Ste?|Str|Ln|Mt"
  val units = "in|fur|mi|lea|drc|oz|qtr|cwt|am|pm" // not covered by "sequence of consonants" in abbrev below; note the abbrev pattern below insists on initial capital letter
  val org = "Alt|Assns?|Bancorp|Bhd|Cos?|Comm|Comp|Corps?|Depts?|Elec|Inc|Inst|Intl|Lib|Ltd|M[ft]g|Mus|Natl|Plc|Pty|Sci|Ser|Sys|Univ"
  val abbrev = "etc|vol|rev|dea|div|ests?|exp|exts?|gal|[BCDFGHJKLMNPQRSTVWX][bcdfghjklmnpqrstvwx]+"
  val abbrevs = Seq(month, day, state, state2, honorific, suffix, place, units, org, abbrev).flatMap(_.split('|').map(_.trim).filter(_.length > 0).map(_ + "\\.")).mkString("|")
  patterns += abbrevs
  val noAbbrev = "[Nn]o\\.(?=\\p{Z}*\\p{Nd})"; patterns += noAbbrev // Capture "No." when followed by a number (as in "No. 5", where it is an abbreviation of "number").  // TODO Consider a token normalization to "number"? -akm
    
  //val htmlLetter = "(?:&[aeiounyAEIOUNY](acute|grave|uml|circ|ring|tilde);)"
  val htmlAccentedLetter = "(?:&[aeiouyntlAEIOUYNTL](acute|grave|uml|circ|orn|tilde|ring);)"; patterns += htmlAccentedLetter // TODO Make this list complete; see http://www.starr.net/is/type/htmlcodes.html
  val letter = s"([\\p{L}\\p{M}]|$htmlAccentedLetter)"
  val ap = "(?:['\u0092\u2019]|&(apos|rsquo|#00?39|#00?92|#2019);)" // apostrophe and open single quotes
  val ap2 = s"(?:$ap|&lsquo;|[`\u0091\u2018\u201B])" // also includes backwards apostrophe and single quotes, but not double quotes
  val contraction = s"(?:n${ap}t|(?<=\\p{L})$ap(d|D|s|S|m|M|re|RE|ve|VE|ll|LL))"; patterns += contraction // an apostrophe, preceded by a non-consumed letter, followed by patterns of contractions
  val apword = s"${ap}n($ap)?|${ap2}em|[OoDdLl]${ap}$letter+|[Oo]${ap2}clock|add${ap2}l|[Cc]${ap2}mon|${ap2}cause|${ap}till?|ol$ap|Dunkin$ap|$ap[1-9]0s|N$ap|\\p{L}\\p{Ll}*[aeiou]$ap[aeiou]\\p{Ll}*"; patterns += apword // words that include an apostrophe, like O'Reilly, C'mon, 'n', Shi'ite, 20's, N'goma
  //val ing = s"[A-Za-z]{3,}in${ap}"; patterns += ing // fishin' (but this pattern also gets all but the last character of "Britain's" :-(  // TODO Try to find some more specific fix for this
  val initials = "[\\p{L}]\\.[\\p{L}\\.]*"; patterns += initials // A.  A.A.A.I.  etc.
  //val briefAbbrevs = "[A-Z][a-z]?\\."; patterns += briefAbbrevs // and initials; so includes A. and Mr. but not Mrs. Calif. or Institute.  Removed because otherwise we get "me." and "it."
  val ordinals = "[0-9]{1,4}(st|nd|rd|th)"; patterns += ordinals // like 1st and 22nd
  val quote = "''|``|[\u2018\u2019\u201A\u201B\u201C\u201D\u0091\u0092\u0093\u0094\u201A\u201E\u201F\u2039\u203A\u00AB\u00BB]{1,2}|[\"\u201C\u201D\\p{Pf}]|&(quot|[rl][ad]quo);|" + ap2 + "{2}"; patterns += quote
  // List of prefixes taken from http://en.wikipedia.org/wiki/English_prefixes with the addition of "e", "uh" and "x" from Ontonotes examples.
  if(tokenizeConnl03) { val dashedWord = s"($letter)([\\p{L}\\p{M}\\p{Nd}_]*(-[\\p{L}\\p{M}\\p{Nd}_]*)*)"; patterns += dashedWord }
  val dashedPrefixes = "(?i:a|anti|arch|be|co|counter|de|dis|e|en|em|ex|fore|hi|hind|mal|mid|midi|mini|mis|out|over|part|post|pre|pro|re|self|step|t|trans|twi|un|under|up|with|Afro|ambi|amphi|an|ana|Anglo|ante|apo|astro|auto|bi|bio|circum|cis|con|com|col|cor|contra|cryo|crypto|de|demi|demo|deutero|deuter|di|dia|dis|dif|du|duo|eco|electro|e|en|epi|Euro|ex|extra|fin|Franco|geo|gyro|hetero|hemi|homo|hydro|hyper|hypo|ideo|idio|in|Indo|in|infra|inter|intra|iso|macro|maxi|mega|meta|micro|mono|multi|neo|non|omni|ortho|paleo|pan|para|ped|per|peri|photo|pod|poly|post|pre|preter|pro|pros|proto|pseudo|pyro|quasi|retro|semi|socio|sub|sup|super|supra|sur|syn|tele|trans|tri|uh|ultra|uni|vice|x)"
  val dashedSuffixes = "(?i:able|ahol|aholic|ation|centric|cracy|crat|dom|e-\\p{L}+|er|ery|esque|ette|fest|fi|fold|ful|gate|gon|hood|ian|ible|ing|isation|ise|ising|ism|ist|itis|ization|ize|izing|less|logist|logy|ly|most|o-torium|rama|ise)"
  val dashedPrefixWord = dashedPrefixes+"-[\\p{L}\\p{M}][\\p{L}\\p{M}\\p{Nd}]*"; patterns += dashedPrefixWord // Dashed words with certain prefixes, like "trans-ocean" or "Afro-pop"
  val dashedSuffixWord = "[\\p{L}\\p{M}\\p{N}]+-"+dashedSuffixes+s"(?!$letter)"; patterns += dashedSuffixWord // Dashed words with certain suffixes, like "senior-itis" // TODO Consider a dashedPrefixSuffixWord?
  // common dashed words in Ontonotes include counter-, ultra-, eastern-, quasi-, trans-,  
  val fraction = "[\u00BC\u00BD\u00BE\u2153\u2154]|&(frac14|frac12|frac34);|(\\p{N}{1,4}[- \u00A0])?\\p{N}{1,4}(\\\\?/|\u2044)\\p{N}{1,4}"; patterns += fraction
  val contractedWord = s"[\\p{L}\\p{M}]+(?=($contraction))"; patterns += contractedWord // Includes any combination of letters and accent characters before a contraction
  val caps = s"\\p{Lu}+([&+](?!($htmlSymbol|$htmlAccentedLetter))(\\p{Lu}(?!\\p{Ll}))+)+"; patterns += caps // For "AT&T" but don't grab "LAT&Eacute;" and be sure not to grab "PE&gym"
  val word = s"($letter)([\\p{L}\\p{M}\\p{Nd}_]|$letter)*(?!-$date)(-\\p{Nd}+)?"; patterns += word // Includes any combination of letters, accent characters, numbers and underscores, dash-followed-by-numbers (as in "G-20" but not "NYT-03-04-2012").  It may include a & as long as it is followed by a letter but not an HTML symbol encoding
  // TODO Not sure why the pattern above is not getting the last character of a word ending in \u00e9 -akm
  val number = s"(?<![\\p{Nd}])[-\\+\\.,]?(?!$date)\\p{Nd}+([\\.:,]\\p{Nd}+)*"; patterns += number // begin with an optional [+-.,] and a number, followed by numbers or .:, punc, ending in number.  Avoid matching dates inside "NYT-03-04-2012".  Cannot be preceded by number (or letter? why?  do we need "USD32"?), in order to separate "1989-1990" into three tokens.
  val number2 = ap+"\\p{Nd}{2}"; patterns += number2 // For years, like '91
  patterns += ap2 // Defined earlier for embedded use, but don't include in patterns until here
  val ellipsis = "\\.{2,5}|(\\.[ \u00A0]){2,4}\\.|[\u0085\u2026]"; patterns += ellipsis // catch the ellipsis not captured in repeatedPunc, such as ". . ." and unicode ellipsis.  Include \\.{2,5} for use in TokenNormalizer1
  val repeatedPunc = "[\\*=\\+\\.\\?!#]+|-{4,}"; patterns += repeatedPunc // probably used as ASCII art
  val mdash = "-{2,3}|&(mdash|MD);|[\u2014\u2015]"; patterns += mdash
  val dash = "&(ndash);|[-\u0096\u0097\\p{Pd}]"; patterns += dash // I think \p{Pd} should include \u2013\u2014\u2015
  val punc = "\\p{P}"; patterns += punc // This matches any kind of punctuation as a single character, so any special handling of multiple punc together must be above, e.g. ``, ---
  val symbol = "\\p{S}|&(degree|plusmn|times|divide|infin);"; patterns += symbol
  val htmlChar = "&[a-z]{3,6};"; patterns += htmlChar // Catch-all, after the more specific cases above, including htmlSymbol
  val catchAll = "\\S"; patterns += catchAll // Any non-space character.  Sometimes, due to contextual restrictions above, some printed characters can slip through.  It will probably be an error, but at least the users will see them with this pattern.
  val newline = "\n"; if (tokenizeNewline) patterns += newline
  val space = "(\\p{Z}|&nbsp;)+" // but not tokenized here

  val tokenRegexString = patterns.filter(_.length != 0).mkString("|")
  val tokenRegex = if (!caseSensitive) ("(?i)"+tokenRegexString).r else tokenRegexString.r

  def process(document: Document): Document = {
    for (section <- document.sections) {
      var prevTokenPeriod = false // Does the previous Token.string consist entirely of .
      val tokenIterator = tokenRegex.findAllIn(section.string)
      while (tokenIterator.hasNext) {
        tokenIterator.next()
        val string = document.string.substring(section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        if (prevTokenPeriod && java.lang.Character.isLowerCase(string(0)) && section.length > 1 && section.tokens(section.length-2).stringEnd == section.tokens(section.length-1).stringStart) {
          // If we have a pattern like "Abbrev. is" with token strings "Abbrev", ".", "is" (currently looking at "is")
          // then assume that the previous-previous word is actually an abbreviation; patch it up to become "Abbrev.", "is"
          val lastTwoTokens = section.takeRight(2).toIndexedSeq
          section.remove(section.length-1); section.remove(section.length-1)
          new Token(section, lastTwoTokens(0).stringStart, lastTwoTokens(1).stringEnd)
          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        } else if (tokenizeNewline && string == "\n") {
          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        } else if (tokenizeSgml || 
            !((string(0) == '<' && string(string.length-1) == '>') // We have an SGML tag 
              || (string(0) == '&' && string(string.length-1) == ';') // We have an odd escaped SGML tag &gt;...&lt;
              || string.toLowerCase == "&nbsp;" // Don't make token from space
              )
        ) {
          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
        }
        if (string == ".") prevTokenPeriod = true else prevTokenPeriod = false
      }
    }
    document
  }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = List(classOf[Token])
  
  /** Convenience function to run the tokenizer on an arbitrary String.  The implementation builds a Document internally, then maps to token strings. */
  def apply(s:String): Seq[String] = process(new Document(s)).tokens.toSeq.map(_.string)
}

object Tokenizer1 extends Tokenizer1(false, false, false, false) {
  def main(args: Array[String]): Unit = {
    val string = io.Source.fromInputStream(System.in).mkString
    val doc = new Document(string)
    Tokenizer1.process(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}
