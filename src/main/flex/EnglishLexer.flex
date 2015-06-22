package cc.factorie.app.nlp.segment

import cc.factorie.util.JavaHashMap

/**
 *  A tokenizer for English
 */

%%

%class EnglishLexer
%unicode
%type Object
%char
%caseless

%{

  final val NORMALIZED_LRB = "("
  final val NORMALIZED_RRB = ")"
  final val NORMALIZED_LCB = "{"
  final val NORMALIZED_RCB = "}"
  final val NORMALIZED_LSB = "["
  final val NORMALIZED_RSB = "]"

  final val NORMALIZED_ONE_QUARTER = "1/4"
  final val NORMALIZED_ONE_HALF = "1/2"
  final val NORMALIZED_THREE_QUARTERS = "3/4"
  final val NORMALIZED_ONE_THIRD = "1/3"
  final val NORMALIZED_TWO_THIRDS = "2/3"

  final val NORMALIZED_ELLIPSIS = "..."
  final val NORMALIZED_AMPERSAND = "&"
  final val NORMALIZED_CENTS = "cents"
  final val NORMALIZED_DOLLAR = "$"
  final val NORMALIZED_MDASH = "--"
  final val NORMALIZED_DASH = "-"
  final val NORMALIZED_QUOTE = "\""
  final val NORMALIZED_APOSTROPHE = "'"

  final val UNESCAPED_SLASH = "/"
  final val UNESCAPED_ASTERISK = "*"
  final val UNESCAPED_ASTERISK2 = "**"

  val htmlSymbolMap = JavaHashMap[String,String]()
  htmlSymbolMap ++= List("&lt;" -> "<", "&gt;" -> ">", "&amp;" -> "&", "&copy;" -> "(c)", "&reg;" -> "(r)", "&trade;" -> "(TM)", "&rsquo;" -> "'", "&lsquo;" -> "'") // TODO complete this collection

  var tokenizeSgml = false
  var tokenizeNewline = false
  var tokenizeWhitespace = false
  var tokenizeAllDashedWords = false
  var abbrevPrecedesLowercase = false
  var normalizeQuotes = true // Convert all double quotes to "
  var normalizeApostrophe = true // Convert all apostrophes to ', even within token strings
  var normalizeCurrency = true // Convert all currency symbols to "$", except cents symbol to "cents"
  var normalizeAmpersand = true // Convert all ampersand symbols (including "&amp;" to "&"
  var normalizeFractions = true // Convert unicode fraction characters to their spelled out analogues, like "3/4"
  var normalizeEllipsis = true // Convert unicode ellipsis character to spelled out analogue, "..."
  var undoPennParens = true // Change -LRB- etc to "(" etc.
  var unescapeSlash = true // Change \/ to /
  var unescapeAsterisk = true // Change \* to *
  var normalizeMDash = true // Convert all em-dashes to double dash --
  var normalizeDash = true // Convert all other dashes to single dash -
  var normalizeHtmlSymbol = true // Convert &lt; to <, etc
  var normalizeHtmlAccent = true // Convert Beyonc&eacute; to Beyonce

  def this(in: Reader, tokSgml: Boolean = false, tokNewline: Boolean = false,
            tokWhitespace: Boolean = false, tokDashed: Boolean = false, abbrevPrecedes: Boolean = false,
            normQuote: Boolean = true, normApostrophe: Boolean = true, normCurrency: Boolean = true,
            normAmpersand: Boolean = true, normFractions: Boolean = true,
            normEllipsis: Boolean = true, pennParens: Boolean = true, unescSlash: Boolean = true,
            unescAsterisk: Boolean = true, normMDash: Boolean = true, normDash: Boolean = true,
            normHtmlSymbol: Boolean = true, normHtmlAccent: Boolean = true) = {
    this(in)
    tokenizeSgml = tokSgml
    tokenizeNewline = tokNewline
    tokenizeWhitespace = tokWhitespace
    tokenizeAllDashedWords = tokDashed
    abbrevPrecedesLowercase = abbrevPrecedes
    normalizeQuotes = normQuote
    normalizeApostrophe = normApostrophe
    normalizeCurrency = normCurrency
    normalizeAmpersand = normAmpersand
    normalizeFractions = normFractions
    normalizeEllipsis = normEllipsis
    undoPennParens = pennParens
    unescapeSlash = unescSlash
    unescapeAsterisk = unescAsterisk
    normalizeMDash = normMDash
    normalizeDash = normDash
    normalizeHtmlSymbol = normHtmlSymbol
    normalizeHtmlAccent = normHtmlAccent
  }

  def tok(): Object = tok(yytext())

  def tok(txt: String): Object = (txt, yychar, yylength)

  /* Uncomment below for useful debugging output */
  def printDebug(tok: String) = {}//println(s"$tok: |${yytext()}|")

%}

/* The [^\u0000] ensures we match newline also */
//HTML = (<script[^>]*>([^\u0000](?!<\/script>))*[^\u0000]?<\/script>)|(<style[^>]*>([^\u0000](?!<\/style>))*[^\u0000]?<\/style>)
//HTML_COMMENT = (<|&lt;)!--([^\u0000](?!-->))*[^\u0000]?--(>|&gt;)

/* Some HTML contains "<% blah %>" tags. */
//SGML2 = <%([^\u0000](?!%>))*[^\u0000]?%>

/* Closing with "%>" doesn't count */
// we require at least one character inside the tag, otherwise it's REPEATED_PUNC
SGML1 = <\/?[A-Za-z!].*?[^%]>
SGML2 = <\/?[A-Za-z!]>

/* TODO make this list complete */
HTML_SYMBOL = &(HT|TL|UR|LR|QC|QL|QR|amp|copy|reg|trade|odq|nbsp|cdq|lt|gt|#[0-9A-Za-z]+);

URL = https?:\/\/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-]

/* Common simple URL without the http */
URL2 = ((www\.([^ \t\n\f\r\"<>|.!?(){},]+\.)+[a-zA-Z]{2,4})|(([^ \t\n\f\r\"`'<>|.!?(){},-_$]+\.)+(com|org|net|edu|gov|cc|info|uk|de|fr|ca)))(\/[^ \t\n\f\r\"<>|()]+[^ \t\n\f\r\"<>|.!?(){},-])?

/* Common, simple URL without the http or www. */
URL3 = [A-Z]*[a-z0-9]+\.(com|org|net|edu|gov|co\.uk|ac\.uk|de|fr|ca)

EMAIL = (mailto:)?\w+[-\+\.'\w]*@(\w+[-\.\+\w]*\.)*\w+
USPHONE = (\+?1[-\. \u00A0]?)?(\([0-9]{3}\)[ \u00A0]?|[0-9]{3}[- \u00A0.])[0-9]{3}[- \u00A0.][0-9]{4}
FRPHONE = (\+33)?(\s[012345][-\. ])?[0-9]([-\. ][0-9]{2}){3}

/* e.g. 3/4/1992 or 2012-04-05, but don't match just the first 8 chars of 12-25-1112 */
DATE = (((19|20)?[0-9]{2}[\-\/][0-3]?[0-9][\-\/][0-3]?[0-9])|([0-3]?[0-9][\-\/][0-3]?[0-9][\-\/](19|20)?[0-9]{2}))
DECADE = (19|20)?[0-9]0s

CURRENCY1 = ((US|AU|NZ|C|CA|FJ|JY|HK|JM|KY|LR|NA|SB|SG|NT|BB|XC|BM|BN|BS|BZ|ZB|B)?\$)|(&(euro|cent|pound);)|\p{Sc}
CURRENCY2 = (USD|EUR|JPY|GBP|CHF|CAD|KPW|RMB|CNY|AD|GMT)

/* For Twitter */
HASHTAG = #[A-Za-z][A-Za-z0-9]+
ATUSER = @[A-Za-z][A-Za-z0-9]+

/* Optional hat, eyes, optional repeated nose, mouth{1,5}, optional beard.  Or horizontal eyes '.' */
EMOTICON = [#<%\*]?[:;!#\$%@=\|][-\+\*=o\^<]{0,4}[\(\)oODPQX\*3{}\[\]]{1,5}[#><\)\(]?

FILENAME = \S+\.(3gp|7z|ace|ai(f){0,2}|amr|asf|asp(x)?|asx|avi|bat|bin|bmp|bup|cab|cbr|cd(a|l|r)|chm|dat|divx|dll|dmg|doc|dss|dvf|dwg|eml|eps|exe|fl(a|v)|gif|gz|hqx|(s)?htm(l)?|ifo|indd|iso|jar|jsp|jp(e)?g|key|lnk|log|m4(a|b|p|v)|mcd|mdb|mid|mov|mp(2|3|4)|mp(e)?g|ms(i|wmm)|numbers|ogg|pages|pdf|php|png|pps|ppt|ps(d|t)?|Penn|pub|qb(b|w)|qxd|ra(m|r)|rm(vb)?|rtf|se(a|s)|sit(x)?|sql|ss|swf|tgz|tif|torrent|ttf|txt|vcd|vob|wav|wm(a|v)|wp(d|s)|xls|xml|xtm|zip)

/* Abbreviation handling */

/* ABBREV below matches all sequences of consonants followed by a period; these are exceptions to that rule */
CONSONANT_NON_ABBREVS = (Ng|cwm|nth|pm)

/* Note that "May" is not included because it is not an abbreviation */
MONTH = Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec

DAY = Mon|Tue|Tues|Wed|Thu|Thurs|Fri
STATE = Ala|Alab|Ariz|Ark|Calif|Colo|Conn|Del|Fla|Ill|Ind|Kans|Kan|Ken|Kent|Mass|Mich|Minn|Miss|Mont|Nebr|Neb|Nev|Dak|Okla|Oreg|Tenn|Tex|Virg|Wash|Wis|Wyo
STATE2 = Ak|As|Az|Ar|Ca|Co|Ct|De|Fm|Fl|Ga|Gu|Hi|Id|Il|Ia|Ks|Ky|La|Mh|Md|Ma|Mi|Mn|Ms|Mo|Mt|Ne|Nv|Mp|Pw|Pa|Pr|Tn|Tx|Ut|Vt|Vi|Va|Wa|Wi|Wy

/* Removed two-word state abbreviations and also ME, OK, OR, OH */
HONORIFIC = Adm|Attys?|Brig|Capts?|Cols?|Comms?|Co?mdrs?|Cpls?|Cpts?|Det|Drs?|Hon|Gens?|Govs?|Lieuts?|Lts?|Majs?|Miss|Messrs|Mr|Mrs|Ms|Pfc|Pres|Profs?|Pvts?|Reps?|Revs?|Sens?|Sgts?|Spc|Supts?
SUFFIX = Bros?|Esq|Jr|Ph\.?[Dd]|Sr
PLACE = Aly|Anx|Ave?|Avn|Blvd|Boul|Byp|Cir|Cor|Rd|Squ|Sta|Ste?|Str|Ln|Mt

/* not covered by "sequence of consonants" in abbrev below; note the abbrev pattern below insists on initial capital letter */
UNITS = in|fur|mi|lea|drc|oz|qtr|cwt
ORG = Alt|Assns?|Bancorp|Bhd|Cos?|Comm|Comp|Corps?|Depts?|Elec|Inc|Inst|Intl|Lib|Ltd|M[ft]g|Mus|Natl|Plc|Pty|Sci|Ser|Sys|Univ
LATIN = e.g|i.e
ABBREV = etc|vol|rev|dea|div|ests?|exp|exts?|gal|[BCDFGHJKLMNPQRSTVWX][bcdfghjklmnpqrstvwx]+

/* Capture "No." when followed by a number (as in "No. 5", where it is an abbreviation of "number"). TODO Consider a token normalization to "number"? -akm */
NOABBREV = [Nn]o\.
LATIN2 = (i.e|e.g)

/* TODO Make this list complete; see http://www.starr.net/is/type/htmlcodes.html */
HTML_ACCENTED_LETTER = &[aeiouyntlAEIOUYNTL](acute|grave|uml|circ|orn|tilde|ring);
LETTER = [\p{L}\p{M}]|{HTML_ACCENTED_LETTER}

/* apostrophe and open single quotes */
AP = (['\u0092\u2019]|&(apos|rsquo|#00?39|#00?92|#2019);)

/* also includes backwards apostrophe and single quotes, but not double quotes */
AP2 = {AP}|&lsquo;|[`\u0091\u2018\u201B]

/* an apostrophe, preceded by a non-consumed letter, followed by patterns of contractions */
//CONTRACTION = ([nN]{AP}[tT]|(?<=\p{L}){AP}(d|D|s|S|m|M|re|RE|ve|VE|ll|LL)(?!\p{L}))
CONTRACTION = [nN]{AP}[tT]|{AP}([dDsSmM]|re|RE|ve|VE|ll|LL)

/* words that include an apostrophe, like O'Reilly, C'mon, 'n', Shi'ite, 20's, N'goma */
APWORD = {AP}nt|{AP}n({AP})?|{AP2}em|[OoDdLl]{AP}{LETTER}+|[Oo]{AP2}clock|ma{AP2}am|add{AP2}l|[Cc]{AP2}mon|{AP2}cause|{AP}till?|ol{AP}|Dunkin{AP}|{AP}[1-9]0s|N{AP}|\p{L}\p{Ll}*[aeiou]{AP}[aeiou]\p{Ll}*

/* Poorly formed initials, as in "A.B". This must come before 'initials' or else 'initials' will capture the prefix. */
INITIALS2 = \p{L}(\.\p{L})+

SINGLE_INITIAL = (\p{L}\.)

/* A.  A.A.A.I.  and U.S. in "U.S..", etc., but not A... or A..B */
//INITIALS = (\p{L}\.)+(?![\.!\?]{2}|\.\p{L})
INITIALS = {SINGLE_INITIAL}{SINGLE_INITIAL}+

/* like 1st and 22nd */
ORDINALS = [0-9]{1,4}(st|nd|rd|th)

QUOTE = ''|``|[\u2018\u2019\u201A\u201B\u201C\u201D\u0091\u0092\u0093\u0094\u201A\u201E\u201F\u2039\u203A\u00AB\u00BB]{1,2}|[\"\u201C\u201D\p{Pf}]|&(quot|[rl][ad]quo);|{AP2}{2}

/* List of prefixes taken from http://en.wikipedia.org/wiki/English_prefixes with the addition of "e", "uh" and "x" from Ontonotes examples. */
/* TODO these should be case-insensitive */
DASHED_PREFIXES = a|anti|arch|be|co|counter|cross|de|dis|e|en|em|ex|fore|hi|hind|mal|mid|midi|mini|mis|o|out|over|part|post|pre|pro|re|self|step|t|trans|twi|un|under|up|with|Afro|ambi|amphi|an|ana|Anglo|ante|apo|astro|auto|bi|bio|circum|cis|con|com|col|cor|contra|cryo|crypto|de|demi|demo|deutero|deuter|di|dia|dis|dif|du|duo|eco|electro|e|en|epi|Euro|ex|extra|fin|Franco|geo|gyro|hetero|hemi|homo|hydro|hyper|hypo|ideo|idio|in|Indo|in|infra|inter|intra|iso|macro|maxi|mega|meta|micro|mono|multi|neo|non|omni|ortho|paleo|pan|para|ped|per|peri|photo|pod|poly|post|pre|preter|pro|pros|proto|pseudo|pyro|quasi|retro|semi|socio|sub|sup|super|supra|sur|syn|tele|trans|tri|uh|ultra|uni|vice|x
DASHED_SUFFIXES = able|ahol|aholic|ation|centric|cracy|crat|dom|e-\p{L}+|er|ery|esque|ette|fest|fi|fold|ful|gate|gon|hood|ian|ible|ing|isation|ise|ising|ism|ist|itis|ization|ize|izing|less|logist|logy|ly|most|o-torium|rama|ise

/* Dashed words with certain prefixes, like "trans-ocean" or "Afro-pop" */
DASHED_PREFIX_WORD = {DASHED_PREFIXES}-[\p{L}\p{M}][\p{L}\p{M}\p{Nd}]*

/* Dashed words with certain suffixes, like "senior-itis" TODO Consider a dashedPrefixSuffixWord? */
/* ok changing this to just not be followed by a letter/punct or & ([^\p{L}\p{M}&]) */
//DASHED_SUFFIX_WORD = [\p{L}\p{M}\p{N}]+-{DASHED_SUFFIXES}(?!{LETTER})
DASHED_SUFFIX_WORD = [\p{L}\p{M}\p{N}]+-{DASHED_SUFFIXES}

/* common dashed words in Ontonotes include counter-, ultra-, eastern-, quasi-, trans-, ... */
FRACTION = [\u00BC\u00BD\u00BE\u2153\u2154]|&(frac14|frac12|frac34);|(\p{N}{1,4}[- \u00A0])?\p{N}{1,4}(\\?\/|\u2044)\p{N}{1,4}

/* Includes any combination of letters and accent characters before a contraction */
CONTRACTED_WORD = [\p{L}\p{M}]+

/* For "AT&T" but don't grab "LAT&Eacute;" and be sure not to grab "PE&gym" */
/* Unicode character classes are inside [] character classes to make sure case is not ignored in this case (ha) */
CAPS = [\p{Lu}]+[&+][\p{Lu}]+

/* Includes any combination of letters, accent characters, numbers and underscores, dash-followed-by-numbers (as in "G-20" but not "NYT-03-04-2012").  It may include a & as long as it is followed by a letter but not an HTML symbol encoding */
/* TODO Not sure why the pattern below is not getting the last character of a word ending in \u00e9 -akm */
WORD = {LETTER}([\p{Nd}_]|{LETTER})*

/* begin with an optional [+-.,] and a number, followed by numbers or .:, punc, ending in number.  Avoid matching dates inside "NYT-03-04-2012".  Cannot be preceded by number (or letter? why?  do we need "USD32"?), in order to separate "1989-1990" into three tokens. */
/* Ok, breaking this not matching dates inside NYT-03-04-2012 thing for simplicity -- do we even need to not match that? */
/* Also, why was there a * at the end?? */
//NUMBER = (?<![\p{Nd}])[-\+\.,]?(?!{DATE})\p{Nd}+([\.:,]\p{Nd}+)*
NUMBER = [-\+\.,]?\p{Nd}+([\.:,]\p{Nd}+)*

/* For years, like '91 */
NUMBER2 = {AP}\p{Nd}{2}

/* catch the ellipsis not captured in repeatedPunc, such as ". . ." and unicode ellipsis.  Include \.{2,5} for use in TokenNormalizer1
   Don't capture "...?!"; let repeatedPunc do that. */
ELLIPSIS = (\.[ \u00A0]){2,4}\.|[\u0085\u2026]

/* This matches any kind of punctuation as a single character, so any special handling of multiple punc together must be above, e.g. ``, --- */
PUNC = \p{P}

/* This should match an escaped forward slash: \/ */
ESCAPED_SLASH = \\\/
ESCAPED_ASTERISK = \\\*
ESCAPED_ASTERISK2 = {ESCAPED_ASTERISK}{ESCAPED_ASTERISK}

/* probably used as ASCII art */
REPEATED_PUNC = [,~\*=\+\.\?!#]+|(----+)

/* I think \p{Pd} should include \u2013\u2014\u2015 */
DASH = &(ndash);|[-\u0096\u0097\p{Pd}]
MDASH = -{2,3}|&(mdash|MD);|[\u2014\u2015]

SYMBOL = \p{S}|&(degree|plusmn|times|divide|infin);

/* Catch-all, after the more specific cases above, including htmlSymbol */
HTMLCHAR = &[a-z]{3,6};

NEWLINE = \R

WHITESPACE = ([\p{Z}\t\f]|&nbsp;)+

/* Any non-space character. Sometimes, due to contextual restrictions above, some printed characters can slip through.
   It will probably be an error, but at least users will see them with this pattern. */
CATCHALL = \P{C}

%%

{SGML1} |
{SGML2}   { printDebug("SGML"); if(tokenizeSgml) tok() else null}

&amp; { printDebug("AMPERSAND"); if(normalizeAmpersand || normalizeHtmlSymbol) tok(NORMALIZED_AMPERSAND) else tok() }

{HTML_SYMBOL} {
  printDebug("HTML_SYMBOL")
  if(normalizeHtmlSymbol){
    val matched = yytext()
    tok(htmlSymbolMap.getOrElse(matched, matched))
  }
  else tok()
}

{URL} { printDebug("URL"); tok() }
{URL2} { printDebug("URL2"); tok() }
{URL3} { printDebug("URL3"); tok() }

{EMAIL} { printDebug("EMAIL"); tok() }
{USPHONE} { printDebug("USPHONE"); tok() }
{FRPHONE} {printDebug("FRPHONE");  tok() }

{DATE} / [^0-9] { printDebug("DATE"); tok() }
{DECADE} { printDebug("DECADE"); tok() }

/* For some reason combining these using | makes the lexer go into an infinite loop */
{CURRENCY1} {
  printDebug("CURRENCY")
  if(normalizeCurrency){
    yytext() match{
      case "\u00A2" => tok(NORMALIZED_CENTS)
      case _ => tok(NORMALIZED_DOLLAR)
    }
  }
  else tok()
}
{CURRENCY2} / [^a-zA-Z] { printDebug("CURRENCY"); if(normalizeCurrency) tok(NORMALIZED_DOLLAR) else tok() }

{HASHTAG} { printDebug("HASHTAG"); tok() }
{ATUSER} { printDebug("ATUSER"); tok() }

// [#<%\*]?[:;!#\$%@=\|][-\+\*=o^<]{0,4}[\(\)oODPQX\*3{}\[\]]{1,5}[#><\)\(]?(?!\S)|'\.'
//'\.' | // get rid of this emoticon, probably more important that we correctly tokenize ''blah blah'.', which is a real thing
{EMOTICON} / {WHITESPACE}|{NEWLINE} { printDebug("EMOTICON"); tok() }

{FILENAME} { printDebug("FILENAME"); tok() }

{CONSONANT_NON_ABBREVS} / \. { printDebug("CONSONANT_NON_ABBREVS"); tok() }

({MONTH}|{DAY}|{STATE}|{STATE2}|{HONORIFIC}|{SUFFIX}|{PLACE}|{UNITS}|{ORG}|{LATIN}|{ABBREV})\. { printDebug("ABBREVS"); tok() }

{NOABBREV} / \p{Z}*\p{Nd} { printDebug("NOABBREV"); tok() }
{LATIN2} / \P{L} { printDebug("LATIN2"); tok() }

{HTML_ACCENTED_LETTER} { printDebug("HTML_ACCENTED_LETTER"); tok() }

/* contractions without apostrophes */
what / cha { printDebug("whatcha"); tok() }
wan / na { printDebug("wanna"); tok() }

{CONTRACTION} / \P{L} { printDebug("CONTRACTION"); tok() }

{APWORD} { printDebug("APWORD"); tok() }

/* [^\p{P}\p{S}] should be non-punctuation */
/* A.B  This must come before 'initials' or else 'initials' will capture the prefix. */
/* \P{P} is non-punctuation */
{INITIALS2} / \P{P} { printDebug("INITIALS2"); tok() }

{INITIALS} { printDebug("INITIALS"); tok() }

{SINGLE_INITIAL} / {WHITESPACE}|{NEWLINE} { printDebug("SINGLE_INITIAL"); tok() }

{ORDINALS} { printDebug("ORDINALS"); tok() }

{QUOTE} { printDebug("QUOTE"); if(normalizeQuotes) tok(NORMALIZED_QUOTE) else tok() }

{DASHED_PREFIX_WORD} {
  printDebug("DASHED_PREFIX_WORD")
  if(tokenizeAllDashedWords){
    val word = yytext()
    yypushback(yylength() - word.indexOf("-"))
  }
  tok()
}
{DASHED_SUFFIX_WORD} / [^\p{L}\p{M}&] {
  printDebug("DASHED_PREFIX_WORD")
  if(tokenizeAllDashedWords){
    val word = yytext()
    yypushback(yylength() - word.indexOf("-"))
  }
  tok()
}

{FRACTION} {
  printDebug("FRACTION")
  if(normalizeFractions){
    yytext() match{
      case "\u00BC" => tok(NORMALIZED_ONE_QUARTER)
      case "\u00BD" => tok(NORMALIZED_ONE_HALF)
      case "\u00BE" => tok(NORMALIZED_THREE_QUARTERS)
      case "\u2153" => tok(NORMALIZED_ONE_THIRD)
      case "\u2154" => tok(NORMALIZED_TWO_THIRDS)
      case _ => tok()
    }
  }
  else tok()
}

-LRB- { printDebug("-LRB-"); if(undoPennParens) tok(NORMALIZED_LRB) else tok() }
-RRB- { printDebug("-RRB-"); if(undoPennParens) tok(NORMALIZED_RRB) else tok() }
-LCB- { printDebug("-LCB-"); if(undoPennParens) tok(NORMALIZED_LCB) else tok() }
-RCB- { printDebug("-RCB-"); if(undoPennParens) tok(NORMALIZED_RCB) else tok() }
-LSB- { printDebug("-LSB-"); if(undoPennParens) tok(NORMALIZED_LSB) else tok() }
-RSB- { printDebug("-RSB-"); if(undoPennParens) tok(NORMALIZED_RSB) else tok() }

{CONTRACTED_WORD} / {CONTRACTION} { printDebug("CONTRACTED_WORD"); tok() }

{CAPS} / [^\p{Ll}] { printDebug("CAPS"); tok() }

{WORD} {
  printDebug("WORD")
  if(normalizeHtmlAccent){
    val matched = yytext()
    var tokenString = ""
    var startIdx = matched.indexOf('&')
    var lastEnd = 0
    while(startIdx != -1){
      // these are of the form "&Eacute;"; grab just the E
      val endIdx = matched.indexOf(';', lastEnd)
      tokenString += matched.substring(lastEnd, startIdx) + yycharat(startIdx+1)
      lastEnd = endIdx+1
      startIdx = matched.indexOf('&', lastEnd)
    }
    tokenString += matched.substring(lastEnd)
    tok(tokenString)
  }
  else tok()
}

{NUMBER} { printDebug("NUMBER"); tok() }

{NUMBER2} { printDebug("NUMBER2"); tok() }

{AP2} { printDebug("AP2"); if(normalizeApostrophe) tok(NORMALIZED_APOSTROPHE) else tok() }

{ELLIPSIS} { printDebug("ELLIPSIS"); if(normalizeEllipsis) tok(NORMALIZED_ELLIPSIS) else tok() }
\.{2,5} / [^!?] { printDebug("ELLIPSIS"); if(normalizeEllipsis) tok(NORMALIZED_ELLIPSIS) else tok() }

{ESCAPED_SLASH} {printDebug("ESCAPED_SLASH"); if(unescapeSlash) tok(UNESCAPED_SLASH) else tok()}
{ESCAPED_ASTERISK} {printDebug("ESCAPED_ASTERISK"); if(unescapeAsterisk) tok(UNESCAPED_ASTERISK) else tok()}
{ESCAPED_ASTERISK2} {printDebug("ESCAPED_ASTERISK2"); if(unescapeAsterisk) tok(UNESCAPED_ASTERISK2) else tok()}

{REPEATED_PUNC} { printDebug("REPEATED_PUNC"); tok() }

{MDASH} { printDebug("MDASH"); if(normalizeMDash) tok(NORMALIZED_MDASH) else tok() }

{DASH} { printDebug("DASH"); if(normalizeDash) tok(NORMALIZED_DASH) else tok() }

{PUNC} { printDebug("PUNC"); tok() }

{SYMBOL} { printDebug("SYMBOL"); tok() }

{HTMLCHAR} { printDebug("HTMLCHAR"); tok() }

{NEWLINE} { printDebug("NEWLINE"); if (tokenizeWhitespace || tokenizeNewline) tok() else null }

{WHITESPACE} { printDebug("WHITESPACE"); if(tokenizeWhitespace) tok() else null}

{CATCHALL} { printDebug("CATCHALL"); tok() }

/* The only crap left here should be control characters, god forbid... throw them away */
. { printDebug("GARB"); null }

<<EOF>> { null }
