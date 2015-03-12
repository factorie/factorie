package cc.factorie.app.nlp.segment;

/**
 *  A tokenizer for English
 */

%%

%class EnglishLexer
%unicode
%function next
%type Object
%char
%caseless

%{

  val tokenizeNewline = false
  val tokenizeWhitespace = false
  val tokenizeAllDashedWords = false

  def getNext(): Object = {
    val txt = yytext()
    getNext(txt, txt)
  }

  def getNext(txt: String, originalText: String): Object = Array(yychar, yylength())

  def printDebug(tok: String) = println(s"$tok: |${yytext()}|")

%}

/* The [^\u0000] ensures we match newline also */
//HTML = (<script[^>]*>([^\u0000](?!<\/script>))*[^\u0000]?<\/script>)|(<style[^>]*>([^\u0000](?!<\/style>))*[^\u0000]?<\/style>)
//HTML_COMMENT = (<|&lt;)!--([^\u0000](?!-->))*[^\u0000]?--(>|&gt;)

/* Some HTML contains "<% blah %>" tags. */
//SGML2 = <%([^\u0000](?!%>))*[^\u0000]?%>

/* Closing with "%>" doesn't count */
//SGML = <\/?[A-Za-z!].*?(?<!%)>
// this requires at least one character inside the tag...
SGML = <\/?[A-Za-z!].*?[^%]>

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

// (?:US|AU|NZ|C|CA|FJ|JY|HK|JM|KY|LR|NA|SB|SG|NT|BB|XC|BM|BN|BS|BZ|ZB|B)?\\$|&(?:euro|cent|pound);|\\p{Sc}|(?:USD|EUR|JPY|GBP|CHF|CAD|KPW|RMB|CNY|AD|GMT)(?![A-Z])
CURRENCY1 = ((US|AU|NZ|C|CA|FJ|JY|HK|JM|KY|LR|NA|SB|SG|NT|BB|XC|BM|BN|BS|BZ|ZB|B)?\$)|(&(euro|cent|pound);)|\p{Sc}
CURRENCY2 = (USD|EUR|JPY|GBP|CHF|CAD|KPW|RMB|CNY|AD|GMT)

/* For Twitter */
HASHTAG = #[A-Za-z][A-Za-z0-9]+
ATUSER = @[A-Za-z][A-Za-z0-9]+

/* Optional hat, eyes, optional repeated nose, mouth{1,5}, optional beard.  Or horizontal eyes '.' */
// [#<%\*]?[:;!#\$%@=\|][-\+\*=o^<]{0,4}[\(\)oODPQX\*3{}\[\]]{1,5}[#><\)\(]?(?!\S)|'\.'
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
//INITIALS = (\p{L}\.)+\.?
INITIALS = {SINGLE_INITIAL}{SINGLE_INITIAL}+

/* like 1st and 22nd */
ORDINALS = [0-9]{1,4}(st|nd|rd|th)

QUOTE = ''|``|[\u2018\u2019\u201A\u201B\u201C\u201D\u0091\u0092\u0093\u0094\u201A\u201E\u201F\u2039\u203A\u00AB\u00BB]{1,2}|[\"\u201C\u201D\p{Pf}]|&(quot|[rl][ad]quo);|{AP2}{2}
DASHEDWORD = ({LETTER})([\p{L}\p{M}\p{Nd}_]*(-[\p{L}\p{M}\p{Nd}_]*)*)

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
//CAPS = \p{Lu}+([&+](?!({HTML_SYMBOL}|{HTML_ACCENTED_LETTER}))(\p{Lu}(?!\p{Ll}))+)+
CAPS = [\p{Lu}]+[&+][\p{Lu}]+

/* Includes any combination of letters, accent characters, numbers and underscores, dash-followed-by-numbers (as in "G-20" but not "NYT-03-04-2012").  It may include a & as long as it is followed by a letter but not an HTML symbol encoding */
/* TODO Not sure why the pattern below is not getting the last character of a word ending in \u00e9 -akm */
//WORD = (\p{Nd}{LETTER}|{LETTER})([\p{L}\p{M}\p{Nd}_]|{LETTER})*+
WORD = {LETTER}([\p{L}\p{M}\p{Nd}_]|{LETTER})*+

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

/* probably used as ASCII art */
REPEATED_PUNC = [,~\*=\+\.\?!#]+|(----+)
MDASH = -{2,3}|&(mdash|MD);|[\u2014\u2015]

/* I think \p{Pd} should include \u2013\u2014\u2015 */
DASH = &(ndash);|[-\u0096\u0097\p{Pd}]

/* This matches any kind of punctuation as a single character, so any special handling of multiple punc together must be above, e.g. ``, --- */
PUNC = \p{P}

SYMBOL = \p{S}|&(degree|plusmn|times|divide|infin);

/* Catch-all, after the more specific cases above, including htmlSymbol */
HTMLCHAR = &[a-z]{3,6};

NEWLINE = \R

WHITESPACE = ([\p{Z}\t\f]|&nbsp;)+

/* Any non-space character. Sometimes, due to contextual restrictions above, some printed characters can slip through.
   It will probably be an error, but at least users will see them with this pattern. */
CATCHALL = \P{C}

%%

//{HTML} { if(!tokenizeSgml) getNext() else null }

//{HTML_COMMENT} { getNext() }

//{SGML2} { getNext() }

{SGML} { printDebug("SGML"); getNext() }

{HTML_SYMBOL} { printDebug("HTML_SYMBOL"); getNext() }

{URL} { printDebug("URL"); getNext() }
{URL2} { printDebug("URL2"); getNext() }
{URL3} { printDebug("URL3"); getNext() }

{EMAIL} { printDebug("EMAIL"); getNext() }
{USPHONE} { printDebug("USPHONE"); getNext() }
{FRPHONE} {printDebug("FRPHONE");  getNext() }

{DATE} / [^0-9] { printDebug("DATE"); getNext() }
{DECADE} { printDebug("DECADE"); getNext() }

/* For some reason combining these using | makes the lexer go into an infinite loop */
/* CURRENCY2 will be matched as word and not currency if it occurs at the end of the file, not sure how to fix */
{CURRENCY1} { printDebug("CURRENCY"); getNext() }
{CURRENCY2} / [^A-Z] { printDebug("CURRENCY"); getNext() }

{HASHTAG} { printDebug("HASHTAG"); getNext() }
{ATUSER} { printDebug("ATUSER"); getNext() }

// [#<%\*]?[:;!#\$%@=\|][-\+\*=o^<]{0,4}[\(\)oODPQX\*3{}\[\]]{1,5}[#><\)\(]?(?!\S)|'\.'
'\.' |
{EMOTICON} / {WHITESPACE}|{NEWLINE} { printDebug("EMOTICON"); getNext() }


{FILENAME} { printDebug("FILENAME"); getNext() }

{CONSONANT_NON_ABBREVS} / \. { printDebug("CONSONANT_NON_ABBREVS"); getNext() }

({MONTH}|{DAY}|{STATE}|{STATE2}|{HONORIFIC}|{SUFFIX}|{PLACE}|{UNITS}|{ORG}|{LATIN}|{ABBREV})\. { printDebug("ABBREVS"); getNext() }

{NOABBREV} / \p{Z}*\p{Nd} { printDebug("NOABBREV"); getNext() }
{LATIN2} / \P{L} { printDebug("LATIN2"); getNext() }

{HTML_ACCENTED_LETTER} { printDebug("HTML_ACCENTED_LETTER"); getNext() }

/* contractions without apostrophes */
what / cha { printDebug("whatcha"); getNext() }
wan / na { printDebug("wanna"); getNext() }

{CONTRACTION} / \P{L} { printDebug("CONTRACTION"); getNext() }

{APWORD} { printDebug("APWORD"); getNext() }

/* [^\p{P}\p{S}] should be non-punctuation */
/* A.B  This must come before 'initials' or else 'initials' will capture the prefix. */
/* \P{P} is non-punctuation */
{INITIALS2} / \P{P} { printDebug("INITIALS2"); getNext() }

{INITIALS} { printDebug("INITIALS"); getNext() }

{SINGLE_INITIAL} / {WHITESPACE} { printDebug("SINGLE_INITIAL"); getNext() }

//{INITIALS} / [^.\p{L}] {
//  printDebug("INITIALS");
//  val matched = yytext()
//  if(matched.endsWith("..")) yypushback(1)
//  getNext()
//}

{ORDINALS} { printDebug("ORDINALS"); getNext() }

{QUOTE} { printDebug("QUOTE"); getNext() }

// TODO deal with this: if this is here then we will never match following {DASHED_PREFIX_WORD}
//{DASHEDWORD} { printDebug("DASHEDWORD"); if (tokenizeAllDashedWords) getNext() else null }

{DASHED_PREFIX_WORD} { printDebug("DASHED_PREFIX_WORD"); getNext() }
{DASHED_SUFFIX_WORD} / [^\p{L}\p{M}&] { printDebug("DASHED_SUFFIX_WORD"); getNext() }

{FRACTION} { printDebug("FRACTION"); getNext() }

{CONTRACTED_WORD} / {CONTRACTION} { printDebug("CONTRACTED_WORD"); getNext() }

{CAPS} / [^\p{Ll}] { printDebug("CAPS"); getNext() }

{WORD} { printDebug("WORD"); getNext() }

{NUMBER} { printDebug("NUMBER"); getNext() }

{NUMBER2} { printDebug("NUMBER2"); getNext() }

{AP2} { printDebug("AP2"); getNext() }

{ELLIPSIS} { printDebug("ELLIPSIS"); getNext() }
\.{2,5} / [^!?] { printDebug("ELLIPSIS"); getNext() }

{REPEATED_PUNC} { printDebug("REPEATED_PUNC"); getNext() }

{MDASH} { printDebug("MDASH"); getNext() }

{DASH} { printDebug("DASH"); getNext() }

{PUNC} { printDebug("PUNC"); getNext() }

{SYMBOL} { printDebug("SYMBOL"); getNext() }

{HTMLCHAR} { printDebug("HTMLCHAR"); getNext() }

{NEWLINE} { printDebug("NEWLINE"); if (tokenizeNewline) getNext() else null }

{WHITESPACE} { printDebug("WHITESPACE"); if(tokenizeWhitespace) getNext() else null}

{CATCHALL} { printDebug("CATCHALL"); getNext() }

/* The below rules are duplicated here (without lookahead) in order to match these patterns
   when they occur right at the end of the file (nothing to look ahead to so originals never match) */
//{DATE} { printDebug("DATE"); getNext() }
//{LATIN2} { printDebug("LATIN2"); getNext() }
//{CONTRACTION} { printDebug("CONTRACTION"); getNext() }
//{INITIALS} {
//  printDebug("INITIALS");
//  val matched = yytext()
//  if(matched.endsWith("..")) yypushback(1)
//  getNext()
//}
//{DASHED_SUFFIX_WORD} { printDebug("DASHED_SUFFIX_WORD"); getNext() }
//{CAPS} { printDebug("CAPS"); getNext() }


<<EOF>> { null }
