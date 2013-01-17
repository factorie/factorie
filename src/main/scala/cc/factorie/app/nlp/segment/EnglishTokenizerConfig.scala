package cc.factorie.app.nlp.segment

import java.util.regex.Pattern
import java.util.zip.{ZipEntry, ZipInputStream}
import collection.mutable
import java.io.{InputStreamReader, BufferedReader}

trait EnglishTokenizerConfig {
  def M_COMPOUNDS: mutable.HashMap[String, Int]
  def L_COMPOUNDS: mutable.ArrayBuffer[Array[CompoundSpan]]
  def T_EMOTICONS: mutable.Set[String]
  def T_ABBREVIATIONS: mutable.Set[String]
  def P_HYPHEN_LIST: String
  def R_UNIT: Array[String]
}

object EnglishTokenizerConfig {

  private val F_DIR = "tokenize/"
  private val F_EMOTICONS = F_DIR+"emoticons.txt"
  private val F_ABBREVIATIONS = F_DIR+"abbreviations.txt"
  private val F_HYPHENS = F_DIR+"hyphens.txt"
  private val F_COMPOUNDS = F_DIR+"compounds.txt"
  private val F_UNITS = F_DIR+"units.txt"
  private val F_MICROSOFT = F_DIR+"microsoft.txt"

  private val S_DELIM = " "
  private val P_DELIM = Pattern.compile(S_DELIM)

  private val rawDefault = Map(
    "abbreviations" -> List(
      "adm.", "brig.", "capt.", "cmdr.", "co.", "col.", "cpl.", "dr.", "drs.", "fr.", "etc.", "ft.", "gen.", "gov."
    , "lt.", "ltd.", "maj.", "mr.", "mrs.", "ms.", "no.", "ph.d.", "phd.", "prof.", "prop.", "pte.", "pvt.", "rep."
    , "reps.", "rev.", "sen.", "sens.", "sgt.", "st.", "ste.", "vs.")
  , "hyphens" -> List(
      "-able", "-ahol", "-aholic", "-ation", "-centric", "-cracy", "-crat", "-dom", "-e-", "-er", "-ery", "-esque"
    , "-ette", "-fest", "-fold", "-ful", "-gate", "-gon", "-hood", "-ian", "-ible", "-ing", "-isation", "-ise", "-ising"
    , "-ism", "-ist", "-itis", "-ization", "-ize", "-izing", "-less", "-logist", "-logy", "-ly", "-most", "-o-torium"
    , "-rama", "-wise", "ambi-", "amphi-", "ante-", "anti-", "arch-", "be-", "bi-", "bio-", "centi-", "circum-", "cis-"
    , "co-", "counter-", "cran-", "cross-", "cryo-", "crypto-", "de-", "deca-", "demi-", "dis-", "eco-", "electro-"
    , "ennea-", "epi-", "ex-", "extra-", "giga-", "hepta-", "hexa-", "ideo-", "idio-", "in-", "infra-", "inter-", "intra-"
    , "iso-", "macro-", "medi-", "micro-", "mid-", "milli-", "mini-", "mm-hm", "mm-mm", "multi-", "neo-", "non-"
    , "novem-", "o-kay", "octa-", "octo-", "over-", "paleo-", "pan-", "penta-", "peri-", "pica-", "poly-", "post-"
    , "pre-", "preter-", "pro-", "quadri-", "quasi-", "quinque-", "re-", "semi-", "sept-", "soci-", "sub-", "super-"
    , "supra-", "sur-", "tele-", "tera-", "tetra-", "tri-", "u-", "uber-", "uh-huh", "uh-oh", "ultra-", "un-", "uni-"
    , "vice-", "x-ray", "e-mail")
  , "compounds" -> List(
      "ai nt", "are nt", "ca nt", "can not", "could nt", "did nt", "do nt", "does nt", "has nt", "had nt", "is nt"
    , "have nt", "might nt", "must nt", "sha nt", "should nt", "was nt", "were nt", "wo nt", "would nt", "it d", "it ll"
    , "that d", "that ll", "that s", "they d", "they re", "they ve", "we ve", "what re", "what s", "what ve", "what cha"
    , "who d", "who ll", "you d", "you ll", "you ve", "do n' cha", "do n cha", "gim me", "lem me", "wha d ya", "wo n cha"
    , "got ta", "gon na", "wan na", "fin na", "should a", "would a", "could a", "d' ye", "more 'n", "'t is", "'t was"
    , "i 'm ma", "du n no")
  , "units" -> List(
      "\\$", "USD|EUR|JPY|GBP|CHF|CAD|KPW|RMB|CNY|AD|GMT"
    , "m|h|d|s|ms|((k|d|c|m|n|p|f|a|z|y)(m|g))|th|in|ft|yd|ch|fur|mi|lea|gr|drc|oz|lb|st|qtr|cwt|(a|p)m")
  , "emoticons" -> List(
      "!-(", "!:-)", "#(,'%/)", "#)", "#-)", "#-o", "#:-)", "#:-O", "#:-o", "#;-)", "$-)", "$8()", "$:-$", "$:-)"
    , "$_$", "%%-", "%')", "%)", "%*@:-(", "%*}", "%+{", "%+|", "%-", "%-(", "%-(I)", "%-)", "%-\\", "%-^", "%-|"
    , "%-}", "%7<", "%:O", "%OD", "%\\v", "&&&&", "&-L", "&-|", "&.(..", "&:-)", "&:-/", "&:-]", "&:-o-8-<", "&;-P"
    , "&===3", "'-)", "'.'", "':-(", "':-/", "':-", "(", "(#)", "(#-[", "(%)", "(&)", "(''')-.-(''')", "(';.;')"
    , "((", "((((name))))", "((()))", "(()):**", "((:-/", "(({..}))", "()", "())=(", "()_RED_(>", "()}:o)", "(*)"
    , "(*)/(*)", "(*)?(*)", "(*^_^*)", "(*_*)", "(-)", "(-.-)ZZZ", "(-:", "(-::-)", "(-_-)", "(-_-*)", "(.!.)"
    , "(.V.)", "(.\\|/.)", "(//.^)", "(//_^)", "(/|\\)", "(00)", "(8-(1)", "(8-)", "(8-{)}", "(8>/--<", "(:)", "(:)-)"
    , "(:)\\/<", "(:+)", "(:-", "(:-#", "(:-$", "(:-&", "(:-(", "(:-)", "(:-*", "(:-...", "(:-D", "(:-\\", "(:-{~", "(:-|K-"
    , "(::()::)", "(::)<", "(::[]::)", "(:<)", "(:>-<", "(:@@@@@@@@@@@@@@@D", "(:I", "(:V", "(:V)", "(:^(", "(:|", "(;_;)"
    , "(<_>)", "(>'.')>", "(>..)>", "(>.<)", "(@)", "(@_@)", "(U)", "(X^(*", "(^)", "(^:^)", "(^_^)", "(^o^)", "(_)"
    , "(_8^(|)", "(_X_)", "(___)0", "(____)", "(_i_)", "(_|_)", "(`_^)", "(a)", "(ap)", "(au)", "(b)", "(bah)", "(brb)"
    , "(c)", "(c:", "(ci)", "(co)", "(d)", "(e)", "(f)", "(g)", "(h)", "(h5)", "(i)", "(ip)", "(k)", "(l)", "(li)"
    , "(m)", "(m)(m)(m)", "(mo)", "(mp)", "(n)", "(nah)", "(o)", "(o^-^o)", "(o^_^)o", "(o_o)", "(o|o)", "(p)", "(pi)"
    , "(pl)", "(s)", "(sn)", "(so)", "(st)", "(t)", "(tu)", "(um)", "(w)", "(x)", "(xx)", "(y)", "(yn)", "(z)", "({)"
    , "(|)", "(|:-)", "(||)", "(})", "(}{)", "(~)", "(~};)", ")-:", ")8-)", "):-(", "):-)", "*", "*!#*!^*&:-", "*!#*!^*&:-)"
    , "****", "**==", "*-(", "*-)", "*-*", "*8-I", "*:*", "*:-)", "*:o)", "*<(8)~/~<", "*<):O)", "*<:)", "*<:-)", "*<:O)"
    , "*<<<<+", "*<<<<=", "*<|8-P~", "*<|:-)", "*<|:-{)}", "*\\O/*", "*|:-)", "*~*", "+-(", "+-(:-)", "+/'\\", "+:-)"
    , "+O=-)", ",-)", ",-}", ",:-)", "-(:)(0)=8", "---", "---(|||]==[]", "----|}", "-.-", "-6", "-8", "-:-)", "-=#:-)", "->=:-)"
    , "-@--@-", "-_-", "-_-;", "-o-o-", "-{:-]", ".-)", ".-]", "./", "._.", "/'", "//_^", "//o-o\\", "/8^{~", "/:("
    , "/:)", "/:-)", "/\\", "/________|___|", "0*-)", "0-)", "0-<-<):", "0:-)", "0;-)", "0=)", "0___", "0|-)", "2:)", "3:*>"
    , "3:-O", "3:08", "3:8)", "3:[", "3:]", "4:-)", "5:-)", "7:)", "7:-)", "7:^]", "8", "8(:-)", "8)", "8)~~*", "8*)"
    , "8-#", "8-)", "8-*", "8-;)", "8->", "8-O", "8-X", "8-o", "8-|", "8-}", "8:-)", "8:]", "8:^)", "8<", "8<:-)"
    , "8=:-)", "8==3", "8==8", "8>", "8^", "9_9", ":\")", ":#", ":###)", ":#)", ":$", ":$)", ":%)", ":%)%", ":'", ":'("
    , ":'-(", ":'-)", ":(", ":(&", ":((", ":()", ":(:)", ":(?)", ":)", ":)#", ":))", ":)>-", ":)X", ":)]", ":*", ":*("
    , ":*)", ":*)?", ":*)O", ":*-(", ":+(", ":,')", ":-", ":-!", ":-\"", ":-\">", ":-#", ":-#|", ":-$", ":-%", ":-&"
    , ":-'", ":-(", ":-($)", ":-(*)", ":-(0)", ":-(=)", ":-)", ":-)(-:", ":-))", ":-)))", ":-)))))))", ":-)*", ":-)--"
    , ":-)---", ":-)-{8", ":-)8", ":-)>", ":-)X", ":-){", ":-)}", ":-)~", ":-*", ":-,", ":---)", ":-----)", ":--------)"
    , ":------------)", ":-------[", ":-.)", ":-/", ":-0", ":-0>", ":-1", ":-3", ":-6", ":-7", ":-8", ":-8(", ":-9"
    , ":-:", ":-<", ":-=)", ":->", ":->|", ":-?", ":-@", ":-@!", ":-A", ":-B", ":-C", ":-D", ":-E", ":-I", ":-J", ":-M"
    , ":-O", ":-P", ":-P```", ":-Q", ":-R", ":-S", ":-SS", ":-T", ":-V", ":-W", ":-X", ":-Y", ":-Z", ":-[", ":-[]", ":-\\"
    , ":-]", ":-`", ":-`|", ":-c", ":-d", ":-e", ":-h", ":-i", ":-l", ":-o", ":-p", ":-r", ":-s", ":-t", ":-v", ":-w"
    , ":-x", ":-{", ":-{)", ":-{)=", ":-{)}", ":-{0", ":-{>", ":-{}", ":-|", ":-||", ":-}", ":-})", ":-}X", ":.-)"
    , ":..[", ":/)", "://", ":/i", ":0", ":3", ":3-<", ":3-]", ":7/", ":8)", ":8]", "::-)", "::=))", ":<", ":<|", ":="
    , ":=)", ":=8)", ":>", ":>)", ":>)=", ":?)", ":@", ":@)", ":D", ":D<", ":E", ":O", ":O$", ":O)", ":O/", ":O?"
    , ":O\'", ":P", ":X", ":[", ":\\", ":^)", ":^,", ":^D\"", ":^J", ":^o", ":^y", ":_(", ":`-(", ":c", ":d", ":n)"
    , ":o", ":o)", ":p", ":pp", ":s", ":u)", ":v", ":x", ":{", ":|", ":|)", ":}{:", ":~", ":~)", ":~/", ";)", ";-("
    , ";-(!)", ";-)", ";-,", ";-/", ";->", ";-D", ";-P", ";-\\", ";;)", ";O)", ";^)", ";^?", ";_;", ";p", ";~["
    , "<\")))><", "<%)", "<(((\">", "<((((><", "<(*.*<)", "<(-.-)>", "<(-_-)>", "<(..<)", "<(:?)", "<(^.^<)", "<):)", "<):-)"
    , "<):^/", "<*))))><", "<*_*>", "<.....", "<.{{{><", "</3", "<3", "<:-(", "<:-)", "<:-0", "<:-I", "<:-P", "<:3)~"
    , "<:3)~~", "<:>==", "<:I", "<:^]", "<:o)", "<<<<(:", "<<<<(:-)", "<><", "<[.,=.,]-", "<]:o){", "<_<", "<l:0"
    , "<o?o>", "<u3", "<{::}>", "<{^-^}>", "<|", "<|-)=", "<|:-()}", "<|:^0|<", "<~8)", "=", "='(", "=(", "=((", "=)"
    , "=))", "=):-)", "=+=", "=+o()", "=-HHHH", "=-o", "=.=", "=/", "=8)", "=:-#}", "=:-(", "=:-)", "=:-0", "=:-H"
    , "=:7(~~", "=:7)~~", "=:>)", "=;", "=====~", "===O____iii", "=D", "=D>", "=P", "=P~", "=X", "=[", "=\\", "=^)"
    , "=^.^=", "=^w^=", "=_=", "=o", "=o)", "={D", "=|:O}", ">'o'<", ">,<", ">-", ">-)", ">-D", ">.<", ">:)", ">:*"
    , ">:*)", ">:-(", ">:-)", ">:-<", ">:->", ">:-|", ">:D<", ">:O", ">:P", ">:o===Q<", ">;-('", ">;->", ">;3", "><"
    , "><(((\">", ">=(", ">=D", ">=[", ">>", ">O<", ">[I", ">^,,^<", ">^..^<", ">_>", "?$?:-)", "?-(", "?-:", "?:-)"
    , "?:-_]", "?:^[]", "??", "?_?", "@(*o*)@", "@(-_-)@", "@(^_^)@", "@)->-", "@,.,@", "@-)", "@->--", "@-}----"
    , "@:-)", "@:-}", "@:I", "@=", "@=)", "@>---", "@>--;--", "@>-;--", "@>;-----", "@?@", "@@@@:-)", "@@@@@:-)"
    , "@_@", "@x", "@}->--", "@};-", "@};---", "@}>-'-,--", "@~'~~~", "@~)~~~~", "B-)", "B-|", "B:-)", "C(_)", "C8<]"
    , "C8o", "C:", "C:-)", "C:-|", "C=:-)", "C|:-=", "D:", "D:-)", "D=", "DX", "E-:-)", "E-:-I", "E:-)", "G:-)", "H-)"
    , "L-)", "L.", "O", "O-(", "O-)", "O-\\-<]:", "O.o", "O/", "O:)", "O:-)", "O[-<]:", "P-(", "P-)", "P=\\", "Q(''q)"
    , "Q('.'Q)", "Q:-)", "Q:|", "QQ", "T-T", "T.T", "T^T", "T_T", "U", "V.v.V", "X(", "X-(", "X-p", "X:-)", "X:-/"
    , "X:-|", "XD", "X[", "Y_Y", "[-(", "[-o<", "[-x", "[:-)", "[:-}", "[::::]", "[:]", "[:|]", "[II]D", "[_]", "[______]"
    , "\\$$$/", "\\%%%/", "\\&&&/", "\\';./", "\\-o", "\\<><>/", "\\VVV/", "\\:D/", "\\_/", "\\m/", "\\m/(**)\\m/", "\\m/<(^_^)>\\m/"
    , "\\m/>.<\\m/", "\\o/", "\\w/^^,\\w/", "\\~/", "]-I", "]:->", "^", "^&^", "^(..)^", "^,..,^", "^-^", "^.^", "^5", "^?^"
    , "^^", "^^;", "^_^", "^_^''", "^j^", "^o)", "^o^", "^v^", "_/)", "`-`", "`:-)", "b-(", "d:)", "d:-)", "d:-p"
    , "d[-_-]b", "d^_^b", "g-)", "l-)", "l-o", "l8r)", "l_l)", "o()~", "o,..,o", "o-)", "o/", "o/<", "o:-)", "o<[^("
    , "oO~", "o_o", "oxx:{}:::::::>", "o{-<]:", "o|-<", "q:-)", "u_u", "{(:-)", "{:-)", "{:-{)}", "{:o", "{{{***}}}"
    , "{}", "{}{", "|-()", "|-)", "|-D", "|-I", "|-O", "|-P", "|-|", "|:-)", "|:-0", "|:-{)~", "|:-|", "|^O", "|_P"
    , "|___|", "|_|", "|_|'", "}(:-(", "})i({", "}-(((*>", "}---:o>", "}:(", "}:-(", "}:-(=", "}:-)", "}:->", "}:-X"
    , "}:8>", "}:^#)", "}=D", "}=^{|~", "}i{", "}|{", "~", "~#:-(", "~,~", "~:(", "~:-(", "~:-P", "~:0", "~:>", "~:O"
    , "~<:-)", "~<>^>", "~O-O~", "~O><", "~O~", "~X(", "~_^", "~o)", "~oO>", "~~8-O", "~~:-(", "~~~~8}", "�-)", "�O�"))

  val default = load(rawDefault)

  def load(zin: ZipInputStream): EnglishTokenizerConfig = initDictionaries(zin)

  def load(rawConfig: Map[String, Iterable[String]]): EnglishTokenizerConfig = initDictionaries(rawConfig)

  private def initDictionaries(rawConfig: Map[String, Iterable[String]]): EnglishTokenizerConfig = {
    val (mc, lc) = initDictionariesCompounds(rawConfig("compounds"))
    new EnglishTokenizerConfig {
      val M_COMPOUNDS = mc
      val L_COMPOUNDS = lc
      val T_EMOTICONS = getSet(rawConfig("emoticons"))
      val T_ABBREVIATIONS = getSet(rawConfig("abbreviations"))
      val P_HYPHEN_LIST = getHyphenPatterns(rawConfig("hyphens"))
      val R_UNIT = initDictionariesUnits(rawConfig("units"))
    }
  }

  private def initDictionaries(zin: ZipInputStream): EnglishTokenizerConfig = {
    var zEntry: ZipEntry = null
    var filename: String = null

    var mcompounds: mutable.HashMap[String, Int] = null
    var lcompounds: mutable.ArrayBuffer[Array[CompoundSpan]] = null
    var temoticons: mutable.Set[String] = null
    var tabbreviations: mutable.Set[String] = null
    var phyphenlist: String = null
    var runit: Array[String] = null

    while ({zEntry = zin.getNextEntry; zEntry != null}) {
      filename = zEntry.getName
      if (filename.equals(F_EMOTICONS))
        temoticons = getSet(wrapStream(zin))
      else if (filename.equals(F_ABBREVIATIONS))
        tabbreviations = getSet(wrapStream(zin))
      else if (filename.equals(F_HYPHENS))
        phyphenlist = getHyphenPatterns(wrapStream(zin))
      else if (filename.equals(F_COMPOUNDS)) {
        val (mc, lc) = initDictionariesCompounds(wrapStream(zin))
        mcompounds = mc
        lcompounds = lc
      } else if (filename.equals(F_UNITS))
        runit = initDictionariesUnits(wrapStream(zin))
    }

    zin.close()

    new EnglishTokenizerConfig {
      val M_COMPOUNDS = mcompounds
      val L_COMPOUNDS = lcompounds
      val T_EMOTICONS = temoticons
      val T_ABBREVIATIONS = tabbreviations
      val P_HYPHEN_LIST = phyphenlist
      val R_UNIT = runit
    }
  }

  private def getSet(zin: Iterable[String]): mutable.Set[String] = {
    val set = new mutable.HashSet[String]()
    set ++= zin.map(_.trim())
    set
  }

  private def wrapStream(zin: ZipInputStream): Iterable[String] = {
    val fin = new BufferedReader(new InputStreamReader(zin))
    val buf = new mutable.ArrayBuffer[String]()
    var line: String = null
    while ({line = fin.readLine(); line != null}) buf += line
    buf
  }

  private def getHyphenPatterns(zin: Iterable[String]): String = {
    val build = new StringBuilder()
    for (line <- zin) {
      build.append("|")
      build.append(line.trim())
    }
    build.substring(1)
  }

  private def initDictionariesCompounds(zin: Iterable[String]): (mutable.HashMap[String, Int], mutable.ArrayBuffer[Array[CompoundSpan]]) = {
    val mcompounds = new mutable.HashMap[String, Int]()
    val lcompounds = new mutable.ArrayBuffer[Array[CompoundSpan]]()

    var i = 0
    var p: Array[CompoundSpan] = null
    var tmp: Array[String] = null

    for (line <- zin) {
      i += 1
      tmp = P_DELIM.split(line.trim())
      val len = tmp.length
      p = new Array[CompoundSpan](len)

      mcompounds(tmp.mkString) = i
      lcompounds += p

      var bIdx = 0
      for (j <- 0 until len) {
        val eIdx = bIdx + tmp(j).length
        p(j) = new CompoundSpan(bIdx, eIdx)
        bIdx = eIdx
      }
    }

    (mcompounds, lcompounds)
  }

  private def initDictionariesUnits(zin: Iterable[String]): Array[String] = {
    val Seq(signs, currencies, units) = zin.map(_.trim).toSeq
    val runit = new Array[String](4)
    runit(0) = "^(?i)(\\p{Punct}*"+signs+")(\\d)"
    runit(1) = "^(?i)(\\p{Punct}*"+currencies+")(\\d)"
    runit(2) = "(?i)(\\d)("+currencies+"\\p{Punct}*)$"
    runit(3) = "(?i)(\\d)("+units+"\\p{Punct}*)$"
    runit
  }
}