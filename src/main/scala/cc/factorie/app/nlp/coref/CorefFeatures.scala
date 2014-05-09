package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.strings.Stopwords
import scala.collection.mutable
import cc.factorie.app.nlp.phrase.{Number, Gender}
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain

/** Various lazily-evaluated cached characteristics of a Mention, typically attached to a Mention as an attr. */
class MentionCharacteristics(val mention: Mention) {
  import cc.factorie.app.nlp.lexicon
  // TODO These should be cleaned up and made more efficient -akm
  lazy val isPRO = CorefFeatures.posTagsSet.contains(mention.phrase.headToken.posTag.categoryValue)
  lazy val isProper = CorefFeatures.properSet.contains(mention.phrase.headToken.posTag.categoryValue)
  lazy val isNoun = CorefFeatures.nounSet.contains(mention.phrase.headToken.posTag.categoryValue)
  lazy val isPossessive = CorefFeatures.posSet.contains(mention.phrase.headToken.posTag.categoryValue)

  lazy val hasSpeakWord = mention.phrase.exists(s => lexicon.iesl.Say.contains(s.string))
  lazy val wnLemma = WordNet.lemma(mention.phrase.headToken.string, "n")
  lazy val wnSynsets = WordNet.synsets(wnLemma).toSet
  lazy val wnHypernyms = WordNet.hypernyms(wnLemma)
  lazy val wnAntonyms = wnSynsets.flatMap(_.antonyms()).toSet
  lazy val nounWords: Set[String] =
      mention.phrase.tokens.filter(_.posTag.categoryValue.startsWith("N")).map(t => t.string.toLowerCase).toSet
  lazy val lowerCaseHead: String = mention.phrase.headToken.string.toLowerCase // was:
  lazy val lowerCaseString:String =  mention.phrase.string.toLowerCase
  lazy val headPhraseTrim: String = mention.phrase.tokensString(" ").trim
  lazy val nonDeterminerWords: Seq[String] =
    mention.phrase.tokens.filterNot(_.posTag.categoryValue == "DT").map(t => t.string.toLowerCase)
  lazy val initials: String =
      mention.phrase.tokens.map(_.string).filterNot(lexicon.iesl.OrgSuffix.contains).filter(t => t(0).isUpper).map(_(0)).mkString("")
  lazy val predictEntityType: Int = mention.phrase.attr[OntonotesPhraseEntityType].intValue // TODO Why not just name this "entityTypeCategory"? And we should use the intValue instead anyway! -akm?
  lazy val demonym: String = lexicon.iesl.DemonymMap.getOrElse(headPhraseTrim, "")

  lazy val capitalization: Char = {
      if (mention.phrase.length == 1 && mention.phrase.head.positionInSentence == 0) 'u' // mention is the first word in sentence
      else { // This was missing before, and I think this was a serious bug. -akm
        val s = mention.phrase.value.filter(_.posTag.categoryValue.startsWith("N")).map(_.string.trim) // TODO Fix this slow String operation
        if (s.forall(_.forall(_.isUpper))) 'a'
        else if (s.forall(t => t.head.isLetter && t.head.isUpper)) 't'
        else 'f'
      }
    }
  lazy val gender = mention.phrase.attr[Gender].categoryValue
  lazy val number = mention.phrase.attr[Number].categoryValue
  lazy val nounPhraseType = mention.phrase.attr[NounPhraseType].categoryValue
  lazy val genderIndex = mention.phrase.attr[Gender].intValue // .toString // TODO Why work in terms of String instead of Int? -akm
  lazy val numberIndex = mention.phrase.attr[Number].intValue // .toString
  lazy val nounPhraseTypeIndex = mention.phrase.attr[NounPhraseType].intValue
  lazy val headPos = mention.phrase.headToken.posTag.categoryValue

  lazy val acronym: Set[String] = {
    if (mention.phrase.length == 1)
        Set.empty
      else {
        val alt1 = mention.phrase.value.map(_.string.trim).filter(_.exists(_.isLetter)) // tokens that have at least one letter character
        val alt2 = alt1.filterNot(t => Stopwords.contains(t.toLowerCase)) // alt1 tokens excluding stop words
        val alt3 = alt1.filter(_.head.isUpper) // alt1 tokens that are capitalized
        val alt4 = alt2.filter(_.head.isUpper)
        Seq(alt1, alt2, alt3, alt4).map(_.map(_.head).mkString.toLowerCase).toSet
      }
  }

  lazy private val cachedCanonicalPronConjStr = if (isPRO) {
    if (!PronounDictionary.canonicalize(lowerCaseHead).equals("")) {
      PronounDictionary.canonicalize(lowerCaseHead)
    } else {
      lowerCaseHead
    }
  } else {
    nounPhraseType
  }
  def computeCanonicalPronounsConjunctionStr = cachedCanonicalPronConjStr

}

// TODO I think this should be renamed, but I'm not sure to what. -akm
object CorefFeatures {
  val posTagsSet = Set("PRP", "PRP$", "WP", "WP$")
  val properSet = Set("NNP", "NNPS")
  val nounSet = Seq("NN", "NNS")
  val posSet = Seq("POS")


  trait Ternary
  case object True extends Ternary
  case object False extends Ternary
  case object Unknown extends Ternary

  def getPairRelations(s1: Mention, s2: Mention): String = {
    val l1 = s1.phrase.headToken.string.toLowerCase
    val l2 = s2.phrase.headToken.string.toLowerCase
    val s1c = s1.attr[MentionCharacteristics]
    val s2c = s2.attr[MentionCharacteristics]
    if (l1 == l2)
      "match"
    else if (l1.contains(l2) || l2.contains(l1))
      "substring"
    else if (s1c.wnSynsets.exists(a => s2c.wnSynsets.contains(a)))
      "Syn"
    else if (s1c.wnSynsets.exists(a => s2c.wnHypernyms.contains(a)) || s2c.wnSynsets.exists(a => s1c.wnHypernyms.contains(a)))
      "Hyp"
    else if (s1c.wnSynsets.exists(s2c.wnAntonyms.contains))
      "Ant"
    else
      "Mismatch"
  }

  def matchingTokensRelations(m1:Mention, m2:Mention) = {
    import cc.factorie.app.nlp.lexicon
    val set = new mutable.HashSet[String]()
    val m1c = m1.attr[MentionCharacteristics]
    val m2c = m2.attr[MentionCharacteristics]
    for (w1 <- m2.phrase.toSeq.map(_.string.toLowerCase))
      for (w2 <- m1.phrase.toSeq.map(_.string.toLowerCase))
       if (w1.equals(w2) || m2c.wnSynsets.exists(m1c.wnHypernyms.contains) || m1c.wnHypernyms.exists(m2c.wnHypernyms.contains) ||
           lexicon.iesl.Country.contains(w1) && lexicon.iesl.Country.contains(w2) ||
           lexicon.iesl.City.contains(w1) && lexicon.iesl.City.contains(w2) ||
           lexicon.uscensus.PersonFirstMale.contains(w1) && lexicon.uscensus.PersonFirstMale.contains(w2) ||
           // commented out the femaleFirstNames part, Roth publication did not use
           lexicon.uscensus.PersonFirstFemale.contains(w1) && lexicon.uscensus.PersonFirstFemale.contains(w2) ||
           lexicon.uscensus.PersonLast.contains(w1) && lexicon.uscensus.PersonLast.contains(w2))
        set += getPairRelations(m1, m2)
    set.toSet
  }

  def countCompatibleMentionsBetween(m1:Mention, m2:Mention, mentions:Seq[Mention]): Seq[String] = {
    val doc = m1.phrase.document
    val ments = mentions.filter(s => s.phrase.start < m1.phrase.start && s.phrase.start > m2.phrase.end)
    val iter = ments.iterator
    var numMatches = 0
    while (numMatches <= 2 && iter.hasNext) {
      val m = iter.next()
      if (CorefFeatures.gendersMatch(m, m1) == True && CorefFeatures.numbersMatch(m, m1) == True) numMatches += 1
    }

    if (numMatches <= 2) (0 to numMatches).map(_.toString)
    else (0 to numMatches).map(_.toString) :+ "_OVER2"
  }

  val maleHonors = Set("mr", "mister")
  val femaleHonors = Set("ms", "mrs", "miss", "misses")
  val neuterWN = Set("artifact", "location", "group")

  val malePron = Set("he", "him", "his", "himself")
  val femalePron = Set("she", "her", "hers", "herself")
  val neuterPron = Set("it", "its", "itself", "this", "that", "anything", "something",  "everything", "nothing", "which", "what", "whatever", "whichever")
  val personPron = Set("you", "your", "yours", "i", "me", "my", "mine", "we", "our", "ours", "us", "myself", "ourselves", "themselves", "themself", "ourself", "oneself", "who", "whom", "whose", "whoever", "whomever", "anyone", "anybody", "someone", "somebody", "everyone", "everybody", "nobody")

  val allPronouns = maleHonors ++ femaleHonors ++ neuterWN ++ malePron ++ femalePron ++ neuterPron ++ personPron
  // TODO: this cache is not thread safe if we start making GenderMatch not local
  // val cache = scala.collection.mutable.Map[String, Char]()
  /*import cc.factorie.app.nlp.lexicon
  def namGender(m: Mention): Char = {
    val fullhead = m.phrase.string.trim.toLowerCase // TODO Is this change with "string" correct? -akm 2/28/2014
    var g = 'u'
    val words = fullhead.split("\\s")
    if (words.length == 0) return g

    val word0 = words.head
    val lastWord = words.last

    var firstName = ""
    var honor = ""
    if (lexicon.iesl.PersonHonorific.contains(word0)) {
      honor = word0
      honor = removePunct(honor)
      if (words.length >= 3)
        firstName = words(1)
    } else if (words.length >= 2) {
      firstName = word0
    } else {
      firstName = word0
    }

    // determine gender using honorifics
    if (maleHonors.contains(honor))
      return 'm'
    else if (femaleHonors.contains(honor))
      return 'f'

    // determine from first name
    if (lexicon.uscensus.PersonFirstMale.contains(firstName))
      g = 'm'
    else if (lexicon.uscensus.PersonFirstFemale.contains(firstName))
      g = 'f'
    else if (lexicon.uscensus.PersonLast.contains(lastWord))
      g = 'p'

    if (lexicon.iesl.City.contains(fullhead) || lexicon.iesl.Country.contains(fullhead)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    if (lexicon.iesl.OrgSuffix.contains(lastWord)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    g
  } */
  /*
  def nomGender(m: Mention, wn: WordNet): Char = {
    val fullhead = m.phrase.string.toLowerCase
    if (wn.isHypernymOf("male", fullhead))
      'm'
    else if (wn.isHypernymOf("female", fullhead))
      'f'
    else if (wn.isHypernymOf("person", fullhead))
      'p'
    else if (neuterWN.exists(wn.isHypernymOf(_, fullhead)))
      'n'
    else
      'u'
  }*/

  /*
  def proGender(m: Mention): Char = {
    val pronoun = m.phrase.string.toLowerCase
    if (malePron.contains(pronoun))
      'm'
    else if (femalePron.contains(pronoun))
      'f'
    else if (neuterPron.contains(pronoun))
      'n'
    else if (personPron.contains(pronoun))
      'p'
    else
      'u'
  }*/


  def strongerOf(g1: Int, g2: Int): Int = {
    if ((g1 == GenderDomain.MALE || g1 == GenderDomain.FEMALE) && (g2 == GenderDomain.PERSON || g2 == GenderDomain.UNKNOWN))
      g1
    else if ((g2 == GenderDomain.MALE || g2 == GenderDomain.FEMALE) && (g1 == GenderDomain.PERSON || g1 == GenderDomain.UNKNOWN))
      g2
    else if ((g1 == GenderDomain.NEUTER || g1 == GenderDomain.PERSON) && g2 == GenderDomain.UNKNOWN)
      g1
    else if ((g2 == GenderDomain.NEUTER || g2 == GenderDomain.PERSON) && g1 == GenderDomain.UNKNOWN)
      g2
    else
      g2
  }


  // TODO Do we really want to return a Char here?
  def gendersMatch(m1:Mention, m2:Mention): Ternary = {
    val g1 = m2.phrase.attr[Gender].intValue
    val g2 = m1.phrase.attr[Gender].intValue
    import GenderDomain._
    // TODO This condition could be much simplified 
    if (g1 == GenderDomain.UNKNOWN || g2 == GenderDomain.UNKNOWN)
      Unknown
    else if (g1 == GenderDomain.PERSON && (g2 == GenderDomain.MALE || g2 == GenderDomain.FEMALE || g2 == GenderDomain.PERSON))
      Unknown
    else if (g2 == GenderDomain.PERSON && (g1 == GenderDomain.MALE || g1 == GenderDomain.FEMALE || g1 == GenderDomain.PERSON))
      Unknown
    else if (g1 == g2)
      True
    else
      False
  }

  def headWordsCross(m1:Mention, m2:Mention, model: CorefModel): String = {
    val w1 = m2.attr[MentionCharacteristics].headPhraseTrim
    val w2 = m1.attr[MentionCharacteristics].headPhraseTrim
    val rare1 = 1.0 / model.CorefTokenFrequencies.lexicalCounter.headWordCounts.getOrElse(w1.toLowerCase, 1).toFloat > 0.1
    val rare2 = 1.0 / model.CorefTokenFrequencies.lexicalCounter.headWordCounts.getOrElse(w2.toLowerCase, 1).toFloat > 0.1
    if (rare1 && rare2 && w1.equalsIgnoreCase(w2))
      "Rare_Duplicate"
    else
      (if (rare1) "RARE" else w1) + "_AND_" + (if (rare2) "RARE" else w2)
  }

  val singPron = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluPron = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singDet = Set("a ", "an ", "this ")
  val pluDet = Set("those ", "these ", "some ")

  def numbersMatch(m1:Mention, m2:Mention): Ternary = {
    val n1 = m2.phrase.attr[Number].intValue
    val n2 = m1.phrase.attr[Number].intValue
    import NumberDomain._
    if (n1 == n2 && n1 != UNKNOWN)
      True
    else if (n1 != n2 && n1 != UNKNOWN && n2 != UNKNOWN)
      False
    else if (n1 == UNKNOWN || n2 == UNKNOWN) {
      if (m1.phrase.toSeq.map(t => t.string.trim).mkString(" ").equals(m2.phrase.toSeq.map(t => t.string.trim).mkString(" ")))
        True
      else
        Unknown
    }
    else
      Unknown
  }

  val relativizers = Set("who", "whom", "which", "whose", "whoever", "whomever", "whatever", "whichever", "that")

  def areAppositive(m1:Mention, m2:Mention): Boolean = {
    (m2.attr[MentionCharacteristics].isProper || m1.attr[MentionCharacteristics].isProper) &&
      (m2.phrase.last.next(2) == m1.phrase.head && m2.phrase.last.next.string.equals(",") ||
        m1.phrase.last.next(2) == m2.phrase.head && m1.phrase.last.next.string.equals(","))
  }

  def isRelativeFor(m1:Mention, m2:Mention) =
    relativizers.contains(m1.attr[MentionCharacteristics].lowerCaseHead) &&
      (m2.phrase.head == m1.phrase.last.next ||
        (m2.phrase.head == m1.phrase.last.next(2) && m1.phrase.last.next.string.equals(",")
          || m2.phrase.head == m1.phrase.last.next(2) && m1.phrase.last.next.string.equals(",")))


  def areRelative(m1:Mention, m2:Mention): Boolean = isRelativeFor(m1, m2) || isRelativeFor(m2, m1)

  def canBeAliases(m1:Mention, m2:Mention): Boolean = {
    val m1c = m1.attr[MentionCharacteristics]
    val m2c = m2.attr[MentionCharacteristics]
    val eType1 = m2c.predictEntityType
    val eType2 = m1c.predictEntityType

    val m1head = m2c.lowerCaseHead
    val m2head = m1c.lowerCaseHead
    val m1Words = m1.phrase.tokens.map(_.string)
    val m2Words = m2.phrase.tokens.map(_.string)

    if (m2c.isProper && m1c.isProper && m2c.predictEntityType.equals(m1c.predictEntityType) && (m2c.predictEntityType.equals(OntonotesEntityTypeDomain.PERSON) || m2c.predictEntityType.equals(OntonotesEntityTypeDomain.GPE)))
      return m2.phrase.last.string.toLowerCase equals m1.phrase.last.string.toLowerCase

    else if ((eType1.equals(OntonotesEntityTypeDomain.ORG) || eType1.equals(OntonotesEntityTypeDomain.O)) && (eType2.equals(OntonotesEntityTypeDomain.ORG) || eType2.equals(OntonotesEntityTypeDomain.O))) {
      val (initials, shorter) =
        if (m1Words.length < m2Words.length)
          (m2c.initials, m1head)
        else
          (m1c.initials, m2head)
      return shorter.replaceAll("[., ]", "") equalsIgnoreCase initials
    }

    false
  }


  lazy val punct = "^['\"(),;.`]*(.*?)['\"(),;.`]*$".r
  def removePunct(s: String): String = {
    val punct(ret) = s
    ret
  }

}
object PronounDictionary {
  val firstPersonPronouns = Set("i", "me", "myself", "mine", "my", "we", "us", "ourself", "ourselves", "ours", "our")
  val secondPersonPronouns = Set("you", "yourself", "yours", "your", "yourselves")
  val thirdPersonPronouns = Set("he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's", "they", "them", "themself", "themselves", "theirs", "their", "they", "them", "'em", "themselves")
  val otherPronouns = Set("who", "whom", "whose", "where", "when","which")

  val demonstratives = Set("this", "that", "these", "those")

  // Borrowed from Stanford
  val singularPronouns = Set("i", "me", "myself", "mine", "my", "yourself", "he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's")
  val pluralPronouns = Set("we", "us", "ourself", "ourselves", "ours", "our", "yourself", "yourselves", "they", "them", "themself", "themselves", "theirs", "their")
  val malePronouns = Set("he", "him", "himself", "his")
  val femalePronouns = Set("her", "hers", "herself", "she")
  val neutralPronouns = Set("it", "its", "itself", "where", "here", "there", "which")


  val allPronouns = firstPersonPronouns ++ secondPersonPronouns ++ thirdPersonPronouns ++ otherPronouns

  // Constructed based on Stanford's Dictionaries class
  val canonicalizations = new mutable.HashMap[String,String]()
  canonicalizations.put("i", "i")
  canonicalizations.put("me", "i")
  canonicalizations.put("my", "i")
  canonicalizations.put("myself", "i")
  canonicalizations.put("mine", "i")
  canonicalizations.put("you", "you")
  canonicalizations.put("your", "you")
  canonicalizations.put("yourself", "you")
  canonicalizations.put("yourselves", "you")
  canonicalizations.put("yours", "you")
  canonicalizations.put("he", "he")
  canonicalizations.put("him", "he")
  canonicalizations.put("his", "he")
  canonicalizations.put("himself", "he")
  canonicalizations.put("she", "she")
  canonicalizations.put("her", "she")
  canonicalizations.put("herself", "she")
  canonicalizations.put("hers", "she")

  canonicalizations.put("we", "we")
  canonicalizations.put("us", "we")
  canonicalizations.put("our", "we")
  canonicalizations.put("ourself", "we")
  canonicalizations.put("ourselves", "we")
  canonicalizations.put("ours", "we")
  canonicalizations.put("they", "they")
  canonicalizations.put("them", "they")
  canonicalizations.put("their", "they")
  canonicalizations.put("themself", "they")
  canonicalizations.put("themselves", "they")
  canonicalizations.put("theirs", "they")
  canonicalizations.put("'em", "they")
  canonicalizations.put("it", "it")
  canonicalizations.put("itself", "it")
  canonicalizations.put("its", "it")
  canonicalizations.put("one", "one")
  canonicalizations.put("oneself", "one")
  canonicalizations.put("one's", "one")

  canonicalizations.put("this", "this")
  canonicalizations.put("that", "that")
  canonicalizations.put("these", "these")
  canonicalizations.put("those", "those")
  canonicalizations.put("which", "which")
  canonicalizations.put("who", "who")
  canonicalizations.put("whom", "who")
  //  canonicalizations.put("where", "where")
  //  canonicalizations.put("whose", "whose")
  // This entry is here just to make results consistent with earlier ones
  // on our very small dev set
  canonicalizations.put("thy", "thy")
  canonicalizations.put("y'all", "you")
  canonicalizations.put("you're", "you")
  canonicalizations.put("you'll", "you")
  canonicalizations.put("'s", "'s")

  def isPronLc(str: String): Boolean = {
    allPronouns.contains(str.toLowerCase)
  }

  def isDemonstrative(str: String): Boolean = {
    demonstratives.contains(str.toLowerCase)
  }

  def canonicalize(str: String): String = {
    if (canonicalizations.contains(str.toLowerCase)) {
      canonicalizations.get(str.toLowerCase).get
    } else {
      ""
    }
  }

  def main(args: Array[String]) {
    //println(PronounDictionary.canonicalizations("'em"))
    println(PronounDictionary.isPronLc("them"))
    println(PronounDictionary.isPronLc("Them"))
    println(PronounDictionary.isPronLc("NotThem"))
  }
}

