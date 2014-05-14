/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

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
  lazy val lowerCaseHead: String = mention.phrase.headToken.string.toLowerCase
  lazy val lowerCaseString:String =  mention.phrase.string.toLowerCase
  lazy val headPhraseTrim: String = mention.phrase.tokensString(" ").trim
  lazy val nonDeterminerWords: Seq[String] =
    mention.phrase.tokens.filterNot(_.posTag.categoryValue == "DT").map(t => t.string.toLowerCase)
  lazy val initials: String =
      mention.phrase.tokens.map(_.string).filterNot(lexicon.iesl.OrgSuffix.contains).filter(t => t(0).isUpper).map(_(0)).mkString("")
  lazy val predictEntityType: Int = mention.phrase.attr[OntonotesPhraseEntityType].intValue
  lazy val demonym: String = lexicon.iesl.DemonymMap.getOrElse(headPhraseTrim, "")

  lazy val capitalization: Char = {
      if (mention.phrase.length == 1 && mention.phrase.head.positionInSentence == 0) 'u' // mention is the first word in sentence
      else {
        val s = mention.phrase.value.filter(_.posTag.categoryValue.startsWith("N")).map(_.string.trim) // TODO Fix this slow String operation
        if (s.forall(_.forall(_.isUpper))) 'a'
        else if (s.forall(t => t.head.isLetter && t.head.isUpper)) 't'
        else 'f'
      }
    }
  lazy val gender = mention.phrase.attr[Gender].categoryValue
  lazy val number = mention.phrase.attr[Number].categoryValue
  lazy val nounPhraseType = mention.phrase.attr[NounPhraseType].categoryValue
  lazy val genderIndex = mention.phrase.attr[Gender].intValue
  lazy val numberIndex = mention.phrase.attr[Number].intValue
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

  lazy val canonicalizedPronounOrType =
    if (isPRO) PronounSets.canonicalForms.getOrElse(lowerCaseString,lowerCaseHead)
    else nounPhraseType
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

  def proWordHead(mention1: Mention,mention2: Mention): String = {
    val m1c = mention1.attr[MentionCharacteristics]
    val m2c = mention2.attr[MentionCharacteristics]
    val e1 = if (m2c.isPRO) mention2.phrase.headToken.string else m2c.predictEntityType
    val e2 = if (m1c.isPRO) mention1.phrase.headToken.string else m1c.predictEntityType
    e1 + "&&" + e2
  }

  def entityTypeMatch(mention1: Mention,mention2: Mention): Ternary = {
    val m1c = mention1.attr[MentionCharacteristics]
    val m2c = mention2.attr[MentionCharacteristics]
    if (m2c.predictEntityType == OntonotesEntityTypeDomain.O || m1c.predictEntityType == OntonotesEntityTypeDomain.O) Unknown
    else if (m2c.predictEntityType == m1c.predictEntityType) True
    else False
  }

  def acronymMatch(mention1: Mention,mention2: Mention):  Ternary = {
    val m1 = mention1.attr[MentionCharacteristics]
    val m2 = mention2.attr[MentionCharacteristics]
    if (mention1.phrase.length == 1 && mention2.phrase.length > 1) {
      if (m2.acronym.contains(mention1.phrase.string.trim.toLowerCase)) True else False
    } else if (mention1.phrase.length > 1 && mention2.phrase.length == 1) {
      if (m1.acronym.contains(mention2.phrase.string.trim.toLowerCase)) True else False
    } else Unknown
  }

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

  def gendersMatch(m1:Mention, m2:Mention): Ternary = {
    val g1 = m2.phrase.attr[Gender].intValue
    val g2 = m1.phrase.attr[Gender].intValue
    // TODO This condition could be simplified
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
    val rare1 = 1.0 / model.CorefTokenFrequencies.counter.headWords.getOrElse(w1.toLowerCase, 1).toFloat > 0.1
    val rare2 = 1.0 / model.CorefTokenFrequencies.counter.headWords.getOrElse(w2.toLowerCase, 1).toFloat > 0.1
    if (rare1 && rare2 && w1.equalsIgnoreCase(w2))
      "Rare_Duplicate"
    else
      (if (rare1) m1.attr[MentionCharacteristics].headPos else w1) + "_AND_" + (if (rare2) m1.attr[MentionCharacteristics].headPos else w2)
  }

  val singDet = Set("a ", "an ", "this ")
  val pluDet = Set("those ", "these ", "some ")

  def numbersMatch(m1:Mention, m2:Mention): Ternary = {
    val n1 = m2.phrase.attr[Number].intValue
    val n2 = m1.phrase.attr[Number].intValue
    import NumberDomain._
    if (n1 == n2 && n1 != UNKNOWN) True
    else if (n1 != n2 && n1 != UNKNOWN && n2 != UNKNOWN) False
    else if (n1 == UNKNOWN || n2 == UNKNOWN) {
      if (m1.phrase.toSeq.map(t => t.string.trim).mkString(" ").equals(m2.phrase.toSeq.map(t => t.string.trim).mkString(" ")))
        True
      else Unknown
    }
    else Unknown
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


  def areRelative(m1: Mention, m2: Mention): Boolean = isRelativeFor(m1, m2) || isRelativeFor(m2, m1)

  def canBeAliases(m1: Mention, m2: Mention): Boolean = {
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

object PronounSets {
  val firstPerson = Set("i", "me", "myself", "mine", "my", "we", "us", "ourself", "ourselves", "ours", "our")
  val secondPerson = Set("you", "yourself", "yours", "your", "yourselves")
  val thirdPerson = Set("he", "him", "himself", "his", "she", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's", "they", "them", "themself", "themselves", "theirs", "their",  "'em")
  val other = Set("who", "whom", "whose", "where", "when","which")

  val demonstrative = Set("this", "that", "these", "those")

  val singular = Set("i", "me", "myself", "mine", "my", "yourself", "he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's")
  val plural = Set("we", "us", "ourself", "ourselves", "ours", "our", "yourself", "yourselves", "they", "them", "themself", "themselves", "theirs", "their")
  val male = Set("he", "him", "himself", "his")
  val female = Set("her", "hers", "herself", "she")

  val neuter = Set("it", "its", "itself", "this", "that", "anything", "something",  "everything", "nothing", "which", "what", "whatever", "whichever")
  val personal = Set("you", "your", "yours", "i", "me", "my", "mine", "we", "our", "ours", "us", "myself", "ourselves", "themselves", "themself", "ourself", "oneself", "who", "whom", "whose", "whoever", "whomever", "anyone", "anybody", "someone", "somebody", "everyone", "everybody", "nobody")

  val allPronouns = firstPerson ++ secondPerson ++ thirdPerson ++ other

  val canonicalForms = new mutable.HashMap[String,String](){
    ("i", "i")
    ("i", "i")
    ("me", "i")
    ("my", "i")
    ("myself", "i")
    ("mine", "i")
    ("you", "you")
    ("your", "you")
    ("yourself", "you")
    ("yourselves", "you")
    ("yours", "you")
    ("he", "he")
    ("him", "he")
    ("his", "he")
    ("himself", "he")
    ("she", "she")
    ("her", "she")
    ("herself", "she")
    ("hers", "she")
    ("we", "we")
    ("us", "we")
    ("our", "we")
    ("ourself", "we")
    ("ourselves", "we")
    ("ours", "we")
    ("they", "they")
    ("them", "they")
    ("their", "they")
    ("themself", "they")
    ("themselves", "they")
    ("theirs", "they")
    ("'em", "they")
    ("it", "it")
    ("itself", "it")
    ("its", "it")
    ("one", "one")
    ("oneself", "one")
    ("one's", "one")
    ("this", "this")
    ("that", "that")
    ("these", "these")
    ("those", "those")
    ("which", "which")
    ("who", "who")
    ("whom", "who")
    ("thy", "thy")
    ("y'all", "you")
    ("you're", "you")
    ("you'll", "you")
    ("'s", "'s")
  }
}

