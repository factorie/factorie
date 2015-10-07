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

import java.net.URL
import java.nio.file.Path

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.ner._
import scala.collection.mutable.{HashMap, HashSet, ArrayBuffer}
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, Lexicon, StaticLexicons, StopWords}
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.StringOps
import cc.factorie.app.nlp.wordnet._
import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import cc.factorie.util.{Trackable, HyperparameterMain, TimingCollector, Trackers}
import cc.factorie.app.nlp.load.{MentionSpeaker, LoadConll2011}
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import java.util.concurrent.ExecutorService
import cc.factorie.optimize._
import java.io._
import cc.factorie.util._
import cc.factorie.variable.{EnumDomain, CategoricalVariable}

import scala.io.Source


/**
 * This file contains an implementation of the Deterministic Within-Document Coreference System described
 * in the papers:
 *
 *    Heeyoung Lee, Yves Peirsman, Angel Chang, Nathanael Chambers, Mihai Surdeanu, Dan Jurafsky.
 *    Stanford's Multi-Pass Sieve Coreference Resolution System at the CoNLL-2011 Shared Task.
 *    In Proceedings of the CoNLL-2011 Shared Task, 2011.
 *
 *    Karthik Raghunathan, Heeyoung Lee, Sudarshan Rangarajan, Nathanael Chambers, Mihai Surdeanu, Dan Jurafsky, Christopher Manning
 *    A Multi-Pass Sieve for Coreference Resolution
 *    EMNLP-2010, Boston, USA. 2010.
 *
 * There are a few differences in this implementation and the system described in the above papers.
 * Most significantly, there is no discourse processing in this implementation. Experiments were performed using the
 * "gold" speaker annotations and the discourse processing sieve. These experiments revealed that the discourse processing
 * only increased the F1 score of this system a small amount.Additionally, this system does not make use of all of the
 * external  resources (Freebase, etc) that the system described in the papers uses.
 *
 * Other differences include a more restrictive condition on personal pronoun agreement in the Pronoun Sieve and a loosening
 * of the NER agreement constraint in the Relaxed Head Matching Sieve.
 *
 * The performance of this deterministic coreference system is not as good as Stanford's implementation. After spending
 * quite a bit of time performing error analysis, it seems that the errors made by this system stem from parses, NER labels,
 * and head word identifications that our system computes differently than Stanford's system. These differences are sometimes
 * not mistakes, but cause deterministic decisions to be made incorrectly. Many of the errors seen were mistakes
 * identifying the head word in a phrase, worsening the performance of the Strict Head Matching Sieve.
 *
 * The performance results on the Conll 2011 test set are shown below. The results presented below used mentions predicted
 * by Stanford's implementation as input to this system.
 * The below results for Stanford's system are from http://conll.cemantix.org/download/re-scoring-conll-evaluations.v16.xlsx
 *
 *     Metric | System   |   Recall   |  Precision  |    F1
 *     -------|----------|------------|-------------|----------
 *     MUC    | factorie |   60.54%   |   56.11%    |   58.24%
 *     MUC    | Stanford |    ---     |     ---     |   59.57%
 *     B3     | factorie |   45.68%   |   49.97%    |   47.73%
 *     B3     | Stanford |    ---     |     ---     |   48.93%
 *     ceafm  | factorie |   53.79%   |   46.85%    |   50.08%
 *     ceafm  | Stanford |    ---     |     ---     |   53.04%
 *     ceafe  | factorie |   53.54%   |   39.21%    |   46.27%
 *     ceafe  | Stanford |    ---     |     ---     |   46.11%
 *     blanc  | factorie |   34.66%   |   51.65%    |   41.49%
 *     blanc  | Stanford |    ---     |     ---     |   48.84%
 *
 *
 * Next steps would be to continue to implement the Discourse Processing Sieve as well as to continue to investigate
 * where errors in the system stem from or perhaps make the system more robust to slight variations in things such as NER labellings, etc.
 *
 * Also, note that there is a debug infrastructure in place for this system, it is defined by the trait DeterministicCorefDebug
 * It generates a folder of HTML files for each document processed by the system. There is an HTML file for each Sieve of the
 * system, which contains a clear representation of all of the decisions made in that sieve as well as the mention cluster assignments
 * after the sieve is completed. The debug infrastructure can be used by passing the name of an output directory as an argument
 * into the sieves.
 *
 * @author Nicholas Monath
 */


/**
 * Domain for labeling documents with "types" such as Article or Conversation, for the
 * sake of a coreference system. 
 */
object CorefDocumentTypeDomain extends EnumDomain {
  val UNKNOWN,     // uncertain
  ARTICLE,        // Article, newswire etc
  CONVERSATION = Value   // conversational Text
  freeze()
}

/**
 * Variable for the CorefDocumentTypeDomain
 *
 */
class CorefDocumentType extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = CorefDocumentTypeDomain
}



/**
 * A collection of fields used in the deterministic coreference system.
 * @param mention
 */
class DeterministicCorefCache(val mention: Mention) {
  lazy val mentionSentence: Sentence = mention.phrase.sentence
  lazy val relaxedMentionString: String = CorefUtil.relaxString(mention)
  lazy val absoluteSentenceNumber: Int = CorefUtil.absoluteSentenceNumber(mention)
  lazy val isIndefiniteArticleOrPronoun = (CorefUtil.indefiniteArticles.contains(mention.phrase.tokens(0).string.toLowerCase.trim) || CorefUtil.indefinitePronouns.contains(mention.phrase.tokens(0).string.toLowerCase.trim))
}


/**
 * A domain representing the animacy of tokens & mentions
 */
object DCorefAnimacyDomain extends EnumDomain {
  val UNKNOWN,     // uncertain
  ANIMATE,        // Animate Object
  INANIMATE = Value   // Inanimate object
  freeze()
}

/**
 * A variable storing an Animacy value
 */
class DCorefAnimacy extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = DCorefAnimacyDomain
}

/**
 * The extension of the animacy categorical variable to phrases
 * @param phrase
 * @param value
 */
class DCorefPhraseAnimacy(val phrase:Phrase, value:Int) extends DCorefAnimacy(value) {
  def this(phrase:Phrase, value:String) = this(phrase, DCorefAnimacyDomain.index(value))
}

/**
 * A domain representing the person (e.g. 1st, 2nd, 3rd) of a mention
 */
object DCorefPersonDomain extends EnumDomain {
  val UNKNOWN,     // uncertain
  FIRST,        // 1st person, I, me, us, etc ...
  SECOND,      // 2nd person, you, your, etc ...
  THIRD = Value   // 3rd person, he, she, it, the car, the dog, etc ...
  freeze()
}

/**
 * A variable storing a Person value
 */
class DCorefPerson extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = DCorefPersonDomain
}

/**
 * The extension of the person categorical variable to phrases
 * @param phrase
 * @param value
 */
class DCorefPhrasePerson(val phrase:Phrase, value:Int) extends DCorefPerson(value) {
  def this(phrase:Phrase, value:String) = this(phrase, DCorefPersonDomain.index(value))
}



/**
 * CorefUtil is a class which provides several methods which are used in the various 
 * deterministic coreference sieves.
 */

object CorefUtil extends CorefUtil {}


class CorefUtil {


  /*
   * Collections of words
   */

  val locationModifiers: Set[String] = Set[String]("east", "west", "north", "south", "eastern", "western", "northern", "southern", "upper", "lower", "northeastern", "northwestern", "southeastern", "southwestern")
  val indefiniteArticles: HashSet[String] = HashSet[String]("a", "an")
  val indefinitePronouns: HashSet[String] = HashSet[String]("another", "anybody", "anyone", "anything", "each", "either", "enough", "everything", "less", "little", "much", "neither", "nobody", "no-one", "nothing", "one", "other", "somebody", "someone", "something", "both", "few", "fewer", "many", "others", "several", "all", "any", "more", "most", "none", "some", "such")
  val stopWordsList: HashSet[String] = HashSet[String]("the", "that", "this", "mr.", "miss", "mrs.", "dr.", "ms.", "inc.", "ltd.", "corp.", "'s")
  val firstPersonPronouns: HashSet[String] = HashSet[String]("i", "we", "me", "us", "'s", "my", "myself", "mine", "our", "ours", "ourself", "ourselves")
  val secondPersonPronouns: HashSet[String] = HashSet[String]("you", "your", "yours","yourself", "yourselves")
  val thirdPersonPersonalPronouns: HashSet[String] = HashSet[String]("he", "his", "him", "himself", "she", "her", "hers", "herself", "they", "their", "theirs", "them", "themself", "themselves","'em")
  val personalPronouns: HashSet[String] = firstPersonPronouns ++ secondPersonPronouns ++ thirdPersonPersonalPronouns
  val animatePronouns: HashSet[String] = personalPronouns ++  HashSet[String]("one", "oneself", "one's","who", "whom", "whose")
  val reflexivePronouns: HashSet[String] =  HashSet[String]("myself", "ourself", "ourselves", "yourself", "yourselves", "himself", "herself", "itself", "themself", "themselves")

  /**
   * incompatibileCache is a HashSet that records which mentions are incompatible.
   * If mentionA is incompatible with mentionB, the concatenation of mentionA.uniqueId and mentionB.uniqueId
   * will be present in the cache. The cache does have a size limit and once filled, no new entries will be 
   * placed in the cache. This limit is controlled by the variable incompatibleCacheMaxSize, which defines 
   * the number of records stored in the cache.
   */
  private val incompatibleCache: HashSet[String] = new HashSet[String]()

  /**
   * The maximum number of items which will be stored in the incompatibility cache. 
   * Set to -1 for no limit.
   */
  val incompatibleCacheMaxSize: Int = 20000

  /**
   * Analogous to the incompatibleCache, but for mentions which are compatible with one another.
   */
  private val compatibleCache: HashSet[String] = new HashSet[String]()

  /**
   * The maximum number of items which will be stored in the compatible cache.
   * Set to -1 for no limit.
   */
  val compatibleCacheMaxSize: Int = 20000


  /**
   * Similar to incompatibileCache, invalidPronounDistanceCache, stores which pronominal mentions
   * are too far from other mentions for resolution. It stores this as a HashSet of strings of the 
   * form: mentionA.uniqueId concatenated with mentionB.uniqueId, where mentionA is a pronominal reference
   * and mentionB is a reference that is too many sentences away in the document. The cache does have a size limit
   * and once filled, no new entries will be  placed in the cache. This limit is controlled by the variable
   * invalidPronounDistanceCacheMaxSize, which defines the number of records stored in the cache.
   */
  private val invalidPronounDistanceCache: HashSet[String] = new HashSet[String]()

  /**
   * The maximum number of items which will be stored in invalidPronounDistanceCache.
   * Set to -1 for no limit.
   */
  val invalidPronounDistanceCacheMaxSize: Int = 20000


  /**
   * Analogous to the invalid pronoun distance cache, but for mentions which satisfy the distance requirement
   */
  private val validPronounDistanceCache: HashSet[String] = new HashSet[String]()

  /**
   * The maximum number of items which will be stored in validPronounDistanceCache.
   * Set to -1 for no limit.
   */
  val validPronounDistanceCacheMaxSize: Int = 20000


  /**
   * The current mention is considered incompatible with the candidate antecedent if ANY of the following conditions hold:
   * - One mention is a substring of the other mention
   * - The mentions are in an "i-within-i" relationship
   * - if the current mention is "this" and the candidate antecedent is farther than three sentences away
   * - if the candidate antecedent is a second person pronoun
   * - if the current mention is a bare plural (a plural common noun without a determiner or modifier)
   * - if the mentions are in a subject object relationship
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def incompatible(currentMention: Mention, candidateAntecedent: Mention): Boolean = {

    // The string which is used to do a lookup in the cache
    val cacheString: String = currentMention.uniqueId ++ candidateAntecedent.uniqueId

    // check cached values
    if (incompatibleCache.contains(cacheString)) {
      return true
     } else if (compatibleCache.contains(cacheString)) {
       return false
    } else {

      // Check each of the compatibility criteria
      val cmHeadString = currentMention.phrase.headToken.string.trim.toLowerCase
      val caHeadString = candidateAntecedent.phrase.headToken.string.trim.toLowerCase


      if (liesWithin(currentMention, candidateAntecedent) ||
        liesWithin(candidateAntecedent, currentMention) || // either mention lies within the other.
        (isIWithinI(currentMention, candidateAntecedent) && !currentMention.phrase.isAppositionOf(candidateAntecedent.phrase) && !CorefFeatures.isRelativeFor(currentMention, candidateAntecedent)) ||
        (isIWithinI(candidateAntecedent, currentMention)  && !candidateAntecedent.phrase.isAppositionOf(currentMention.phrase) && !CorefFeatures.isRelativeFor(candidateAntecedent, currentMention))|| // either mention is in an i-within-i relationship with other and neither is in an appositive or relative pronoun construction
        isBarePlural(currentMention) || // mention is bare plural
        ( (!reflexivePronouns.contains(cmHeadString) && !reflexivePronouns.contains(caHeadString)) && hasSubjectObjectRelationship(currentMention, candidateAntecedent))) {  // mentions are in a subject-object relationship and neither is a reflexive pronoun

        if (incompatibleCache.size < incompatibleCacheMaxSize || incompatibleCacheMaxSize == -1) {
          incompatibleCache.add(cacheString)
        }
        return true
      } else {
         if (compatibleCache.size < compatibleCacheMaxSize || compatibleCacheMaxSize == -1) {
           compatibleCache.add(cacheString)
         }
        return false
      }
    }
  }

  /**
   * Returns true if the current mention is a first or second person pronoun and the number of sentences separating the two mentions is more than 3
   * or if the currentMention is "this" and the sentence distance is more than three.
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def incompatiblePronounMatch(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val cacheString: String = currentMention.uniqueId ++ candidateAntecedent.uniqueId
    if (invalidPronounDistanceCache.contains(cacheString)) {
      return true
     } else if (validPronounDistanceCache.contains(cacheString)) {
       return false
    } else {
      val cmHeadString = currentMention.phrase.headToken.string.trim.toLowerCase
      if ((firstPersonPronouns.contains(cmHeadString) || secondPersonPronouns.contains(cmHeadString) ||  cmHeadString.equals("this")) && sentenceDistance(currentMention, candidateAntecedent) > 3) {
        if (invalidPronounDistanceCache.size < invalidPronounDistanceCacheMaxSize || invalidPronounDistanceCacheMaxSize == - 1) {
          invalidPronounDistanceCache.add(cacheString)
        }
        return true
      } else {
         if (validPronounDistanceCache.size < validPronounDistanceCacheMaxSize || validPronounDistanceCacheMaxSize == - 1) {
           validPronounDistanceCache.add(cacheString)
         }
        return false
      }
    }
  }


  /**
   * Returns true if a mention in cluster1 is incompatible with some mention in cluster2
   * @param cluster1
   * @param cluster2
   * @return
   */
  def incompatible(cluster1: MentionCluster, cluster2: MentionCluster): Boolean = {
    for (m1 <- cluster1.mentions) {
      for (m2 <- cluster2.mentions) {
        if (incompatible(m1,m2)) {

          // Make all of the mentions in two clusters incompatible
          for (m3 <- cluster1.mentions) {
            for (m4 <- cluster2.mentions) {
              val cacheString: String = m3.uniqueId ++ m4.uniqueId
               if (compatibleCache.contains(cacheString)) {
                 compatibleCache.remove(cacheString)
               }
              if (incompatibleCache.size < incompatibleCacheMaxSize || incompatibleCacheMaxSize == -1) {
                incompatibleCache.add(cacheString)
              }
            }
          }

          return true
        }
      }
    }
    return false
  }

  /**
   * Given a document, remove any mention from the internal within-doc coref structure
   * if that mention lies within a larger mention.
   *
   * For example, if mention A has a span of tokens 30-36, mention B has a span of tokens 31-33, and mention C has
   * a span of tokens 30-34. This function will remove mentions B and C.
   * @param document
   * @return
   */
  def removeLiesWithinMentions(document: Document): Unit = {
    for (m1 <- document.getCoref.mentions) {
      for (m2 <- document.getCoref.mentions) {
        if (m1.uniqueId != m2.uniqueId && liesWithin(m1,m2)) {
          if ( (m1.phrase.end - m1.phrase.start) > (m2.phrase.end - m2.phrase.start)) {
            document.getCoref.deleteMention(m2)
          } else {
            document.getCoref.deleteMention(m1)
          }
        }
      }
    }
  }

  /**
   * Given a document, remove any mention from the internal within-doc coref structure
   * if that mention lies within a larger mention and both mentions have the same head word.
   * @param document
   * @return
   */
  def removeLiesWithinMentionsWithSameHead(document: Document): Unit = {
    for (m1 <- document.getCoref.mentions) {
      for (m2 <- document.getCoref.mentions) {
        if (m1.uniqueId != m2.uniqueId && liesWithinWithSameHeadWord(m1,m2)) {
          if ( (m1.phrase.end - m1.phrase.start) > (m2.phrase.end - m2.phrase.start)) {
            document.getCoref.deleteMention(m2)
          } else {
            document.getCoref.deleteMention(m1)
          }
        }
      }
    }
  }

  /**
   * Returns true if the span of m1 is contained by m2.
   * @param m1
   * @param m2
   * @return
   */
  def liesWithin(m1: Mention, m2: Mention): Boolean = {
    inTheSameSentence(m1, m2) && m1.phrase.start >= m2.phrase.start && m1.phrase.end <= m2.phrase.end
  }


  /**
   * Returns true if the span of m1 is contained by m2 and the mentions have the same head words
   * @param m1
   * @param m2
   * @return
   */
  def liesWithinWithSameHeadWord(m1: Mention, m2: Mention): Boolean = {
    inTheSameSentence(m1, m2) && m1.phrase.start >= m2.phrase.start && m1.phrase.end <= m2.phrase.end && m1.phrase.headToken == m2.phrase.headToken
  }


  /**
   * Returns true if m1 is a descendant of m2 in the parse tree
   */
  def isIWithinI(m1: Mention, m2: Mention): Boolean = {
    var curr: Token = m1.phrase.headToken
    val m2head: Token = m2.phrase.headToken
    var i_within_i: Boolean = false
    if (inTheSameSentence(m1,m2)) {
      while (curr != null && !i_within_i) {
        i_within_i = curr.positionInSentence == m2head.positionInSentence
        curr = curr.parseParent
      }
    }
    i_within_i
  }



  /**
   * Returns the number of sentences (absolute value) between the two mentions
   * @param m1
   * @param m2
   * @return
   */
  def sentenceDistance(m1: Mention, m2: Mention): Int = {
    if (m1.attr.contains(classOf[DeterministicCorefCache]) && m1.attr.contains(classOf[DeterministicCorefCache]))
      return math.abs(m1.attr[DeterministicCorefCache].absoluteSentenceNumber - m2.attr[DeterministicCorefCache].absoluteSentenceNumber)
    else
      return math.abs(absoluteSentenceNumber(m1) - absoluteSentenceNumber(m2))
  }


  /**
   * Returns the sentence number of the mention within the document. Note that this is the number of sentences which
   * appear before the sentence in which the mention occurs in the document. This differs from the field of the sentence
   * object, indexInSection, which returns the sentence's position in its section.
   * @param mention
   * @return
   */
  def absoluteSentenceNumber(mention: Mention):Int = {
      val mSection: Section = mention.phrase.section
      val mSectionIdx: Int = mSection.indexInDocument
      var result: Int = -1
      if (mSectionIdx == 0) {
        result = mention.phrase.sentence.indexInSection
      } else {
        var numSentenceInSectionsBeforeM: Int = mention.phrase.sentence.indexInSection
        val allSections: Seq[Section] = mSection.document.sections
        for (i <- 0 to mSectionIdx) {
          val numSentences: Int = allSections(i).sentences.length
          numSentenceInSectionsBeforeM += numSentences
        }
        result = mention.phrase.sentence.indexInSection + numSentenceInSectionsBeforeM

      }
      return result
  }


  /**
   * Returns true iff m1 and m2 appear in the same sentence
   * @param m1
   * @param m2
   * @return
   */
  def inTheSameSentence(m1: Mention, m2: Mention): Boolean = {
    return 0 == sentenceDistance(m1,m2)
  }


  /**
   * Returns true if the mention's head token has a parse label of subject (i.e. either csubj, nsubj, csubjpass, or nsubjpass)
   * @param m
   * @return
   */
  def isSubject(m: Mention): Boolean = {
    val mentionParseLabel: Int = m.phrase.headToken.parseLabel.intValue
    return (mentionParseLabel == ParseTreeLabelDomain.csubj || mentionParseLabel == ParseTreeLabelDomain.csubjpass
      || mentionParseLabel == ParseTreeLabelDomain.nsubj || mentionParseLabel == ParseTreeLabelDomain.nsubjpass)
  }

  /**
   * Returns true if mention's head token has a parse label of object (i.e. either dobj or iobj)
   * @param m
   * @return
   */
  def isObject(m: Mention): Boolean = {
    val mentionParseLabel: Int = m.phrase.headToken.parseLabel.intValue
    return (mentionParseLabel == ParseTreeLabelDomain.dobj || mentionParseLabel == ParseTreeLabelDomain.iobj) // One could consider including pobj here, I found results were better without it
  }



  /**
   * Returns true if one of the mentions is the subject of a particular verb and the other mention an object of the same verb
   * @param m1
   * @param m2
   * @return
   */
  def hasSubjectObjectRelationship(m1: Mention, m2: Mention): Boolean = {
    if (inTheSameSentence(m1, m2)) {
      val m1HeadToken: Token = m1.phrase.headToken
      val m2HeadToken: Token = m2.phrase.headToken
      if ((isSubject(m1) && isObject(m2)) || (isSubject(m2) && isObject(m1))) {
        // find the verb of each and make sure that they are the same:
        var tmp: Token = m1HeadToken
        var m1HeadVerb: Token = null
        var found: Boolean = false
        while (tmp != null && !found) {
          if (tmp.attr[PennPosTag].isVerb) {
            m1HeadVerb = tmp
            found = true
          } else {
            tmp = tmp.parseParent
          }
        }
        tmp = m2HeadToken
        var m2HeadVerb: Token = null
        found = false
        while (tmp != null && !found) {
          if (tmp.attr[PennPosTag].isVerb) {
            m2HeadVerb = tmp
            found = true
          } else {
            tmp = tmp.parseParent
          }
        }
        if (m1HeadVerb == m2HeadVerb) {
          return true
        }
      }
    }
    return false
  }


  /**
   * Returns DCorefPersonDomain.FIRST if the mention is a first person pronoun, DCorefPersonDomain.SECOND if the mention is a second person pronoun
   * else returns DCorefPersonDomain.THIRD. Note that "I"s and "You"s appearing in quotation marks are considered third person
   * @param mention
   * @return
   */
  def getPerson(mention: Mention): Int = {
    // if pronoun is "I" & "you" or relative return false
    val currentMentionHead: Token = mention.phrase.headToken
    val currentMentionHeadString: String =  currentMentionHead.lemmaString.trim.toLowerCase
    if (CorefFeatures.relativizers.contains(currentMentionHeadString)) {
      return DCorefPersonDomain.THIRD
    }
    var firstQuote: Boolean = true
    var firstQuoteIdx: Int = -1
    if (firstPersonPronouns.contains(currentMentionHeadString) || secondPersonPronouns.contains(currentMentionHeadString)) {
      val currentMentionSentenceTokens: Seq[Token] = mention.phrase.sentence.tokens
      for (token <- currentMentionSentenceTokens) {
        if (token.string.trim == "\"" || token.string.trim == "``"|| token.string.trim == "''") {
          if (firstQuote) {
            firstQuoteIdx = token.positionInSection
            firstQuote = false
          } else {
            if (token.positionInSection > currentMentionHead.positionInSection && firstQuoteIdx < currentMentionHead.positionInSection) {
              return DCorefPersonDomain.THIRD
            }
            firstQuote = true
          }
        }
      }
      if (firstPersonPronouns.contains(currentMentionHeadString))
        return DCorefPersonDomain.FIRST
      else
        return DCorefPersonDomain.SECOND
    } else {
      return DCorefPersonDomain.THIRD
    }
  }

  /**
   * Returns true if the two mentions to not have differing modifiers that fall into the set of "location modifiers", which are
   * words such as "north" or "southeast"
   * @param m1
   * @param m2
   * @return
   */
  def agreesInLocation(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val currentMentionModifiers: Set[String] = currentMention.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].isNoun || x.attr[PennPosTag].isAdjective) && x != (currentMention.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    val candidateAntecedentModifiers: Set[String] = candidateAntecedent.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].isNoun || x.attr[PennPosTag].isAdjective) && x != (candidateAntecedent.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    if (currentMentionModifiers.intersect(locationModifiers) != candidateAntecedentModifiers.intersect(locationModifiers)) {
      return false
    }
    return true

  }

  /**
   * Returns true if every modifier of the current mention is present in the antecedent. Additionally, any modifier of either mention
   * which is in the set of location modifiers (words such as "north or southwest") must be present in both mentions. Note how
   * this definition is not symmetric; the antecedent's modifiers (except for the location modifiers) need not be in mention.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  def hasCompatibleModifiersOnly(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    // Check that current mention does not have additional modifier that antecedent doesn't have
    // Check that the location modifiers are consistent
    val currentMentionModifiers: Set[String] = currentMention.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].isNoun || x.attr[PennPosTag].isAdjective || x.attr[PennPosTag].categoryValue == "CD") && x != (currentMention.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    val candidateAntecedentModifiers: Set[String] = candidateAntecedent.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].isNoun || x.attr[PennPosTag].isAdjective || x.attr[PennPosTag].categoryValue == "CD") && x != (candidateAntecedent.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    if (currentMentionModifiers.diff(candidateAntecedentModifiers).size != 0) {
      return false
    }
    if (currentMentionModifiers.intersect(locationModifiers) != candidateAntecedentModifiers.intersect(locationModifiers)) {
      return false
    }
    return true

  }

  /**
   * Returns true if every mention in mentionCluster1 has compatible modifiers with every mention in mentionCluster2
   * @param mentionCluster1
   * @param mentionCluster2
   * @param cm
   * @return
   */
  def hasCompatibleModifiersOnly(mentionCluster1: MentionCluster, mentionCluster2: MentionCluster, cm: MentionClusterManager): Boolean = {
    for (m1 <- mentionCluster1.mentions) {
      for (m2 <- mentionCluster2.mentions) {
        if (!hasCompatibleModifiersOnly(m1, m2, cm)) {
          return false
        }
      }
    }
    return true
  }


  /**
   * Returns true if the mention is a plural common noun without any modifiers or determiners
   * @param currentMention
   * @return
   */
  def isBarePlural(currentMention: Mention): Boolean = {
    val currentMentionHead: Token = currentMention.phrase.headToken
    val currentMentionHeadPos: PennPosTag = currentMentionHead.attr[PennPosTag]
    return (currentMentionHeadPos.categoryValue == "NNS" && currentMention.phrase.tokens.length == 1)
  }


  /**
   * Returns true if all the modifiers in a given phrase which are labeled "CD" in the parse
   * agree with those in other mention
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def agreementBetweenModifiersWhichAreNumbers(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val currentMentionModifiers: Set[String] = currentMention.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].categoryValue == "CD") && x != (currentMention.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    val candidateAntecedentModifiers: Set[String] = candidateAntecedent.phrase.tokens.toSeq.filter(x => (x.attr[PennPosTag].categoryValue == "CD") && x != (candidateAntecedent.phrase.headToken)).map(_.string.trim.toLowerCase).toSet
    if (currentMentionModifiers.diff(candidateAntecedentModifiers).size != 0) {
      return false
    }
    return true
  }


  /**
   * Ranks m1 and m2. The ranking is based on the following criteria:
   *  1. Proper nouns are more representative than common nouns, and common nouns more representative than pronouns
   *  2. If they are both proper, common or pronoun, the following attributes of the mentions are used for ranking
   *    - Distance of head to start of phrase (larger distance better)
   *    - Section of mention (lower index is better)
   *    - Sentence position in section (lower index is better)
   *    - Head position in Sentence (earlier is better)
   *    - Length of mention (if len < 5, shorter is better, else longer is better)
   * @param m1
   * @param m2
   * @return The more representative of the two mentions
   */
  def moreRepresentativeOf(m1: Mention, m2: Mention): Mention = {

    // First check the part of speech
    if ((m1.attr[MentionCharacteristics].isProper) && (m2.attr[MentionCharacteristics].isPRO || m2.attr[MentionCharacteristics].isNoun))
      return m1
    else if ((m1.attr[MentionCharacteristics].isNoun) && (m2.attr[MentionCharacteristics].isPRO))
      return m1
    if ((m2.attr[MentionCharacteristics].isProper) && (m1.attr[MentionCharacteristics].isPRO || m1.attr[MentionCharacteristics].isNoun))
      return m2
    else if ((m2.attr[MentionCharacteristics].isNoun) && (m1.attr[MentionCharacteristics].isPRO))
      return m2

    // Determine value for each tie-breaker
    val m1RankingAttributes: Seq[Int] = Seq[Int](-m1.phrase.headToken.positionInSentence + m1.phrase.start, m1.phrase.section.indexInDocument, m1.phrase.sentence.indexInSection, m1.phrase.headToken.positionInSentence, math.max(0, m1.phrase.tokens.length - 5), m1.phrase.tokens.length)
    val m2RankingAttributes: Seq[Int] = Seq[Int](-m2.phrase.headToken.positionInSentence + m2.phrase.start, m2.phrase.section.indexInDocument, m2.phrase.sentence.indexInSection, m2.phrase.headToken.positionInSentence, math.max(0, m2.phrase.tokens.length - 5), m2.phrase.tokens.length)

    for (i <- 0 to m1RankingAttributes.length - 1 by 1) {
      if (m1RankingAttributes(i) < m2RankingAttributes(i)) {
        return m1
      } else if (m1RankingAttributes(i) > m2RankingAttributes(i)) {
        return m2
      }
    }

    // As a final tie breaker, we just sort the mentions. In testing, this case was never hit
    if (m1.phrase.string <= m2.phrase.string)
      return m1
    else
      return m2
  }

  /**
   * Returns the depth of the mention in the parse tree
   * @param mention
   * @return
   */
  def depthOfMention(mention: Mention): Int = {
    var depth: Int = 0
    var tmp: Token = mention.phrase.headToken
    while (tmp.parseParent ne null) {
      tmp = tmp.parseParent
      depth+=1
    }
    return depth
  }

  /**
   * Returns the animacy of a mention
   * @param mention
   * @return
   */
  def getAnimacy(mention: Mention): Int = {

    // Check if Animate pronoun
    if (animatePronouns.contains(mention.phrase.headToken.string.trim.toLowerCase)) {
      return DCorefAnimacyDomain.ANIMATE
    } else if (mention.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) {
      if (mention.phrase.attr[OntonotesPhraseEntityType].intValue == OntonotesEntityTypeDomain.PERSON) {
        return DCorefAnimacyDomain.ANIMATE
      } else if (!(mention.phrase.attr[OntonotesPhraseEntityType].intValue == OntonotesEntityTypeDomain.MISC || mention.phrase.attr[OntonotesPhraseEntityType].intValue == OntonotesEntityTypeDomain.O)) {
        return DCorefAnimacyDomain.INANIMATE
      }
    }
    return DCorefAnimacyDomain.UNKNOWN
  }

  /**
   * Removes all entries from the various caches of the CorefUtil
   */
  def clearCaches(): Unit = {
    incompatibleCache.clear()
    compatibleCache.clear()
    invalidPronounDistanceCache.clear()
    validPronounDistanceCache.clear()
  }

  /**
   * Returns the mention string with determiners removed
   * @param m
   * @return
   */
  def mentionStringWithoutDeterminer(m: Mention): String = {
    var res: String = ""
    for (token <- m.phrase.tokens) {
      if (token.attr[PennPosTag].categoryValue != "DT" ||token.attr[PennPosTag].categoryValue != "WDT" || token.attr[PennPosTag].categoryValue != "PDT") {
        res = res + token.string + " "
      }
    }
    return res.trim
  }


  /**
   * Removes any phrases starting with a comma or a WDT that appear after the head token, returns the resulting
   * string of the mention.
   * @param mention
   * @return
   */
  def relaxString(mention: Mention): String = {
    val mentionHead: Token = mention.phrase.headToken
    var mentionString: String = ""
    val mentionHeadIdx: Int = mentionHead.positionInSection - mention.phrase.start
    var idx: Int = 0
    for (token <- mention.phrase.tokens) {
      if ((idx > mentionHeadIdx) && (token.string.trim == "," || token.attr[PennPosTag].categoryValue == "WDT" || token.attr[PennPosTag].categoryValue == "WP" || token.attr[PennPosTag].categoryValue == "WP$" || token.attr[PennPosTag].categoryValue == "WRB")) {
        return mentionString.trim
      } else {
        mentionString += token.string.trim + " "
      }
      idx += 1
    }
    return mentionString.trim
  }

}




/**
 * An implementation of a deterministic coreference system
 */
object DeterministicCoref extends DocumentAnnotator {

  // todo fix this
  @deprecated("This exists to preserve prior behavior, it should be a constructor argument", "10/5/15")
  val lexicon = new StaticLexicons()(LexiconsProvider.classpath)

  private val CFUtil: CorefUtil = new CorefUtil()

  // The ordered list of sieves used. The sieves will be applied in the order the appear in the list
  // Note: To turn debug information on pass a directory name as the second argument to the sieves you wish to debug, i.e. PreciseConstructSieve(CFUtil, "debug")
  private val _sieves: List[Sieve] = List(new ExactMatchSieve(CFUtil), new RelaxedStringMatchSieve(CFUtil), new PreciseConstructionsSieve(CFUtil, "", lexicon), new StrictHeadMatchingSieve(CFUtil), new StrictHeadMatchingSieveVar1(CFUtil), new StrictHeadMatchingSieveVar2(CFUtil), new AliasSieve(CFUtil), new RelaxedHeadMatchingSieve(CFUtil), new LexicalChainSieve(CFUtil), new PronounSieve(CFUtil))

  // A sorted version of the mentions.
  private var _sorted_mentions: Seq[Mention] = null

  // PreReq Attributes:
  def prereqAttrs: Seq[Class[_]] = ParseBasedPhraseFinder.prereqAttrs.toSeq ++ Seq(classOf[PennPosTag])

  // Adds the WithinDocCoref attribute
  def postAttrs = Seq(classOf[WithinDocCoref])

  val options: CorefOptions = new CorefOptions



  /**
   * Generates the list of potential mentions within the Document (doc) 
   * and adds each of these mentions to the WithinDocCoref object (coref)
   * Note that it adds each of these mentions with the attr[MentionCharacteristics]
   * which is used by the different sieves.
   */
  private def annotateMentions(doc: Document): Unit = {

    // Mention detection method number 1
    if (false) {
      if (doc.coref.mentions.isEmpty) (ConllPhraseFinder(doc) ++ PronounFinder(doc) ++ NnpPosNounPhraseFinder(doc) ++ AcronymNounPhraseFinder(doc)).distinct.foreach(phrase => doc.getCoref.addMention(phrase))
      doc.coref.mentions.foreach(mention => NounPhraseEntityTypeLabeler.process(mention.phrase))
      doc.coref.mentions.foreach(mention => NounPhraseGenderLabeler.process(mention.phrase))
      doc.coref.mentions.foreach(mention => NounPhraseNumberLabeler.process(mention.phrase))
      doc.coref.mentions.foreach(mention => mention.attr += new MentionCharacteristics(mention, lexicon))
    } else {

      // if the document has not been parsed. N.B. This only applies if you are using Gold mentions (and are using the testing framework)
      if (!doc.sentences.isEmpty && doc.sentences.toSeq(0).parse == null) {
        doc.sentences.foreach(OntonotesTransitionBasedParser.process)
      }

      // Parse based mention detection
      if (doc.getCoref.mentions == null || doc.getCoref.mentions.isEmpty) {
        val phraseMentions: Seq[Phrase] = ParseBasedPhraseFinder.getPhrases(doc)
        for (phrase <- phraseMentions.distinct) {
          doc.getCoref.addMention(phrase)
        }
        CFUtil.removeLiesWithinMentionsWithSameHead(doc)
      }

      // Add mention attributes
      for (mention <- doc.getCoref.mentions) {
        mention.attr += new MentionCharacteristics(mention, lexicon)
        mention.attr += new DeterministicCorefCache(mention)
      }

      // Label phrases
      doc.getCoref.mentions.foreach(mention => NounPhraseEntityTypeLabeler.process(mention.phrase))
      doc.getCoref.mentions.foreach(mention => NounPhraseGenderLabeler.process(mention.phrase))
      doc.getCoref.mentions.foreach(mention => NounPhraseNumberLabeler.process(mention.phrase))
    }

  }





  /**
   * Determine the ordering of the candidate antecedents for each mention. The order of the antecedents
   * is determined by the position of the mentions in the parse tree. For all mentions, antecedents are considered
   * in reverse sentence order from the mention. For pronominal mentions, the order of antecedents is always from
   * shallowest to deepest in the parse tree, earliest appearing in the sentence to latest appearing in the sentence.
   * For nominal mentions, antecedents in the same sentence are ordered shallowest to deepest, earliest to latest and
   * antecedents in previous sentences are ordered shallowest to deepest, latest to earliest.
   *
   * The ordering is sorted as a map from the uniqueId of a mention to a list of integers which are the index
   * into the set of mentions for the document for the antecedents.
   * @param document
   * @return
   */
  def determineOrdering(document: Document): HashMap[String, List[Int]] = {
    // For each mention we want to find it's score in each sentence
    val DEPTH_CONSTANT: Int = 10000
    val START_CONSTANT: Int = 100
    val END_CONSTANT: Int = 1
    // Find the Left - to - Right first ordering of the mentions in each sentence

    val sentenceNumber2LROrdering: HashMap[Int,Seq[Tuple2[Int,Int]]] = HashMap[Int,Seq[Tuple2[Int,Int]]]()
    var idx:Int = 0
    for (mention <- _sorted_mentions) {
      val mSentNo: Int = mention.attr[DeterministicCorefCache].absoluteSentenceNumber
      val mentionScore: Int = CFUtil.depthOfMention(mention)*DEPTH_CONSTANT + mention.phrase.start*START_CONSTANT + mention.phrase.end*END_CONSTANT

      if (!sentenceNumber2LROrdering.contains(mSentNo)) {
        sentenceNumber2LROrdering += (mSentNo -> Seq(Tuple2[Int,Int](idx,mentionScore)))
      } else {
        sentenceNumber2LROrdering(mSentNo) = sentenceNumber2LROrdering(mSentNo)  :+ Tuple2[Int,Int](idx,mentionScore)
      }
      idx += 1
    }

    for (key <- sentenceNumber2LROrdering.keys) {
      sentenceNumber2LROrdering.update(key, sentenceNumber2LROrdering(key).sortBy(m => (m._2,m._1)))
    }

    var ordering: HashMap[String, List[Int]] = HashMap[String, List[Int]]()
    for (mention <- _sorted_mentions) {
      val mentionSentenceNo: Int = mention.attr[DeterministicCorefCache].absoluteSentenceNumber
      var candidateOrdering: Seq[Int] = sentenceNumber2LROrdering(mentionSentenceNo).map(_._1)
      if (mention.attr[MentionCharacteristics].isPRO) {
        for (i <- mentionSentenceNo - 1 to 0 by -1)
          if (sentenceNumber2LROrdering.contains(i))
            candidateOrdering = candidateOrdering ++ sentenceNumber2LROrdering(i).map(_._1)
      } else {
        for (i <- mentionSentenceNo - 1 to 0 by -1)
          if (sentenceNumber2LROrdering.contains(i))
            candidateOrdering = candidateOrdering ++ sentenceNumber2LROrdering(i).map(_._1).reverse
      }
      ordering += (mention.uniqueId -> candidateOrdering.toList)
    }
    return ordering
  }


  def tokenAnnotationString(token: Token): String = {
    val entities = token.document.getCoref.entities.toSeq
    _sorted_mentions.find(m => m.phrase.contains(token)) match {
      case Some(mention) =>
        val mtokens = mention.phrase.tokens
        if (mtokens.length == 1) "(" + entities.indexOf(mention.entity) + ")"
        else if (mtokens.indexOf(token) == 0) "(" + entities.indexOf(mention.entity)
        else if (mtokens.indexOf(token) == mtokens.length - 1) entities.indexOf(mention.entity) + ")"
        else "_"
      case None => "_"
    }
  }

  /**
   *  Find and resolve mentions in a document
   */
  def process(document: Document) = {


    CFUtil.clearCaches()

    //val startTime = java.lang.System.currentTimeMillis()

    // Find all mentions in the document
    annotateMentions(document)

    // Reset the entity attributes of the mentions, in the case we are using gold mentions
    document.getCoref.resetPredictedMapping()

    // A list of mentions sorted by their position in the document.
    _sorted_mentions = document.getCoref.mentions.sortBy(m => (m.phrase.start, m.phrase.end))

    // Determine the ordering of antecedents for the mentions
    val ordering: HashMap[String, List[Int]] = determineOrdering(document)

    // a new mention cluster manager object.
    val cm = new MentionClusterManager(_sorted_mentions, CFUtil)

    // Use each of the sieves to resolve the mentions
    for (sieve <- _sieves) {
      sieve.resolveMentions(_sorted_mentions, ordering, cm, document)
    }

    /* Convert the clusters into Entity groups */
    for (mention <- _sorted_mentions) {
      val cluster: MentionCluster = cm.getCluster(mention)
      var bestCand: Mention = cluster.firstMention
      if (bestCand.entity ne null) {
        bestCand.entity += mention
      } else {
        val entity = document.getCoref.newEntity()
        entity += bestCand
        entity += mention
      }
    }
    //println("Document " + document.uniqueId + " Processed in " + ( java.lang.System.currentTimeMillis() - startTime) + " ms")
    document
  }

  /**
   * A subclass used for evaluation purposes. Follows the similar class in ForwardCoref.
   */
  class CorefTester(scorer: CorefConllOutput, scorerMutex: Object, val pool: ExecutorService) {
    def map(doc: Document): Unit = {

      // Make sure that the targetCoref is there
      assert(doc.targetCoref ne null, "Cannot perform test on document without test key.")
      val trueCoref = doc.targetCoref

      // Reset the predicted mapping
      if (doc.coref ne null) {
        doc.coref.resetPredictedMapping()
      }

      // process the document
      process(doc)

      // remove singletons
      val predCoref = doc.getCoref
      predCoref.removeSingletons()

      // score

      val b3 = ClusterF1Evaluation.BCubedNoSingletons(predCoref, trueCoref)
      val ce = ClusterF1Evaluation.CeafE(predCoref, trueCoref)
      val muc = ClusterF1Evaluation.MUCNoSingletons(predCoref, trueCoref)
      val cm = ClusterF1Evaluation.CeafM(predCoref, trueCoref)

      scorerMutex.synchronized {
        scorer.microB3.microAppend(b3)
        scorer.microCE.microAppend(ce)
        scorer.microCM.microAppend(cm)
        scorer.microMUC.microAppend(muc)
      }
    }

    def runParallel(ins: Seq[Document]) = cc.factorie.util.Threading.parMap(ins, pool)(map)

    def runSequential(ins: Seq[(Document)]) = ins.map(map)
  }

  /**
   * Perform the classification test on the set of documents passed in to the function
   * @param testDocs
   * @param wn
   * @param name
   * @return
   */
  def doTest(testDocs: Seq[Document], wn: WordNet, name: String): Double = {
    val scorer = new CorefConllOutput
    object ScorerMutex
    val pool = java.util.concurrent.Executors.newFixedThreadPool(options.numThreads)
    var accuracy = 0.0
    try {
      val tester = new CorefTester(scorer, ScorerMutex, pool)
      tester.runSequential(testDocs)
      println("-----------------------")
      println("  * Overall scores")
      scorer.printInhouseScore(name)
      accuracy = scorer.microMUC.f1
    } finally pool.shutdown()
    accuracy
  }
}


/**
 * A trait extended by each of the sieves used for debugging, defines useful methods for easily writing debug output.
 */
trait DeterministicCorefDebug {
  lazy val debugOutputDir: String = ""
  lazy val debugOutputFilename: String = ""
  private lazy val debugOutputDirectoryFile: java.io.File = new java.io.File(debugOutputDir)
  lazy val debugOutputFile: java.io.PrintWriter = {debugOutputDirectoryFile.mkdirs();  new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(debugOutputDir, debugOutputFilename), true)));}
  /**
   * The String which defines the HTML table format for the sieve output
   */
  val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\">"
  /**
   * Ending string of the table
   */
  val debugHTMLTableEnd: String = "</table>"
  lazy val performDebug: Boolean = {if (debugOutputDir == "" || debugOutputDir == null) false else true}

  /**
   * Writes the inputted string to the debugout output file and flushes the file
   * @param s
   */
  def debugPrint(s: String): Unit = {
    if (debugOutputFile != null && performDebug) {
      debugOutputFile.print(s)
      debugOutputFile.flush()
    }
  }

  /**
   * Writes the inputted string to the debugout output file, writes a new line and flushes the file
   * @param s
   */
  def debugPrintln(s: String = ""): Unit = {
    if (debugOutputFile != null && performDebug) {
      debugOutputFile.print(s + "\n")
      debugOutputFile.flush()
    }
  }

  /**
   * Method used for formatting debug output files, creates a HTML table entry of X if the inputted boolean is true
   * otherwise creates an empty table entry.
   * @param b
   * @return
   */
  def ifTrueX(b: Boolean): String = {
    if (b) return " <td> X </td> " else "<td> </td>"
  }
}


/**
 * The abstract definition of the Sieve class. The main difference between the sieves is what is used
 * as the matching criteria which is defined by  the matching function
 */
abstract class Sieve(CFUtil: CorefUtil, debugDirectory: String = "") extends DeterministicCorefDebug {

  /**
   * A flag which indicates whether or not nominal mentions will be resolved by the sieve. The default value
   * is true.
   */
  val resolveNominal: Boolean = true

  /**
   * A flag which indicates whether or not pronominal mentions will be resolved by the sieve. The default value is false.
   */
  val resolvePronominal: Boolean = false

  /**
   * The name of the sieve.
   */
  val name: String = "sieve"

  /**
   * A flag which indicates whether or not to force coreferent mentions to satisfy the compatibility requirements defined
   * in the CorefUtil class.
   */
  val restrictToCompatibleMentions: Boolean = true

  /**
   * A flag which requires pronominal mentions to satisify the pronoun distance requirement specified in the CorefUtil
   * class
   */
  val holdPronominalMentionsToDistanceRequirement: Boolean = true


  // Override the debug output directories
  override lazy val debugOutputDir: String = debugDirectory
  override lazy val debugOutputFilename: String = name + ".html"


  /**
   * The process for resolving mentions in a document. Iterate over each mention in the set of mentions comparing it to
   * candidate antecedents in the order specified by the ordering parameter, resolving mentions as defined by the particular sieve's matching function
   * @param mentions - the set of mentions
   * @param ordering - a map from the unique id of a mention to a list of candidate antecedents represented as integers, which are indices to the set of mentions
   * @param cm - the cluster manager
   * @param document - the document
   */
  def resolveMentions(mentions: Seq[Mention], ordering: HashMap[String, List[Int]], cm: MentionClusterManager, document: Document): Unit = {
    var i: Int = 0
    var idx: Int = 0
    var not_resolved: Boolean = true

   // val startTime = java.lang.System.currentTimeMillis()
    // Loop over mentions
    for (mention <- mentions) {
      // If the mention satisfies the pruning criteria, attempt to resolve it
      if (satisfiesPruningCriteria(mention)) {
        // Make sure that the mention is the first in the cluster
        if (cm.isFirstInCluster(mention)) {
          // Check to make sure it satisfies the nominal/pronominal restrictions
          if ((!mention.attr[MentionCharacteristics].isPRO && resolveNominal) || (mention.attr[MentionCharacteristics].isPRO && resolvePronominal)) {

            // Debug output
            if (performDebug) {
              debugPrintln("<p> <u> Attempting to Resolve Mention:  <b> <i> " + mention.phrase.string + " </b> </i> from sentence #" + mention.phrase.sentence.indexInSection + "</u>" +
                " words " + (mention.phrase.start - mention.phrase.sentence.start) + "-" + (mention.phrase.end - mention.phrase.sentence.start) +
                ". <br> The cluster of this word is: " + cm.getCluster(mention).toStringHTML +
                " <br> The most representative element is <b> " + cm.getCluster(mention).mostRepresentativeMention.phrase.string + "</b>" +
                " <br> The head token of the most representative element in the cluster is: <b>" + cm.getCluster(mention).mostRepresentativeMention.phrase.headToken.string.trim + "</b>" +
                " <br> The cluster attributes are: " + cm.getCluster(mention).attributeString + " </p>")
              debugPrintln(debugHTMLTableStart)
            }

            // i is the index into the list ordering(mention.uniqueId)
            i = 0
            not_resolved = true
            // Iterate through possible antecedents, stop if the mention is resolved
            while (i < ordering(mention.uniqueId).length && not_resolved) {
              idx = ordering(mention.uniqueId)(i)
              val candidate_match: Mention = mentions(idx)

              // Make sure that the mentions are compatible
               if ((!restrictToCompatibleMentions || !CFUtil.incompatible(cm.getCluster(mention), cm.getCluster(candidate_match))) &&
                 (!holdPronominalMentionsToDistanceRequirement || (!mention.attr[MentionCharacteristics].isPRO || !CFUtil.incompatiblePronounMatch(mention, candidate_match)))) {

                /*
                 * Make sure that the candidate antecedent (if in the same sentence, appears before the current mention)
                 * This is necessary because of the way in which the ordering is done -- rather than calculating
                 * the unique ordering for each mention, we do it on a sentence by sentence level.
                 */
                if (mention.phrase.start >= candidate_match.phrase.start && mention != candidate_match) {
                  if (matchingFunction(mention, candidate_match, cm)) {
                    // Update clustering if necessary
                    cm.mergeClusters(mention, candidate_match)
                    not_resolved = false
                  }
                }
              }
              i += 1
            }
            if (performDebug) {
              debugPrintln(debugHTMLTableEnd)
            }
          }
        }
      }
    }
    if (performDebug) {
      debugPrintln(cm.toHTMLString)
    }
    //println("\t" + name + " in " + ( java.lang.System.currentTimeMillis() - startTime) + " ms")
  }

  /**
   * Returns true only if the mention satisfies the pruning criteria. The default pruning criteria is that the
   * first token in the mention string is neither an indefinite article nor indefinite pronoun.
   */
  def satisfiesPruningCriteria(mention: Mention): Boolean = {
    return !mention.attr[DeterministicCorefCache].isIndefiniteArticleOrPronoun
  }

  /**
   * Each sieve defines this as its own function for resolving mentions.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean

}


/**
 * A sieve used to resolve mentions which are an exact, but case insensitive, string match
 */
class ExactMatchSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends Sieve(CFUtil, debugDirectory) {
  override val name: String = "ExactMatchSieve"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Exact Match <b> </td> </tr>"

  /**
   * The pruning criteria does not apply to this sieve.
   * @param mention
   * @return
   */
  override def satisfiesPruningCriteria(mention: Mention): Boolean = {
    return true
  }

  /**
   * The matching function of the exact match sieve resolves a mention to a candidate antecedent if the mentions are a case insensitive string match
   * or a case insenstive string match match with the exception of an apostrophe s
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {

    val res: Boolean = (currentMention.attr[MentionCharacteristics].lowerCaseString.equals(candidateAntecedent.attr[MentionCharacteristics].lowerCaseString) ||
      currentMention.attr[MentionCharacteristics].lowerCaseString.equals(candidateAntecedent.attr[MentionCharacteristics].lowerCaseString.replace(" 's", "")) ||
      candidateAntecedent.attr[MentionCharacteristics].lowerCaseString.equals(currentMention.attr[MentionCharacteristics].lowerCaseString.replace(" 's", "")))

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + "</td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res) + "</tr>")
    }

    return res
  }
}



/**
 * Precise Constructs Sieve, used for resolving mentions through specific grammatical constructions such as apposition as well as equivalent word forms such as demonyms and acronyms.
 */
class PreciseConstructionsSieve(CFUtil: CorefUtil, debugDirectory: String = "", lexicon:StaticLexicons) extends Sieve(CFUtil, debugDirectory) {
  override val name: String = "PreciseConstructionsSieve"

  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Appsositive <b> </td> <td> <b> Predicate Nominative <b> </td> <td> <b> Acronym <b> </td> <td> <b> Demonym <b> </td> <td> <b> Relative Pronoun </b> </td></tr>"
  override val resolvePronominal: Boolean = true

  /**
   * The matching function of the precise constructs sieve resolves a mention to a candidate antecedent if any of the following relationships between the mentions exists:
   * the mentions are in apposition of one another, the mentions are in a predicate nominative relationship, one mention is an acronym of the other mention, one mention is
   * a demonym of the other relation, or one mention is a relative pronoun of the other mention.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {

    val res1: Boolean = isAppositive(currentMention, candidateAntecedent, cm)
    val res2: Boolean = isPredicateNominative(currentMention, candidateAntecedent)
    val res3: Boolean = isAcronym(currentMention, candidateAntecedent)
    val res4: Boolean = isDemonym(currentMention, candidateAntecedent)
    val res5: Boolean = isRelativePronoun(currentMention, candidateAntecedent)

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res1) + ifTrueX(res2) + ifTrueX(res3) + ifTrueX(res4) + ifTrueX(res5) + "</tr>")
    }

    return (res1 || res2 || res3 || res4 || res5)
  }


  /**
   * Returns true if the two mentions are in apposition of one another and the mentions agree on all attributes (see MentionCluster class for definition of attribute agreement).
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  def isAppositive(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val currentMentionCluster: MentionCluster = cm.getCluster(currentMention)
    val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)
    if (currentMentionCluster.agreesInAllAttributesWith(candidateAntecedentCluster)) {
        return currentMention.phrase.isAppositionOf(candidateAntecedent.phrase)
    }
    return false
  }


  /**
   * Returns true if the current mention and candidate antecedent are in a predicate nominative relationship.
   * This is determined using the dependency tree checking that both mentions are noun phrases dependent on the verb "to be"
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def isPredicateNominative(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val currentMentionHead: Token = currentMention.phrase.headToken
    val candidateAntecedentHead: Token = candidateAntecedent.phrase.headToken
    val currentMentionSentence: Sentence = currentMention.attr[DeterministicCorefCache].mentionSentence
    var predNom: Boolean = false
    val currentMentionHeadParent: Token = currentMentionHead.parse.parent(currentMentionHead)
    val candidateAntecedentHeadParent: Token = candidateAntecedentHead.parse.parent(candidateAntecedentHead)
    val currentMentionSentenceRootChild: Token = currentMentionSentence.parse.rootChild
    if (CFUtil.inTheSameSentence(currentMention, candidateAntecedent) &&
      (currentMentionSentenceRootChild.lemmaString == "be")) {
      if (currentMentionHeadParent == null || candidateAntecedentHeadParent == null)
        return false
      predNom = ((currentMentionHeadParent == currentMentionSentenceRootChild) && (candidateAntecedentHeadParent == currentMentionSentenceRootChild))
    }

    return predNom
  }


  /**
   * Returns true if the current mention is a relative pronoun for the candidate antecedent
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def isRelativePronoun(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val res: Boolean = CorefFeatures.isRelativeFor(currentMention, candidateAntecedent)
    return res

  }

  /**
   * Uses the MentionCharacteristics function to generate possible acronyms for both the
   * current mention and candidate antecedent. If both mentions are proper nouns and one is in the list of
   * possible acronyms for the other, this function returns true.
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def isAcronym(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    var res: Boolean = false
    if ((currentMention.attr[MentionCharacteristics].isProper) && (candidateAntecedent.attr[MentionCharacteristics].isProper)) {

      res = (currentMention.attr[MentionCharacteristics].acronym.contains(currentMention.attr[MentionCharacteristics].lowerCaseString) || candidateAntecedent.attr[MentionCharacteristics].acronym.contains(candidateAntecedent.attr[MentionCharacteristics].lowerCaseString))
    }
    return res
  }


  /**
   * Returns true if the current mention is a demonym of the candidate antecedent or vice versa
   * @param currentMention
   * @param candidateAntecedent
   * @return
   */
  def isDemonym(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val currentMentionString: String = CFUtil.mentionStringWithoutDeterminer(currentMention)
    val candidateAntecedentString: String = CFUtil.mentionStringWithoutDeterminer(candidateAntecedent)
    val currentMentionDemonym: String = lexicon.iesl.DemonymMap.getOrElse(currentMentionString, "")
    val candidateAntecedentDemonym: String = lexicon.iesl.DemonymMap.getOrElse(candidateAntecedentString, "")
    var res: Boolean = false
    if (currentMentionDemonym.length > 0 && candidateAntecedentDemonym.length > 0)
      res = (currentMentionDemonym == candidateAntecedentDemonym)
    res = (res || currentMentionString.equalsIgnoreCase(candidateAntecedentDemonym) || currentMentionDemonym.equalsIgnoreCase(candidateAntecedentString))

    return res
  }
}


/**
 * The parent class of the strict and relaxed head matching sieves.
 * @param debugDirectory
 */
abstract class HeadMatchingSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends Sieve(CFUtil, debugDirectory) {
  override val name: String = "HeadMatchingSieve"

  override lazy val debugOutputDir: String = debugDirectory

  /**
   * Returns true if there does not exist a word (excluding stopwords) which appears in one mention's cluster
   * but not the other's
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  protected def satisfiesWordInclusion(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val currentMentionCluster: MentionCluster = cm.getCluster(currentMention)
    val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)
    var currentMentionWords: HashSet[String] = HashSet()
    val currentMentionHeadString: String = currentMention.attr[MentionCharacteristics].lowerCaseHead
    for (t <- currentMentionCluster.allTokens) {
      val t_string: String = new StringOps(t.string.trim).toLowerCase.toString
      if ((!CFUtil.stopWordsList.contains(t_string) && !t_string.equalsIgnoreCase(currentMentionHeadString))) {
        currentMentionWords += t_string.trim
      }
    }
    var candidateAntecedentWords: HashSet[String] = HashSet()
    for (t <- candidateAntecedentCluster.allTokens) {
      val t_string: String = new StringOps(t.string.trim).toLowerCase.toString
      if (!CFUtil.stopWordsList.contains(t_string)) {
        candidateAntecedentWords += t_string.trim
      }
    }
    return (0 == (currentMentionWords.diff(candidateAntecedentWords).size))
  }

}

/**
 * The standard strict head matching sieve. It is used to resolve mentions which share the same head word.
 * @param debugDirectory
 */
class StrictHeadMatchingSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends HeadMatchingSieve(CFUtil, debugDirectory) {
  override val name: String = "StrictHeadMatchingSieve"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td><td><b> Candidate Head </b></td><td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Cluster Head Match <b> </td> <td> <b> Satisfies Word Inclusion <b> </td> <td> <b> Compatible Modifiers Only <b> </td> <td> <b> Not i-within-i <b> </td> </tr>"

  /**
   * The matching function of the standard strict head matching sieve resolves a mention to a candidate antecedent
   * if the head token of the most representative element in the cluster of the current mention is an exact string match
   * of a head token of one of the mentions in the candidate antecedent cluster; word inclusion between the two clusters
   * holds, that is there are no non-stop words that appear in one cluster but not the other; and each mention in the two
   * clusters has compatible modifiers.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val res1: Boolean = isClusterHeadMatch(currentMention, candidateAntecedent, cm)
    val res2: Boolean = satisfiesWordInclusion(currentMention, candidateAntecedent, cm)
    val res3: Boolean = CFUtil.hasCompatibleModifiersOnly(cm.getCluster(currentMention), cm.getCluster(candidateAntecedent), cm)


    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.headToken.string + "</td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res1) + ifTrueX(res2) + ifTrueX(res3) + ifTrueX(true) + "</tr>")
    }

    return (res1 && res2 && res3)

  }


  /**
   * Returns true if  the head token of the most representative element in the cluster of the current mention is an exact string match
   * of a head token of one of the mentions in the candidate antecedent cluster.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  protected def isClusterHeadMatch(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val mostRepresentativeMentionInCurrentCluster: Mention = cm.getCluster(currentMention).mostRepresentativeMention
    val currentMentionHeadWord: String = mostRepresentativeMentionInCurrentCluster.attr[MentionCharacteristics].lowerCaseHead
    val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)
    for (mention <- candidateAntecedentCluster.mentions) {
      val candidateAntecedentHeadWord: String = mention.attr[MentionCharacteristics].lowerCaseHead
      if (currentMentionHeadWord.equals(candidateAntecedentHeadWord)) {
        return true
      }
    }
    return false
  }

}

/**
 * A variant of the strict head matching sieve, which removes the constraint on compatible modifiers
 * @param debugDirectory (optional) Output directory for debug information
 */
class StrictHeadMatchingSieveVar1(CFUtil: CorefUtil, debugDirectory: String = "") extends StrictHeadMatchingSieve(CFUtil, debugDirectory) {
  override val name: String = "StrictHeadMatchingSieve_no_CompatibleModifiers"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td> <b> Candidate Head </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Cluster Head Match <b> </td> <td> <b> Satisfies Word Inclusion <b> </td> </tr>"

  /**
   * The matching function for this variant of the strict head matching sieve is identical to the standard string head matching sieve
   * with the constraint on compatible modifiers removed.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val res1: Boolean = isClusterHeadMatch(currentMention, candidateAntecedent, cm)
    val res2: Boolean = satisfiesWordInclusion(currentMention, candidateAntecedent, cm)

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.headToken.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res1) + ifTrueX(res2) + "</tr>")
    }
    return (res1 && res2)

  }
}

/**
 * A variant of the strict head matching sieve, which removes the constraint on word inclusion
 * @param debugDirectory (optional) Output directory for debug information
 */
class StrictHeadMatchingSieveVar2(CFUtil: CorefUtil, debugDirectory: String = "") extends StrictHeadMatchingSieve(CFUtil, debugDirectory) {
  override val name: String = "StrictHeadMatchingSieve_no_wordInclusion"
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td> <b> Candidate Head </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Cluster Head Match <b> </td> <td> <b> Compatible Modifiers Only <b> </td> </tr>"

  /**
   * The matching function of this variant of the strict head matching sieve is identical to the standard strict head matching sieve's matching function
   * with the constraint on word inclusion removed.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val res1: Boolean = isClusterHeadMatch(currentMention, candidateAntecedent, cm)
    val res2: Boolean = CFUtil.hasCompatibleModifiersOnly(cm.getCluster(currentMention), cm.getCluster(candidateAntecedent), cm)

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td> " + candidateAntecedent.phrase.headToken.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res1) + ifTrueX(res2) +  "</tr>")
    }
    return (res1 && res2)

  }
}

/**
 * The relaxed head matching sieve is a variant of the strict head matching sieve, which involves matching the head word of a mention
 * to some word in an antecedent cluster.
 * @param debugDirectory (Optional) Directory for debug output
 */
class RelaxedHeadMatchingSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends HeadMatchingSieve(CFUtil, debugDirectory) {
  override val name: String = "RelaxedHeadMatchingSieve"

  // Debug Settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Relaxed Cluster Head Match <b> </td> <td> <b> Satisfies Word Inclusion <b> </td> <td> <b> Equal NER Type <b> </td> <td> <b> Not i-within-i <b> </td> </tr>"

  /**
   * The matching function of the relaxed head matching sieve resolves a mention to a candidate antecedent if the head token of the most representative
   * item in the current mention is an exact string match to some token in the candidate antecedent cluster and the two clusters satisfy the properties of
   * word inclusion (there are no non-stop words that appear in one cluster but not the other) and the two mentions have the same NER type (excluding the type of "O").
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val res1: Boolean = isRelaxedClusterHeadMatch(currentMention, candidateAntecedent, cm)
    val res2: Boolean = satisfiesWordInclusion(currentMention, candidateAntecedent, cm)
    val res3: Boolean = equalNERType(currentMention, candidateAntecedent, cm)

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res1) + ifTrueX(res2) + ifTrueX(res3) + ifTrueX(true) + "</tr>")
    }

    return (res1 && res2 && res3)

  }


  /**
   * Returns true if the head token of most representative element of the current mention cluster
   * matches any word in antecedent cluster
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  protected def isRelaxedClusterHeadMatch(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val currentMentionHeadWord: String = cm.getCluster(currentMention).mostRepresentativeMention.attr[MentionCharacteristics].lowerCaseHead
    val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)
    for (token <- candidateAntecedentCluster.allTokens) {
        if (currentMentionHeadWord.equalsIgnoreCase(token.string.trim)) {
          return true
        }
    }
    return false
  }

  /**
   * Returns true if the two mentions have the same NER type and that type is not "O"
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  protected def equalNERType(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    // if both are named entities with same type
    val currentMentionNER: Int = if (currentMention.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) currentMention.phrase.attr[OntonotesPhraseEntityType].intValue else OntonotesEntityTypeDomain.O
    val candidateAntecedentNER: Int = if (candidateAntecedent.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) candidateAntecedent.phrase.attr[OntonotesPhraseEntityType].intValue else OntonotesEntityTypeDomain.O

    val res: Boolean = (currentMentionNER != OntonotesEntityTypeDomain.O && currentMentionNER == candidateAntecedentNER)

    if (res)
      return true
    else if (currentMentionNER == OntonotesEntityTypeDomain.PERSON && (candidateAntecedent.phrase.attr[Gender].intValue == GenderDomain.PERSON || candidateAntecedent.phrase.attr[Gender].intValue == GenderDomain.MALE || candidateAntecedent.phrase.attr[Gender].intValue == GenderDomain.FEMALE))
      return true
    else if (candidateAntecedentNER == OntonotesEntityTypeDomain.PERSON && (currentMention.phrase.attr[Gender].intValue  == GenderDomain.PERSON || currentMention.phrase.attr[Gender].intValue == GenderDomain.MALE || currentMention.phrase.attr[Gender].intValue == GenderDomain.FEMALE))
      return true
    else
      return false
  }
}

/**
 * The sieve for resolving pronominal mentions
 * @param debugDirectory - (Optional) Directory for debug output
 */
class PronounSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends Sieve(CFUtil, debugDirectory) {
  override val name: String = "PronounSieve"

  // Resolve only pronominal references
  override val resolvePronominal: Boolean = true
  override val resolveNominal: Boolean = false

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Cluster</b> </td> <td><b> Attributes</b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Number <b> </td> <td> <b> Gender <b> </td> <td> <b> Person <b> </td> <td> <b> Animacy <b> </td> <td> <b> NER </b> </td> <td> <b> Personal Pronoun Agreement </b> </td> </tr>"


  /**
   * The matching function for the Pronoun Sieve resolves the current mention to a candidate antecedent if the two mentions' clusters agree on the attributes of person, gender, number, animacy and NER label.
   * Please see the MentionCluster class' documentation for a specific definition of agreement.
   * Additionally, personal pronouns have the added constraint that they must be resolved to mention which is specifically labeled as a person (i.e. either in the NER label or in the gender attribute)
   * and that exactly agrees in number (i.e not unknown "wildcard" agreement).
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    if (currentMention.attr[MentionCharacteristics].isPRO) {
      val currentMentionCluster: MentionCluster = cm.getCluster(currentMention)
      val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)


      if (performDebug) {
        val attributeAgreement: Seq[Boolean] = currentMentionCluster.attributeAgreement(candidateAntecedentCluster)
        val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
        val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
        debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td><td>" + cm.getCluster(candidateAntecedent).toStringHTML + "</td><td>" + cm.getCluster(candidateAntecedent).attributeString +
          "</td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(attributeAgreement(0)) +
          ifTrueX(attributeAgreement(1)) + ifTrueX(attributeAgreement(2)) + ifTrueX(attributeAgreement(3)) + ifTrueX(attributeAgreement(4)) +
          ifTrueX(personalPronounAgreement(currentMention, candidateAntecedent)) + "</tr>")
      }

      return currentMentionCluster.agreesInAllAttributesWith(candidateAntecedentCluster) && personalPronounAgreement(currentMention, candidateAntecedent)
    }

    return false
  }

  /**
   * Returns true if any of the following conditions are met:
   *  - The current mention is a singular personal pronoun and the antecedent is labeled as a singular person either by the NER label or gender attribute
   *  - The current mention is a plural personal pronoun -- in this case we do not place additional restrictions on the antecedent as the antecedents often are not labeled with PERSON NER labels or gender attributes
   *  - The current mention is NOT a personal pronoun and the candidate antecedent is NOT labeled as a person either by the NER label or gender attribute
   */
  private def personalPronounAgreement(currentMention: Mention, candidateAntecedent: Mention): Boolean = {
    val candidateAntecedentNER: Int = candidateAntecedent.attr[MentionCharacteristics].predictEntityType
    val candidateAntecedentGender: Int = candidateAntecedent.attr[MentionCharacteristics].genderIndex
    if (currentMention.phrase.attr[Number].intValue == NumberDomain.SINGULAR && CFUtil.personalPronouns.contains(currentMention.phrase.string.trim.toLowerCase))
      return (candidateAntecedent.phrase.attr[Number].intValue == NumberDomain.SINGULAR && (!candidateAntecedent.attr[MentionCharacteristics].isPRO || CFUtil.personalPronouns.contains(candidateAntecedent.phrase.string.trim.toLowerCase)) && (candidateAntecedentGender == GenderDomain.PERSON || candidateAntecedentGender == GenderDomain.MALE || candidateAntecedentGender == GenderDomain.FEMALE || candidateAntecedentNER == OntonotesEntityTypeDomain.PERSON))
    else if (currentMention.phrase.attr[Number].intValue == NumberDomain.PLURAL && CFUtil.personalPronouns.contains(currentMention.phrase.string.trim.toLowerCase))
      return true
    else if (!CFUtil.personalPronouns.contains(currentMention.phrase.string.trim.toLowerCase))
      return !(CFUtil.personalPronouns.contains(candidateAntecedent.phrase.string.trim.toLowerCase) || candidateAntecedentGender == GenderDomain.PERSON || candidateAntecedentGender == GenderDomain.MALE || candidateAntecedentGender == GenderDomain.FEMALE || candidateAntecedentNER == OntonotesEntityTypeDomain.PERSON)
    else
      return true
  }

}

/**
 * The Relaxed String Matching sieve is used to resolve mentions which have strings that are identical up except for phrases following the head token
 */
class RelaxedStringMatchSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends Sieve(CFUtil, debugDirectory) {
  
  override val name: String = "RelaxedStringMatch"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Relaxed String Match </b> </td> <td> <b> Relaxed String Mention </b> </td> <td> <b> Relaxed String Candidate Antecedent </b> </td> </tr>"


  /**
   * The matching function of the Relaxed String matching sieve resolves the current mention to the candidate antecedent if after removing any phrases starting 
   * with a comma or WDT that appear after the head token the two mention strings identically match.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val currentMentionRelaxedString: String = CFUtil.relaxString(cm.getCluster(currentMention).mostRepresentativeMention)
    val candidateAntecedentRelaxedString: String = CFUtil.relaxString(candidateAntecedent)
    val res: Boolean = (currentMentionRelaxedString.equalsIgnoreCase(candidateAntecedentRelaxedString))

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + "</td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>")
      if (res) debugPrint("<td> X </td>") else debugPrintln("<td>  </td>")
      debugPrintln("<td>" + currentMentionRelaxedString + "</td> <td>" + candidateAntecedentRelaxedString + "</td> </tr>")
    }

    return res
  }


}


/**
 * A sieve for resolving proper noun mentions with similar head words
 * @param debugDirectory
 */
class ProperHeadWordMatchSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends HeadMatchingSieve(CFUtil, debugDirectory) {

  // Name
  override val name: String = "ProperHeadWordMatch"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Both Proper Nouns </b> </td> <td> <b> Head Word String Match </b> </td> <td> <b> No Location Mismatches </b>  </td> <td> <b> No Number Mismatches </b> </td> </tr>"


  /**
   * The matching function of the ProperHeadWord sieve resolves the current mention to the candidate antecedent
   if both are proper nouns, have identical head tokens, and have agreement between location and numerical modifiers
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val res1: Boolean = currentMention.attr[MentionCharacteristics].isProper && candidateAntecedent.attr[MentionCharacteristics].isProper
    val res2: Boolean = currentMention.phrase.headToken.string.equalsIgnoreCase(candidateAntecedent.phrase.headToken.string)
    val res3: Boolean = CFUtil.agreesInLocation(currentMention, candidateAntecedent)
    val res4: Boolean = CFUtil.agreementBetweenModifiersWhichAreNumbers(currentMention, candidateAntecedent)

    if (performDebug) {
      val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
      val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
      debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" +
        ifTrueX(res1) + ifTrueX(res2) + ifTrueX(res3) + ifTrueX(res4) + "</tr>")
    }

    return res1 && res2 && res3 && res4 
  }
}


/**
 * A parent class for those sieves using Semantic Similarity
 * @param debugDirectory
 */
abstract class SemanticSimilaritySieve(CFUtil: CorefUtil, debugDirectory: String = "") extends Sieve(CFUtil, debugDirectory) {
  /**
   * A WordNet object used by the sieves
   */
  lazy val wn: WordNet = WordNet

  /**
   * Essentially a wrapper method to the WordNet lemmatizer. Returns the lemma
   * of a given mention string
   * @param mention
   * @return
   */
  protected def refineMentionStringForKB(mention: Mention): String = {
    val mentionString: String = mention.phrase.string.trim
    return wn.lemma(mentionString, "N")
  }
}


/**
 * A sieve which resolves mentions which are aliases of one another
 * @param debugDirectory
 */
class AliasSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends SemanticSimilaritySieve(CFUtil, debugDirectory) {

  override val name: String = "AliasSieve"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Synonyms </b> </td> <td> <b> Coref Feature Alias </b> </td> </tr>"


  /**
   * The matching function of the Alias Sieve resolves the current mention to a candidate antecedent mention if the most representative mention in the cluster of each is a proper noun
   * and either in the same synset in WordNet or labeled as aliases by the CorefFeatures.canBeAliases function.
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    val currentMentionMostRepresentative: Mention = cm.getCluster(currentMention).mostRepresentativeMention
    val candidateAntecedentMostRepresentative: Mention = cm.getCluster(candidateAntecedent).mostRepresentativeMention
    if (currentMentionMostRepresentative.attr[MentionCharacteristics].isProper && candidateAntecedentMostRepresentative.attr[MentionCharacteristics].isProper) {
      val sameSynset: Boolean = wn.areSynonyms(refineMentionStringForKB(currentMentionMostRepresentative), refineMentionStringForKB(candidateAntecedentMostRepresentative))
      val corefFeaturesAlias: Boolean = CorefFeatures.canBeAliases(currentMention, candidateAntecedent)
     if (performDebug) {
       val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
       val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
       debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(sameSynset) + ifTrueX(corefFeaturesAlias) + "</tr>")
     }
      return sameSynset || corefFeaturesAlias
    }
    return false
  }
}

/**
 * The Lexical Chain Sieve. A sieve that resolves mentions by using synonym paths in WordNet.
 * @param debugDirectory (optional) the name of a directory to write debug output
 */
class LexicalChainSieve(CFUtil: CorefUtil, debugDirectory: String = "") extends SemanticSimilaritySieve(CFUtil, debugDirectory) {

  override val name: String = "LexicalChainSieve"

  // Debug settings
  override lazy val debugOutputDir: String = debugDirectory
  override val debugHTMLTableStart: String = "<table border=\"1\" style=\"width:100%\"> <tr> <td><b> Candidate </b> </td> <td><b> Candidate Sent No </b> </td>  <td><b> Candidate Span </b> </td> <td> <b> Have WordNet Path (max length 4) </b> </td> </tr>"

  /**
   * The matching function of the Lexical Chain Sieve resolves a mention to a candidate antecedent if the sentence distance between the
   * two mentions is less than 3, the clusters of both the mention and antecedent agree on all attributes, the location & numerical modifiers
   * of each are consistent, and there is a path of length 4 or less between the synsets of the two mentions (including all senses of the words).
   * @param currentMention
   * @param candidateAntecedent
   * @param cm
   * @return
   */
  override def matchingFunction(currentMention: Mention, candidateAntecedent: Mention, cm: MentionClusterManager): Boolean = {
    // if attributes agree and mentions are less than three sentences apart, and they share a synonym, the match is ok
    val currentMentionCluster: MentionCluster = cm.getCluster(currentMention)
    val candidateAntecedentCluster: MentionCluster = cm.getCluster(candidateAntecedent)
    if (CFUtil.sentenceDistance(currentMention, candidateAntecedent) < 3 &&
      currentMentionCluster.agreesInAllAttributesWith(candidateAntecedentCluster) &&
      CFUtil.agreesInLocation(currentMention, candidateAntecedent) &&
      CFUtil.agreementBetweenModifiersWhichAreNumbers(currentMention, candidateAntecedent)) {
      val currentMentionSynonyms: HashSet[String] = getAllSynonyms(refineMentionStringForKB((currentMentionCluster.mostRepresentativeMention)))
      val candidateAntecedentSynonyms: HashSet[String] = getAllSynonyms(refineMentionStringForKB((candidateAntecedentCluster.mostRepresentativeMention)))
      val res: Boolean = (currentMentionSynonyms.intersect(candidateAntecedentSynonyms).size > 0)

      if (performDebug) {
        val start: Int = candidateAntecedent.phrase.start - candidateAntecedent.phrase.sentence.start
        val end: Int = candidateAntecedent.phrase.end - candidateAntecedent.phrase.sentence.start
        debugPrint("<tr> <td>" + candidateAntecedent.phrase.string + " </td> <td>" + candidateAntecedent.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td>" + ifTrueX(res) + "</tr>")
      }
      return res
    }
    return false
  }

  /**
   * Traverses the synsets of the word in WordNet gathering all possible synonyms (from all senses of the word)
   * up to levelsDeep synsets
   * @param mentionLemma - lemma used as input to wordnet
   * @param levelsDeep - number of synset layers to traverse
   * @return
   */
  private def getAllSynonyms(mentionLemma: String, levelsDeep: Int = 4): HashSet[String] = {
    var allSynonyms: HashSet[String] = HashSet[String]()
    var nextLemmas: HashSet[String] = HashSet[String](mentionLemma)
    var nextSynsets: Seq[Synset] = Seq[Synset]()
    for (i <- 0 to levelsDeep by 1) {
      for (lemma <- nextLemmas) {
        allSynonyms += lemma
        nextSynsets = nextSynsets ++ wn.synsets(lemma)
      }
      nextLemmas = HashSet[String]()
      for (syn <- nextSynsets) {
        nextLemmas += syn.id
      }
    }
    return allSynonyms
  }
}


/**
 * A management system of the mention clusters used by the deterministic coreference system
 * @param mentions
 */
class MentionClusterManager(mentions: Seq[Mention], CFUtil: CorefUtil) {

  // A mapping from the uniqueId of the mentions to their integer cluster id number
  private var _mention2clusterid: HashMap[String, Int] = HashMap[String, Int]()
  // A mapping from the integer cluster id number to the corresponding cluster
  private var _clusterid2cluster: HashMap[Int, MentionCluster] = HashMap[Int, MentionCluster]()

  /* Initialize the clusters for each of the mentions */
  var count: Int = 0
  for (mention <- mentions) {
    _mention2clusterid += (mention.uniqueId -> count)
    _clusterid2cluster += (count -> new MentionCluster(CFUtil))
    _clusterid2cluster(count).addMention(mention)
    count += 1
  }

  /**
   * Merges the currentMention's cluster with the candidateAntecedent's cluster
   * that is it adds every element of the currentMention's cluster to the candidateAntecedent's cluster
   * and destroys the old cluster.
   * @param currentMention
   * @param candidateAntecedent
   */
  def mergeClusters(currentMention: Mention, candidateAntecedent: Mention): Unit = {
    val cmClusterId: Int = getClusterId(currentMention)
    val cmCluster: MentionCluster = getCluster(currentMention)
    for (mention <- cmCluster.mentions) {
      setCluster(mention, candidateAntecedent)
    }
    _clusterid2cluster.remove(cmClusterId)
  }


  /**
   * Returns the cluster of the given mention
   * @param mention - a mention from the document
   * @return the MentionCluster of the mention
   */
  def getCluster(mention: Mention): MentionCluster = {
    return _clusterid2cluster(_mention2clusterid(mention.uniqueId))
  }

  /**
   * Returns true if the given mention is the mention in its cluster which appears
   * earliest in the document.
   * @param mention
   * @return
   */
  def isFirstInCluster(mention: Mention): Boolean = {
    val mentionsCluster: MentionCluster = this.getCluster(mention)
    return (mention eq mentionsCluster.firstMention)
  }

  /**
   * Returns an HTML formatted string of the cluster assignments.
   * @return
   */
  def toHTMLString: String = {
    var s: String = "<br><br><br><p><b> Current Cluster Assignments </b> </p>"
    for (paircidcluster <- _clusterid2cluster) {
      if (paircidcluster._2.mentions.size > 0) {
        s += "\n <table border=\"1\" style=\"width:75%\">\n <tr> <td> <b> Mention Sentence </b> </td> <td><b> Mention Span </b> </td> <td><b> Mention</b> </td> </tr>"
        for (mention <- paircidcluster._2.mentions) {
          val start: Int = mention.phrase.start - mention.phrase.sentence.start
          val end: Int = mention.phrase.end - mention.phrase.sentence.start
          s += "\n <tr> <td> " + mention.phrase.sentence.indexInSection + "</td> <td> " + start + "-" + end + "</td> <td> " + mention.phrase.string + "</td> </tr>"
        }
        s += "\n </table><p><br><br><br></p>"
      }
    }
    return s
  }


  /**
   * Sets the cluster id of mention to the passed in value
   * @param mention
   * @param new_cluster_id
   */
  private def setCluster(mention: Mention, new_cluster_id: Int): Unit = {
    _clusterid2cluster(_mention2clusterid(mention.uniqueId)).removeMention(mention)
    _clusterid2cluster(new_cluster_id).addMention(mention)
    _mention2clusterid.update(mention.uniqueId, new_cluster_id)
  }

  /**
   * Sets the cluster id of mention1 to be that of mention2, that is
   * it places mention1 in mention2's cluster.
   * @param mention1
   * @param mention2
   */
  private def setCluster(mention1: Mention, mention2: Mention): Unit = {
    setCluster(mention1, _mention2clusterid(mention2.uniqueId))
  }

  /**
   * Returns the cluster ID number of the given mention
   * @param mention
   * @return
   */
  private def getClusterId(mention: Mention): Int = {
    return _mention2clusterid(mention.uniqueId)
  }
}




/**
 * A representation of the intermediary clusters of mentions used by the deterministic coreference system.
 * The clusters maintain information about various attributes of their mentions.
 */
class MentionCluster (CFUtil: CorefUtil) {
  /*
   * An internal data structure used for management of the attributes of the cluster
   */
  private object MentionClusterAttribute extends Enumeration {
    type MentionClusterAttribute = Value
    val NUMBER, GENDER, PERSON, ANIMACY, NER = Value
  }


  /*
   * A mapping from the uniqueId field of the mentions to the mentions themselves.
   * Note that we could not just keep a HashSet of the mentions, because hashing
   * on the Mention objects themselves will cause problems
   */
  private val _mentionMap: HashMap[String,Mention] = HashMap[String,Mention]()
  
  /*
   * A Seq of all of the mentions in the cluster
   */
  private var _mentions: Seq[Mention] = Seq[Mention]()

  /*
   * A Seq of all of the tokens of the mentions in the cluster
   */
  private var _allTokens: Seq[Token] = Seq[Token]()

  /*
   * A hashmap from each of the MentionCluster attributes to the HashSet which
   * stores the values of the attributes for each mention in the cluster
   */
  private val _attributes: HashMap[MentionClusterAttribute.Value, HashSet[Int]] = HashMap[MentionClusterAttribute.Value, HashSet[Int]]()
  // Initialize the _attribute structure
  for (at <- MentionClusterAttribute.values) {
    _attributes.put(at, HashSet[Int]())
  }

  /*
   * The mention in the cluster which appears first in the document
   */
  private var _firstMention: Mention = null

  /*
   * The "most representative" mention in the cluster, see method definition
   * for explanation. 
   */
  private var _mostRepresentativeMention: Mention = null


  /**
   * Returns an formatted string of the mentions in the cluster, in the form
   * "{<sentence_no>:(<phrase_start>,<phrase_end>):<mention_string>}"
   */
  override def toString: String = {
    var s: String = "{ "
    for (m <- _mentions) {
      s += m.phrase.sentence.indexInSection + "(" + m.phrase.start +"," + m.phrase.end + "):" + m.phrase.string + " "
    }
    s += "}"
    return s
  }

  /**
   * Returns an HTML formatted string of the mentions in the cluster
   */
  def toStringHTML: String = {
    var s: String = "{ "
    for (m <- _mentions) {
      s += m.phrase.sentence.indexInSection + "(" + m.phrase.start +"," + m.phrase.end + "):<b>" + m.phrase.string + "</b> "
    }
    s += "}"
    return s
  }

  /**
   * Returns a Seq of all of the mentions in the cluster
   */
  def mentions: Seq[Mention] = {
    _mentions
  }

  /**
   * Adds a mention to the cluster
   * @param mention - the mention to add
   */
  def addMention(mention: Mention): Unit = {

    // Update the mention Map
    _mentionMap.put(mention.uniqueId, mention)

    // Update _mentions
    _mentions = _mentionMap.values.toSeq.sortBy(m => (m.attr[DeterministicCorefCache].absoluteSentenceNumber, m.phrase.start, m.phrase.end))

    // Update first mention
    _firstMention = _mentions(0)

    // Update most representative mention
    if (_mostRepresentativeMention == null) {
      _mostRepresentativeMention = mention
    } else {
      for (mention <- _mentionMap.values) {
        if (mention.uniqueId != _mostRepresentativeMention.uniqueId)
          _mostRepresentativeMention = CFUtil.moreRepresentativeOf(_mostRepresentativeMention, mention)
      }
    }
    // _mostRepresentativeMention = CorefUtil.moreRepresentativeOf(_mostRepresentativeMention, mention)
    // Update each set of attributes & _allTokens
    _attributes(MentionClusterAttribute.NUMBER).add(mention.phrase.attr[Number].intValue)
    _attributes(MentionClusterAttribute.GENDER).add(mention.phrase.attr[Gender].intValue)
    _attributes(MentionClusterAttribute.PERSON).add(CFUtil.getPerson(mention))
    _attributes(MentionClusterAttribute.ANIMACY).add(CFUtil.getAnimacy(mention))
    if (mention.phrase.attr.contains(classOf[OntonotesPhraseEntityType])) {
      _attributes(MentionClusterAttribute.NER).add(mention.phrase.attr[OntonotesPhraseEntityType].intValue)
    } else {
      _attributes(MentionClusterAttribute.NER).add(OntonotesEntityTypeDomain.O)
    }


    _allTokens = _allTokens ++ mention.phrase.tokens
  }


  /**
   * Removes a mention from the cluster
   * @param mention - The mention to remove
   */
  def removeMention(mention: Mention): Unit = {
    _mentionMap.remove(mention.uniqueId)
  }

  /**
   * Returns the mention in the cluster which appears first in the document
   */
  def firstMention: Mention = {
    _firstMention
  }

  /**
   * Returns a Seq of all of the tokens of the mentions in the cluster
   */
  def allTokens: Seq[Token] = {
    _allTokens
  }

  /**
   * Returns a HashSet of the String representation of the gender attributes of the mentions in the cluster
   */
  def numberAttributes: HashSet[Int] = {
    _attributes(MentionClusterAttribute.NUMBER)
  }

  /**
   * Returns a HashSet of the String representation of the gender attributes of the mentions in the cluster
   */
  def genderAttributes: HashSet[Int] = {
    _attributes(MentionClusterAttribute.GENDER)
  }

  /**
   * Returns a HashSet of the String representation of the person attributes of the mentions in the cluster
   */
  def personAttributes: HashSet[Int] = {
    _attributes(MentionClusterAttribute.PERSON)
  }


  /**
   * Returns a HashSet of the String representation of the animacy attributes of the mentions in the cluster
   */
  def animacyAttributes: HashSet[Int] = {
    _attributes(MentionClusterAttribute.ANIMACY)
  }

  /**
   * Returns a HashSet of the String representation of the NER labels of the mentions in the cluster
   */
  def nerAttributes: HashSet[Int] = {
    _attributes(MentionClusterAttribute.NER)
  }

  /**
   * Returns the "most representative mention" in the cluster. 
   * The representativeness of a mention is determined as a total 
   * ordering of mentions such that: all proper nouns are more representative 
   * than common nouns, and all common nouns more representative than pronouns.
   * Mentions with the same part of speech are ordered first by 
   * their distance Start of the sentence (smaller distance is better), the section
   * of a mention (lower index is better), the sentence position in section (lower index is better)
   * head position in Sentence (earlier is better), the length of mention (if length < 5, shorter length is better, other longer length is better)
   */
  def mostRepresentativeMention: Mention = {
    _mostRepresentativeMention
  }

/**
   * Returns true if this at least one of this cluster's mentions has the same 
   * number attribute as one of the other cluster's mentions or if 
   * a mention in either cluster has unknown number. 
   * @param otherCluster
   */
  def agreesInNumberWith(otherCluster: MentionCluster): Boolean = {
    val thisMentionNumbers: HashSet[Int] = this.numberAttributes
    val otherMentionNumbers: HashSet[Int] = otherCluster.numberAttributes

    if (thisMentionNumbers.contains(NumberDomain.UNKNOWN) || otherMentionNumbers.contains(NumberDomain.UNKNOWN))
      return true
    val thisSETDIFFotherISEMPTY: Boolean = (thisMentionNumbers.diff(otherMentionNumbers).size == 0)
    val otherSETDIFFthisISEMPTY: Boolean = (otherMentionNumbers.diff(thisMentionNumbers).size == 0)
    return (thisSETDIFFotherISEMPTY || otherSETDIFFthisISEMPTY)
  }



  /**
   * Returns true if any of the following conditions hold: at least one of this cluster's 
   * mentions has the same gender attribute as one of the other cluster's mentions;  
   * a mention in either cluster has unknown gender; 
   * or a mention in one cluster has gender "PERSON" and a mention in the other cluster
   * has a gender of either male or female.
   * @param otherCluster
   */
  def agreesInGenderWith(otherCluster: MentionCluster): Boolean = {
    val thisMentionGenders: HashSet[Int] = this.genderAttributes
    val otherMentionGenders: HashSet[Int] = otherCluster.genderAttributes

    if (thisMentionGenders.contains(GenderDomain.UNKNOWN) || otherMentionGenders.contains(GenderDomain.UNKNOWN))
      return true

    // Handle case where one contains PERSON and the other contains either male or female.
    if (thisMentionGenders.contains(GenderDomain.PERSON) && (otherMentionGenders.contains(GenderDomain.MALE) || otherMentionGenders.contains(GenderDomain.FEMALE)))
      return true
    if (otherMentionGenders.contains(GenderDomain.PERSON) && (thisMentionGenders.contains(GenderDomain.MALE) || thisMentionGenders.contains(GenderDomain.FEMALE)))
      return true

    val thisSETDIFFotherISEMPTY: Boolean = (thisMentionGenders.diff(otherMentionGenders).size == 0)
    val otherSETDIFFthisISEMPTY: Boolean = (otherMentionGenders.diff(thisMentionGenders).size == 0)
    return (thisSETDIFFotherISEMPTY || otherSETDIFFthisISEMPTY)
  }


/**
   * Returns true if this at least one of this cluster's mentions has the same 
   * person attribute as one of the other cluster's mentions. 
   * @param otherCluster
   */
  def agreesInPersonWith(otherCluster: MentionCluster): Boolean = {
    val thisMentionPerson: HashSet[Int] = this.personAttributes
    val otherMentionPerson: HashSet[Int] = otherCluster.personAttributes
    val thisSETDIFFotherISEMPTY: Boolean = (thisMentionPerson.diff(otherMentionPerson).size == 0)
    val otherSETDIFFthisISEMPTY: Boolean = (otherMentionPerson.diff(thisMentionPerson).size == 0)
    return (thisSETDIFFotherISEMPTY || otherSETDIFFthisISEMPTY)
  }



  /**
   * Returns true if this at least one of this cluster's mentions has the same 
   * animacy attribute as one of the other cluster's mentions or if 
   * a mention in either cluster has unknown animacy. 
   * @param otherCluster
   */
  def agreesInAnimacyWith(otherCluster: MentionCluster): Boolean = {
    val thisMentionAnimacies: HashSet[Int] = this.animacyAttributes
    val otherMentionAnimacies: HashSet[Int] = otherCluster.animacyAttributes

    if (thisMentionAnimacies.contains(DCorefAnimacyDomain.UNKNOWN) || otherMentionAnimacies.contains(DCorefAnimacyDomain.UNKNOWN))
      return true

    val thisSETDIFFotherISEMPTY: Boolean = (thisMentionAnimacies.diff(otherMentionAnimacies).size == 0)
    val otherSETDIFFthisISEMPTY: Boolean = (otherMentionAnimacies.diff(thisMentionAnimacies).size == 0)

    return (thisSETDIFFotherISEMPTY || otherSETDIFFthisISEMPTY)

  }


  /**
   * Returns true if this cluster's mentions and the other cluster's mentions 
   * have at least one NER label in common or if one of the cluster's mentions
   * have an NER label of "O" or "MISC"
   * @param otherCluster
   */
  def agreesInNERLabelsWith(otherCluster: MentionCluster): Boolean = {
    val thisMentionNER: HashSet[Int] = this.nerAttributes
    val otherMentionNER: HashSet[Int] = otherCluster.nerAttributes
    if (thisMentionNER.contains(OntonotesEntityTypeDomain.O) || thisMentionNER.contains(OntonotesEntityTypeDomain.MISC) || otherMentionNER.contains(OntonotesEntityTypeDomain.O) || otherMentionNER.contains(OntonotesEntityTypeDomain.MISC))
      return true
    val thisSETDIFFotherISEMPTY: Boolean = (thisMentionNER.diff(otherMentionNER).size == 0)
    val otherSETDIFFthisISEMPTY: Boolean = (otherMentionNER.diff(thisMentionNER).size == 0)

    return (thisSETDIFFotherISEMPTY || otherSETDIFFthisISEMPTY)

  }


  /**
   * Returns a string representation of the cluster's attributes.
   * of the form "{<number attributes> || <gender attributes> || <person attributes> || <animacy attributes> || <ner attributes>}"
   */
  def attributeString: String = {
    val res: String = "{" + this.numberAttributes.toString + " || " + this.genderAttributes.toString + " || " + this.personAttributes.toString + " || " + this.animacyAttributes.toString + " || " + this.nerAttributes.toString + "}"
    return res.replaceAll("Set", "")
  }


  /**
   * Returns true only if this cluster agrees with the passed in cluster in
   * the attributes of number, gender, person, animacy, and NER labels. 
   * Please refer to the individual agreement methods for explanation of
   * the definition of agreement in these attributes.
   * @param otherCluster
   */
  def agreesInAllAttributesWith(otherCluster: MentionCluster): Boolean = {
    val res1: Boolean = this.agreesInNumberWith(otherCluster)
    val res2: Boolean = this.agreesInGenderWith(otherCluster)
    val res3: Boolean = this.agreesInPersonWith(otherCluster)
    val res4: Boolean = this.agreesInAnimacyWith(otherCluster)
    val res5: Boolean = this.agreesInNERLabelsWith(otherCluster)
    return (res1 && res2 && res3 && res4 && res5)
  }

  /**
   * Returns an Seq of 5 booleans, such that the positions in the Seq correspond to
   * agreement with otherCluster in the attributes in the following order: [number, gender, person, animacy, nerlabel]
   * @param otherCluster
   */
  def attributeAgreement(otherCluster: MentionCluster): Seq[Boolean] = {
    val res1: Boolean = this.agreesInNumberWith(otherCluster)
    val res2: Boolean = this.agreesInGenderWith(otherCluster)
    val res3: Boolean = this.agreesInPersonWith(otherCluster)
    val res4: Boolean = this.agreesInAnimacyWith(otherCluster)
    val res5: Boolean = this.agreesInNERLabelsWith(otherCluster)
    return Seq[Boolean](res1, res2, res3, res4, res5)
  }

}
