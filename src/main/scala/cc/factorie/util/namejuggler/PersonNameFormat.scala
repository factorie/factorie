/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package cc.factorie.util.namejuggler

import cc.factorie.util.ModelProvider
import cc.factorie.app.nlp.lexicon.TrieUnionLexicon
import cc.factorie.app.nlp.lexicon.iesl.{PersonFirstHigh, PersonFirstHighest}
import cc.factorie.util.namejuggler.StringUtils._

object PersonNameFormat {

  //todo fix this
  @deprecated("The classpath based provider here is unsafe, it should be moved to the constructor", "10/05/15")
  private lazy val nameLexicon = new TrieUnionLexicon("first names", new PersonFirstHighest()(ModelProvider.classpath()), new PersonFirstHigh()(ModelProvider.classpath()))

  // http://notes.ericwillis.com/2009/11/common-name-prefixes-titles-and-honorifics/
  // ** add map from expanded versions, e.g. Professor, Senator, etc.
  // ** Add all manner of religious prefixes, e.g. "very rev", "right rev", "rt. rev", "rt. rev dom", etc.
  private val validPrefixes =
    Set("Ms.", "Miss", "Mrs.", "Mr.", "Master", "Rev.", "Fr.", "Dr.", "Atty.", "Prof.", "Hon.", "Pres.", "Gov.", "Coach", "Ofc.", "Msgr.", "Sr.", "Br.", "Supt.", "Rep.",
      "Sen.", "Amb.", "Treas.", "Sec.", "Pvt.", "Cpl.", "Sgt.", "Adm.", "Maj.", "Capt.", "Cmdr.", "Lt.", "Lt. Col.", "Col.", "Gen.").map(_.toLowerCase)

  private val allValidPrefixes = {
    validPrefixes ++ validPrefixes.map(_.stripPunctuation)
  }

  // ** don't bother listing these-- too many possibilities
  // http://en.wikipedia.org/wiki/List_of_post-nominal_letters
  // http://en.wikipedia.org/wiki/Academic_degrees
  private val validDegrees = Seq("M.D.", "Ph.D.")

  private val degreeFixer = validDegrees.map(canonical => (canonical.toLowerCase.stripPunctuation, canonical)).toMap

  def fixDegree(s: String) : String = degreeFixer.getOrElse(s.toLowerCase.stripPunctuation, s.toUpperCase)

  private val allValidDegrees = {
    import StringUtils._
    validDegrees ++ validDegrees.map(_.stripPunctuation)
  }

  private val validHereditySuffixes =
    Seq("Jr.", "Sr.", "II", "III", "IV")

  // ** add more
  private val validSurnameParticles =
    Seq("van", "von", "der", "de", "du", "da", "di", "do", "la", "del", "della", "ter", "bin", "ben")

  def isPrefix(s: String): Boolean = {
    !s.trim.isEmpty && (allValidPrefixes.contains(s.trim.toLowerCase))
  }

  def isHereditySuffix(s: String): Boolean = {
    !s.trim.isEmpty  && (validHereditySuffixes.contains(s.trim))
  }


  def isSurnameParticle(s: String): Boolean = {
    !s.trim.isEmpty  && (validSurnameParticles.contains(s.trim))
  }

  def isSurnameParticleNoCase(s:String):Boolean = isSurnameParticle(s.toLowerCase)

  def isGivenNames(s:String):Boolean = {
    nameLexicon.containsLemmatizedWord(s.toLowerCase)
    //(Lexicons.firstnameHighest.countTokenMatchesLC(s) + Lexicons.firstnameHigh.countTokenMatchesLC(s)) > 0
  }
  
  def fixParticle(s:String) : String = {
    val lc = s.toLowerCase
    if (isSurnameParticle(lc)) lc else s
  }

  val consecutiveUpperCasePattern = ".*([A-Z][A-Z]).*".r

  /**
   * We can't be sure "MD" is a degree, because it could be the initials for Mark Dobson.
   * Conversely we can't be sure that a real degree conforms to any standard of punctuation or capitalization.
   * Someone might even be referred to as "Jim Beam, Brewer" in which case "Brewer" is a degree-like suffix.
   *
   * @param s
   * @return
   */
  def likelyDegree(s: String, containsLowerCase: Boolean, hasFirstName: Boolean): Boolean = {
    import StringUtils.enrichString

    if (s.stripPunctuation.size < 2) {
      false
    }
    else {
      val stringMatch = allValidDegrees.contains(s)

      val caseSuggestive = s match {
        case consecutiveUpperCasePattern(c) => {
          !c.isEmpty && containsLowerCase
        }
        case _ => false
      }

      // if we haven't already found a first name somewhere else, then we should interpret this as initials.
      stringMatch || (caseSuggestive && hasFirstName)
    }
  }

}

/**
 * A name format specification, for use both in formatting outputs and for forming expectations when parsing inputs.
 * @param withPrefixes
 * @param withSuffixes
 * @param givenFormat
 * @param surFormat
 * @param inverted
 * @param initialTerminator
 * @param initialSeparator
 * @param degreeAbbreviator
 * @param degreeSeparator
 */
case class PersonNameFormat(withPrefixes: Boolean, givenFormat: NameComponentFormat, surFormat: NameComponentFormat, inverted: Boolean,
                            invertedSeparator: String = ",", withSuffixes: Boolean, initialTerminator: String = ".", initialSeparator: String = ".",
                            degreeAbbreviator: String = ".", degreeSeparator: String = ", ", allCaps: Boolean = false)

sealed class NameComponentFormat

case object Omit extends NameComponentFormat

case object Ambiguous extends NameComponentFormat

case object FirstInitial extends NameComponentFormat

case object AllInitials extends NameComponentFormat

case object OneName extends NameComponentFormat

case object OneNameCaps extends NameComponentFormat

case object AllNames extends NameComponentFormat

case object AllNamesCaps extends NameComponentFormat
