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
package cc.factorie.app.nlp.phrase

import cc.factorie.app.nlp._
import cc.factorie.variable.{EnumDomain, CategoricalVariable}
import scala.reflect.ClassTag
import cc.factorie.app.nlp.coref.{Mention, WithinDocCoref, MentionList}

object GenderDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  NEUTER,          // known to be non-person
  PERSON,          // person, but uncertain about gender
  MALE,            // male person
  FEMALE = Value   // female person
  freeze()
}

class Gender(categoryIndex:Int) extends CategoricalVariable[String](categoryIndex) {
  def this(category:String) = this(GenderDomain.index(category))
  final def domain = GenderDomain  
}
class PhraseGender(val phrase:Phrase, categoryIndex:Int) extends Gender(categoryIndex) {
  def this(phrase:Phrase, category:String) = this(phrase, GenderDomain.index(category))
}


/** Cheap gender predictor based on rules and lexicons. */
class PhraseGenderLabeler[A<:AnyRef](documentAttrToPhrases:(A)=>Iterable[Phrase])(implicit docAttrClass:ClassTag[A]) extends DocumentAnnotator {
  def process(document:Document): Document = {
    for (phrase <- documentAttrToPhrases(document.attr[A])) process(phrase)
    document
  }
  def process(phrase:Phrase): Unit = {
    import GenderDomain._
    val gender = new PhraseGender(phrase, UNKNOWN)
    phrase.attr += gender
    if (phrase.length > 0) {
      val genderFromLexicon = lexiconGender(phrase) 
      if (genderFromLexicon.isDefined) gender := genderFromLexicon.get
      else {
        val firstWord = phrase(0).string.toLowerCase
        val lastWord = phrase.last.string.toLowerCase
        var firstName = firstWord
        if (lexicon.iesl.PersonHonorific.containsWord(firstWord)) {
          gender := PERSON
          if (maleHonors.contains(firstWord)) gender := MALE
          else if (femaleHonors.contains(firstWord)) gender := FEMALE
          if (phrase.length >= 3) firstName = phrase(1).string.toLowerCase
        }
        if (gender.intValue != MALE && gender.intValue != FEMALE) {
          if (lexicon.iesl.Month.containsWord(firstWord)) gender := NEUTER
          else if (lexicon.uscensus.PersonFirstMale.containsWord(firstName)) gender := MALE
          else if (lexicon.uscensus.PersonFirstFemale.containsWord(firstName) && firstName != "an") gender := FEMALE
          else if (gender.intValue == GenderDomain.UNKNOWN && lexicon.iesl.PersonLast.containsWord(lastWord)) gender := PERSON
          if (lexicon.iesl.City.contains(phrase) || lexicon.iesl.Country.contains(phrase) || lexicon.iesl.OrgSuffix.containsWord(lastWord))
            if (gender.intValue == UNKNOWN) gender := NEUTER else gender := UNKNOWN // Could be either person or other; mark it unknown
        }
      }
    }
  }
  
  /** Test various words in the phrase to see if they indicate gender.  Return an index into the NounPhraseGenderDomain. */
  def lexiconGender(phrase: Phrase): Option[Int] = {
    if (phrase.length == 1) lexiconGender(phrase.tokens(0).string)
    else if (phrase.length == 2) lexiconGender(phrase.tokens(0).string).orElse(lexiconGender(phrase.tokens(1).string))
    else lexiconGender(phrase.headToken.string).orElse(lexiconGender(phrase.tokens(0).string).orElse(lexiconGender(phrase.tokens(1).string)))
  }
  def lexiconGender(word:String): Option[Int] = {
    val lemma = word.toLowerCase
    if (maleWords.contains(lemma)) Some(GenderDomain.MALE)
    else if (femaleWords.contains(lemma)) Some(GenderDomain.FEMALE)
    else None
}

  //since lemmaString is singular, we don't need to hard code in the plural form of these words
  val maleHonors = Set("mr.", "mr", "mister")
  val femaleHonors = Set("ms.", "ms", "mrs.", "mrs", "miss", "misses")
  
  val maleFemaleWords = Seq(
      ("", "actress"),
      ("", "adulteress"),
      ("", "giantess"),
      ("", "heiress"),
      ("", "hostess"),
      ("", "poetess"),
      ("", "shepherdess"),
      ("baron", "baroness"),
      ("boar", "sow"),
      ("boy", "girl"),
      ("boy-friend", "girl-friend"),
      ("boyfriend", "girlfriend"),
      ("bridegroom", "bride"),
      ("bro", "sis"),
      ("brother", "sister"),
      ("brother-in-law", "sister-in-law"),
      ("buck", "roe"),
      ("bull", "cow"),
      ("chap", ""),
      ("cock", "hen"),
      ("codger", ""),
      ("count", "countess"),
      ("dad", "mom"),
      ("dad", "mum"),
      ("daddy", "mommy"),
      ("deacon", "deaconess"),
      ("dude", "dame"),
      ("duke", "duchess"),
      ("emperor", "empress"),
      ("father", "mother"),
      ("father-in-law", "mother-in-law"),
      ("fiance", "fiancee"),
      ("fianc\u00E9", "fianc\u00E9e"),
      ("gigolo", "prostitute"),
      ("godfather", "godmother"),
      ("godson", "goddaughter"),
      ("grandfather", "grandmother"),
      ("grandpa", "grandma"),
      ("grandson", "granddaughter"),
      ("guy", "gal"),
      ("he", "she"),
      ("hero", "heroine"),
      ("him", "her"),
      ("his", "hers"),
      ("husband", "wife"),
      ("king", "queen"),
      ("lad", "lass"),
      ("landlord", "landlady"),
      ("lion", "lioness"),
      ("lord", "lady"),
      ("male", "female"),
      ("man", "woman"),
      ("manservant", "maidservant"),
      ("master", "mistress"),
      ("men", "women"),
      ("monk", "nun"),
      ("nephew", "niece"),
      ("pa", "ma"),
      ("papa", "mama"),
      ("papa", "mamma"),
      ("papa", "momma"),
      ("peacock", "peahen"),
      ("pop", "mom"),
      ("pope", ""),
      ("priest", "priestess"),
      ("prince", "princess"),
      ("ram", "ewe"),
      ("sir", "madam"),
      ("sir", "ma'am"),
      ("son-in-law", "daughter-in-law"),
      ("stallion", "mare"),
      ("step-father", "step-mother"),
      ("step-son", "step-daughter"),
      ("steward", "stewardess"),
      ("tiger", "tigress"),
      ("tom", "tib"), // cat or elephant
      ("uncle", "aunt"),
      ("waiter", "waitress"),
      ("widower", "widow")
      )
  val maleWords = maleFemaleWords.map(_._1).filter(_.length > 0).toSet
  val femaleWords = maleFemaleWords.map(_._2).filter(_.length > 0).toSet

  override def tokenAnnotationString(token:Token): String = { val phrases = documentAttrToPhrases(token.document.attr[A]).filter(_.contains(token)); phrases.map(_.attr[Gender].categoryValue).mkString(",") }
  override def phraseAnnotationString(phrase:Phrase): String = { val t = phrase.attr[Gender]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(docAttrClass.runtimeClass)//Require some TokenSpanList containing subclass of Phrase elements
  // Note that this postAttr doesn't indicate if all Phrases or just some Mention Phrases were actually labeled.
  def postAttrs: Iterable[Class[_]] = List(classOf[PhraseGender])
}

/** Gender label all phrases in the Document's NounPhraseList. */
class NounPhraseGenderLabeler extends PhraseGenderLabeler[NounPhraseList](phrase=>phrase)
object NounPhraseGenderLabeler extends NounPhraseGenderLabeler

/** Gender label phrases of all Mentions in the Document's MentionList. */
class MentionPhraseGenderLabeler extends PhraseGenderLabeler[Seq[Mention]](mentions =>mentions.map(_.phrase))
object MentionPhraseGenderLabeler extends MentionPhraseGenderLabeler

// No reason to have this.  The label for a Mention should always go on its Phrase. -akm
//object MentionGenderLabeler extends GenderLabeler[Mention,MentionList]