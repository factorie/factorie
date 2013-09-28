package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable.{EnumDomain, CategoricalVariable}

object NounPhraseGenderDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  NEUTER,          // known to be non-person
  PERSON,          // person, but uncertain about gender
  MALE,            // male person
  FEMALE = Value   // female person
}
class NounPhraseGenderLabel(val mention:NounPhrase, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def this(m:NounPhrase, initialIntValue:Int) = this(m, NounPhraseGenderDomain(initialIntValue).category.asInstanceOf[String])
  def domain = NounPhraseGenderDomain
}

/** Cheap gender predictor based on rules and lexicons. */
class NounPhraseGenderLabeler extends DocumentAnnotator {
  val maleHonors = Set("mr.", "mr", "mister")
  val femaleHonors = Set("ms.", "ms", "mrs.", "mrs", "miss", "misses")
  def process(document:Document): Document = {
    import NounPhraseGenderDomain._
    for (mention <- document.attr[NounPhraseList]) {
      val gender = new NounPhraseGenderLabel(mention, UNKNOWN)
      mention.attr += gender
      if (mention.length > 0) {
        val prnClassification = classifyPronoun(mention)
        if(prnClassification.isDefined){
           gender := prnClassification.get
        }else{
          val firstWord = mention(0).string.toLowerCase
          val lastWord = mention.last.string.toLowerCase
          var firstName = firstWord
          if (lexicon.iesl.PersonHonorific.containsWord(firstWord)) {
            gender := PERSON
            if (maleHonors.contains(firstWord)) gender := MALE
            else if (femaleHonors.contains(firstWord)) gender := FEMALE
            if (mention.length >= 3) firstName = mention(1).string.toLowerCase
          }
          if (gender.intValue != MALE && gender.intValue != FEMALE) {
            if (lexicon.iesl.Month.containsWord(firstWord)) gender := NEUTER
            else if (lexicon.uscensus.PersonFirstMale.containsWord(firstName)) gender := MALE
            else if (lexicon.uscensus.PersonFirstFemale.containsWord(firstName) && firstName != "an") gender := FEMALE
            else if (gender.intValue == NounPhraseGenderDomain.UNKNOWN && lexicon.iesl.PersonLast.containsWord(lastWord)) gender := PERSON
            if (lexicon.iesl.City.contains(mention) || lexicon.iesl.Country.contains(mention) || lexicon.iesl.OrgSuffix.containsWord(lastWord))
              if (gender.intValue == UNKNOWN) gender := NEUTER else gender := UNKNOWN // Could be either person or other; mark it unknown
          }
        }
      }
    }
    document
  }
  def classifyPronoun(mention: NounPhrase): Option[Int] = {
    if(mention.length > 1)
      return None
    else{
      val lemma = mention.tokens.head.lemmaString.toLowerCase
      if(maleWords.contains(lemma))
        return Some(NounPhraseGenderDomain.MALE)
      else if (femaleWords.contains(lemma))
        return Some(NounPhraseGenderDomain.FEMALE)
      else return None
    }
  }

  //since lemmaString is singular, we don't need to hard code in the plural form of these words
  val maleWords = Seq("he","him","his","man", "men","brother","boy", "male","uncle","nephew","father","grandfather", "king","lord","husband","boyfriend", "pope","priest").toSet
  val femaleWords = Seq("she","her", "woman","women","sister","girl","female","aunt","mother","niece","grandmother","queen","lady","wife","mistress","girlfriend","actress","nun").toSet

  override def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[NounPhraseList].filter(_.contains(token)); mentions.map(_.attr[NounPhraseGenderLabel].categoryValue).mkString(",") }
  override def phraseAnnotationString(mention:Phrase): String = { val t = mention.attr[NounPhraseGenderLabel]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[NounPhrase])
  def postAttrs: Iterable[Class[_]] = List(classOf[NounPhraseGenderLabel])
}

object MentionGenderLabeler extends NounPhraseGenderLabeler

