package cc.factorie.app.nlp.mention
import cc.factorie._
import cc.factorie.app.nlp._

object MentionGenderDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  NEUTER,          // known to be non-person
  PERSON,          // person, but uncertain about gender
  MALE,            // male person
  FEMALE = Value   // female person
}
class MentionGenderLabel(val mention:Mention, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def this(m:Mention, initialIntValue:Int) = this(m, MentionGenderDomain(initialIntValue).category.asInstanceOf[String])
  def domain = MentionGenderDomain
}

/** Cheap gender predictor based on rules and lexicons. */
class MentionGenderLabeler extends DocumentAnnotator {
  val maleHonors = Set("mr.", "mr", "mister")
  val femaleHonors = Set("ms.", "ms", "mrs.", "mrs", "miss", "misses")
  def process(document:Document): Document = {
    import MentionGenderDomain._
    for (mention <- document.attr[MentionList]) {
      val gender = new MentionGenderLabel(mention, UNKNOWN)
      mention.attr += gender
      if (mention.span.length > 0) {
        val prnClassification = classifyPronoun(mention)
        if(prnClassification.isDefined){
           gender := prnClassification.get
        }else{
          val firstWord = mention.span(0).string.toLowerCase
          val lastWord = mention.span.last.string.toLowerCase
          var firstName = firstWord
          if (lexicon.iesl.PersonHonorific.containsWord(firstWord)) {
            gender := PERSON
            if (maleHonors.contains(firstWord)) gender := MALE
            else if (femaleHonors.contains(firstWord)) gender := FEMALE
            if (mention.span.length >= 3) firstName = mention.span(1).string.toLowerCase
          }
          if (gender.intValue != MALE && gender.intValue != FEMALE) {
            if (lexicon.iesl.Month.containsWord(firstWord)) gender := NEUTER
            else if (lexicon.uscensus.PersonFirstMale.containsWord(firstName)) gender := MALE
            else if (lexicon.uscensus.PersonFirstFemale.containsWord(firstName) && firstName != "an") gender := FEMALE
            else if (gender.intValue == MentionGenderDomain.UNKNOWN && lexicon.iesl.PersonLast.containsWord(lastWord)) gender := PERSON
            if (lexicon.iesl.City.contains(mention.span) || lexicon.iesl.Country.contains(mention.span) || lexicon.iesl.OrgSuffix.containsWord(lastWord))
              if (gender.intValue == UNKNOWN) gender := NEUTER else gender := UNKNOWN // Could be either person or other; mark it unknown
          }
        }
      }
    }
    document
  }
  def classifyPronoun(mention: Mention): Option[Int] = {
    if(mention.span.length > 1)
      return None
    else{
      val lemma = mention.span.tokens.head.lemmaString.toLowerCase
      if(maleWords.contains(lemma ))
        return Some(MentionGenderDomain.MALE)
      else if (femaleWords.contains(lemma))
        return Some(MentionGenderDomain.FEMALE)
      else return None
    }
  }
  val maleWords = Seq("he","him","his").toSet
  val femaleWords = Seq("she","her").toSet

  override def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[MentionList].filter(_.span.contains(token)); mentions.map(_.attr[MentionGenderLabel].categoryValue).mkString(",") }
  override def mentionAnnotationString(mention:Mention): String = { val t = mention.attr[MentionGenderLabel]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[MentionGenderLabel])
}

object MentionGenderLabeler extends MentionGenderLabeler

