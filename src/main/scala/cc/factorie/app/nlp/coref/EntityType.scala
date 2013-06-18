package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp._
import cc.factorie.{CategoricalDomain, LabeledCategoricalVariable}
import mention.{MentionType, MentionList, Mention}

/**
 * Created with IntelliJ IDEA.
 * User: belanger
 * Date: 5/31/13
 * Time: 9:19 AM
 * To change this template use File | Settings | File Templates.
 */



//'Entity Type' is a misnomer that is used elsewhere in the literature, use it too. Really, this is a type associated with a mention, not an entity

class EntityType(val mention:Mention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesEntityTypeDomain
}



object OntonotesEntityTypeDomain extends CategoricalDomain[String] {
  this ++= Seq(
    "PERSON", "ORG", "GPE", "UKN", "DATE", "CARDINAL", "EVENT", "FAC", "LANGUAGE", "LAW", "LOC", "MONEY", "NORP", "ORDINAL", "PERCENT", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART"
  )

  freeze()
}


//this gives each Mention and EntityType. This is a very simple rule-based annotator that can not even produce predictions
//for many of the categories in  OntonotesEntityTypeDomain, only PERSON, ORG, GPE, and EVENT.
object EntityTypeAnnotator1 extends EntityTypeAnnotator1(null)  //the constructor for CorefGazetteers is such that if you pass it null, it reads from the classpath

class EntityTypeAnnotator1(lexDir: String) extends DocumentAnnotator {
  import EntityTypeAnnotator1Util._
  lazy val gaz = new CorefGazetteers(lexDir)
  def process1(document:Document): Document = {
    document.attr[MentionList].foreach(predictEntityType(_))
    document
  }
  def predictEntityType(m: Mention): Unit = {
    val prediction = classifyUsingRules(m.span.tokens.map(_.lemmaString),gaz)
    m.attr += new EntityType(m,prediction)
  }
  override def tokenAnnotationString(token:Token): String = {
    token.document.attr[MentionList].filter(mention => mention.span.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[EntityType].categoryValue + ":" + m.span.indexOf(token)).mkString(","); case _ => "_" }
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Mention])
  def postAttrs: Iterable[Class[_]] = List(classOf[EntityType])

}

//the reason some functions for above are pulled into a separate object as this simplifies the API with other projects greatly
object EntityTypeAnnotator1Util {
  final val articles = Seq("a","A","the","The").toSet

  //this expects cased strings as input
  def classifyUsingRules(strings: Seq[String], cg: CorefGazetteers): String = {

    val uStr = strings.filter(!articles.contains(_))
    val str = uStr.map(_.toLowerCase)
    val str1 = strings.mkString(" ")
    val uStr1 = uStr.mkString(" ")

    val isPerson = detectIfPerson(str,uStr, cg)
    val isPlace = detectIfPlace(str1,uStr1, cg)
    val isEvent = detectIfEvent(str1,uStr1, cg)
    val isOrganization = detectIfOrg(str1,uStr1, cg)
    val onlyOne =  Seq(isPerson, isPlace, isEvent,  isOrganization).count(y => y) == 1

    if(onlyOne){
      if(isPerson) return "PERSON"
      else if(isPlace) return "GPE"
      else if(isEvent) return "EVENT"
      else if(isOrganization) return "ORG"
      else
        return "UKN"
    }else{
      if(isPlace && isOrganization) //the place lexicon is mostly contained in the organization lexicon, so you need to treat it carefully.
        return "GPE"
      else if(isPlace && isPerson)
        return "GPE"
      else
        return "UKN"
    }
  }

  def detectIfPerson(strs: Seq[String], uStrs: Seq[String], cg: CorefGazetteers): Boolean = {
    val isCased = strs.zip(uStrs).exists(ab => ab._1 != ab._2)
    val str = strs.mkString(" ")
    val uStr = uStrs.mkString(" ")
    val fullStringPerson = cg.personFullNames.contains(str) && isCased
    val fields = strs
    val uFields = uStrs

    val firstIsCased = fields(0) != uFields(0)
    val secondIsCased = if(fields.length == 2) fields(1) != uFields(1) else false
    val firstContained = cg.personFirstWords.contains(fields(0))
    val secondContained =   if(fields.length == 2) cg.lastNames.contains(fields(1))  else false

    val firstName = fields.length == 2  && firstContained  &&  firstIsCased  && secondIsCased
    val firstName2 = fields.length == 1 && cg.firstNames.contains(fields(0))  &&  firstIsCased
    val lastName = fields.length == 2   && firstContained && secondContained   &&  secondIsCased && firstIsCased
    val lastName2 = fields.length == 1   && cg.lastNames.contains(fields(0))   &&  firstIsCased
    val bothNames = fields.length == 2 && firstContained && secondContained
    val isI = fields.length == 1 && uStr == "I"

    val isPerson = lastName || lastName2 || firstName || firstName2|| fullStringPerson  || bothNames
    isPerson  && ! isI
  }
  //the follow three methods just check for exact string matches
  def detectIfPlace(s: String, us: String, cg: CorefGazetteers): Boolean = {
    cg.placeWords.contains(s)
  }
  def detectIfOrg(s: String, us: String, cg: CorefGazetteers): Boolean = {
    cg.orgWords.contains(s)
  }
  def detectIfEvent(s: String, us: String, cg: CorefGazetteers): Boolean = {
    cg.events.contains(s)
  }

  def classifyUsingRules(rawString: String, cg: CorefGazetteers): String = {
    classifyUsingRules(rawString.split(" "),cg)
  }

}
