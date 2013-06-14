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




class EntityType(val mention:Mention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
  def domain = OntonotesEntityTypeDomain
}



object OntonotesEntityTypeDomain extends CategoricalDomain[String] {
  this ++= Seq(
    "PERSON", "ORG", "GPE", "UKN", "DATE", "CARDINAL", "EVENT", "FAC", "LANGUAGE", "LAW", "LOC", "MONEY", "NORP", "ORDINAL", "PERCENT", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART"
  )

  freeze()
}


