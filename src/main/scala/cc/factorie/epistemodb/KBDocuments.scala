package cc.factorie.epistemodb

import com.google.common.collect.HashBiMap
import cc.factorie.app.nlp.{DocumentAnnotator, Document}
import cc.factorie.app.nlp.relation.RelationMentionList
import cc.factorie.app.nlp.{Document=>FactorieDocument}

trait XDocMention[T] {
  // identifies the document from which this mention came
  def docPointer:String
  // some representation of the entity used to do XDoc coref
  def entity:T
  // unique string for this mention
  def id:String
  // predicted entity id
  def predictedEnt:Int
  // true entity id
  def trueEnt:Option[Int]
}

trait XDocCorefSystem[T] {
  def generateXDocMentions(doc:FactorieDocument):Iterable[XDocMention[T]]
  def performCoref(mention:Iterator[XDocMention[T]]):Iterable[XDocMention[T]]
}

trait KbDocuments {
  type EntityMap = HashBiMap[String, Int]
  // - build the doc table
  // - build xDocMentions table
  def addDoc(doc: FactorieDocument)
  def documents: Iterator[FactorieDocument]
  // - use xDocMentions table (and maybe doc table)
  // - populates __entityMap
  // We should be able to share this implementation between Documents implementations
  def doXDocCoref[T](em: EntityMap, xcs: XDocCorefSystem[T]) {
    xcs.performCoref(documents.flatMap(xcs.generateXDocMentions)) foreach { ment =>
      em.put(ment.id, ment.predictedEnt)
    }
  }
}

class MongoDocuments extends KbDocuments {
  def addDoc(doc: FactorieDocument) {
    throw new UnsupportedOperationException
  }
  def documents: Iterator[FactorieDocument] = {
    throw new UnsupportedOperationException
  }
}

class MemoryDocuments extends KbDocuments {
  def addDoc(doc: FactorieDocument) {
    throw new UnsupportedOperationException
  }
  def documents: Iterator[FactorieDocument] = {
    throw new UnsupportedOperationException
  }
}

trait RelationDocumentAnnotator extends DocumentAnnotator {
  def postAttrs = Seq(classOf[RelationMentionList])
  // def process(doc:Document)
}