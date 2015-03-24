package cc.factorie.epistemodb

import cc.factorie.app.nlp.ner.NerTag
import cc.factorie.app.nlp.relation.RelationMentionList
import cc.factorie.app.nlp.{Document => FactorieDocument, DocumentAnnotator}
import com.google.common.collect.HashBiMap

case class MentionId(docid: String, startOffset: Int, length:Int)

trait XDocMention[T] {
  // identifies the document and offsets from which this mention came
  def mentionId: MentionId
  // some representation of the entity used to do XDoc coref
  // This stores of the necessary features.
  def features: T

  // predicted entity id
  def predictedEnt: Int

  // TODO: should this go into features?
  // TODO: or, should this be TAC type?
  def entityTag: NerTag

  // Maps kb name to link to kb entry.
  def kbLinks: Map[String, String]
}

trait XDocCorefSystem[T] {
  def generateXDocMentions(doc:FactorieDocument):Iterable[XDocMention[T]]
  def performCoref(mention:Iterator[XDocMention[T]]):Iterable[XDocMention[T]]
}

// TODO:
// do we need this?
// how do the relationmentions interact with the documents and the matrix?
trait KbDocuments {
  type EntityMap = HashBiMap[MentionId, Int]
  // - build the doc table
  // - build xDocMentions table
  def addDoc(doc: FactorieDocument)
  def documents: Iterator[FactorieDocument]
  // - use xDocMentions table (and maybe doc table)
  // - populates __entityMap
  // We should be able to share this implementation between Documents implementations
  def doXDocCoref[T](em: EntityMap, xcs: XDocCorefSystem[T]) {
    xcs.performCoref(documents.flatMap(xcs.generateXDocMentions)) foreach { ment =>
      em.put(ment.mentionId, ment.predictedEnt)
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