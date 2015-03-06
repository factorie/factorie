package cc.factorie.epistemodb

import com.google.common.collect.HashBiMap
import cc.factorie.app.nlp.{DocumentAnnotator, Document}
import cc.factorie.app.nlp.relation.RelationMentionList
import cc.factorie.app.nlp.{Document=>FactorieDocument}

/**
 * Created by beroth on 3/4/15.
 */
trait XDocCorefSystem {
  // TODO
}

trait KbDocuments {
  type EntityMap = HashBiMap[String, Int]
  // - build the doc table
  // - build xDocMentions table
  def addDoc(doc: FactorieDocument)

  def documents: Iterator[FactorieDocument]

  // - use xDocMentions table (and maybe doc table)
  // - populates __entityMap
  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem)
}

class MongoDocuments extends KbDocuments {
  def addDoc(doc: FactorieDocument) {
    throw new UnsupportedOperationException
  }

  def documents: Iterator[FactorieDocument] = {
    throw new UnsupportedOperationException
  }

  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem) {
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

  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem) {
    throw new UnsupportedOperationException
  }
}


trait RelationDocumentAnnotator extends DocumentAnnotator {
  def postAttrs = Seq(classOf[RelationMentionList])
  //  def process(doc:Document)
}

/*
trait relationMentionExtractor {
  getRelationMentions(doc): List[RelationMention]
}*/

