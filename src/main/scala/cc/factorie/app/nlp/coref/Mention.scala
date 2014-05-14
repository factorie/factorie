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

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase._
import cc.factorie.util.{Attr,UniqueId,ImmutableArrayIndexedSeq,EvaluatableClustering}
import cc.factorie.variable._
import scala.collection.mutable.ArrayBuffer

/** Either a mention, entity or sub-entity in an coreference or entity resolution model.
    These are the "nodes" in a trees in which observed mentions are the leaves and inferred entities are the roots.
    In "hierarchical coreference" there may be additional nodes at intermediate levels of the tree.
    @author Andrew McCallum */
trait Node extends UniqueId with Attr {
  type ParentType <: Node
  /** A pointer to the Node immediate above this Node in the tree. */
  def parent: ParentType
}

/** A "mention" of an entity in a resolution problem.
    A leaf in a coreference hierarchy.
    This is the super-trait for mentions in both within-document coreference and cross-document entity resolution.
    @author Andrew McCallum */
trait AbstractMention extends Node {
  def parent: ParentType
  /** The root of the coreference tree in which this mention is a leaf. */
  def entity: ParentType
  /** A string representation of the observed mention, e.g. "Michael Smith". */
  def string: String
}

/** An "entity" in an entity resolution problem.
    A non-leaf Node in a coreference hierarchy.  
    It could be a root (entity) or an intermediate node (sub-entity in hierarchical coref).
    This is the super-trait for entities in both within-document coreference and cross-document entity resolution.
    @author Andrew McCallum */
trait AbstractEntity extends Node {
  def children: Iterable[Node]  // Immediate children
  def childIds: Iterable[String] = ???
  def mentions: Iterable[AbstractMention] // Leaves of tree
}


// Below is infrastructure for within-document coreference

// TODO Turn this into a trait.  Only concrete will be an inner class of WithinDocCoref
/** An entity mention whose contents come from a nlp.phrase.Phrase.
    Users should not create these themselves, but rather use WithinDocCoref create them.
    The uniqueId is abstract.
    @author Andrew McCallum */
abstract class Mention(val phrase:Phrase) extends AbstractMention {
  type ParentType = WithinDocEntity
  private var _entity:WithinDocEntity = null
  protected[coref] def _setEntity(e:WithinDocEntity): Unit = _entity = e
  def entity: ParentType = _entity
  def parent: ParentType = _entity
  lazy val string = phrase.tokensString(" ")
  // If number, gender and entity type are needed, put a CategoricalVariable subclass in the Attr
}

// TODO All three of these classes should be removed. -akm
/** A collection of Mentions, either immutable or mutable. */
trait MentionCollection extends Iterable[Mention]
/** An immutable ordered collection of Mentions. */
class MentionList(mentions:Iterable[Mention]) extends ImmutableArrayIndexedSeq(mentions) with MentionCollection
/** An mutable ordered collection of Mentions. */
class MentionBuffer extends ArrayBuffer[Mention] with MentionCollection



/** An entity whose evidence comes from some Phrases within a single document.
    Users should not create these themselves, but rather use WithinDocCoref create them.
    The uniqueId is abstract.
    @author Andrew McCallum */
abstract class WithinDocEntity(val document:Document) extends AbstractEntity {
  type ParentType = WithinDocEntity
  private val _mentions = new scala.collection.mutable.LinkedHashSet[Mention]
  def parent: WithinDocEntity = null
  def mentions:scala.collection.Set[Mention] = _mentions
  def isSingleton:Boolean = _mentions.size == 1 //TODO Is this okay to do? or is there a better way
  def isEmpty:Boolean = _mentions.isEmpty
  def children: Iterable[Mention] = _mentions
  def getFirstMention: Mention = if(isEmpty) null else if(isSingleton) _mentions.head else mentions.toSeq.sortBy(m => m.phrase.start).head
  def +=(mention:Mention): Unit = {
    assert(mention.phrase.document eq document)
    //assert(!_mentions.contains(mention)) // No reason to do this; might catch a bug.
    if (mention.entity ne null) mention.entity._mentions -= mention
    if(!_mentions.contains(mention))_mentions += mention
    mention._setEntity(WithinDocEntity.this)
  }
  def -=(mention:Mention): Unit = {
    assert(mention.phrase.document eq document)
    assert(_mentions.contains(mention)) // No reason to do this; might catch a bug.
    assert(mention.entity == this)
    _mentions -= mention
    mention._setEntity(null)
  }
  var canonicalName: String = null // TODO Is this necessary?
  var canonicalMention: Mention = null // TODO Is this necessary?
  // If number, gender and entity type are needed, put a CategoricalVariable subclass in the Attr
}



/** Container for a within-document coreference solution, typically stored as an attr of the Document.
    Some may contain imperfect an inferred coref solution; others may store a gold-standard target coref solution.
    Concrete instances of Mention and WithinDocEntity are created here.
    @author Andrew McCallum
    */
class WithinDocCoref(val document:Document) extends EvaluatableClustering[WithinDocEntity,Phrase] {
  /** When we have labeled gold-standard truth for coref, it is stored here. */
  var target: WithinDocCoref = null // ...the alternative would have been to create different subclasses of WithinDocCoref so they could be stored separately in the Document.attr, but I chose this as cleaner. -akm
  /** A mapping from (the Phrase's span value) to Mention */
  private val _spanToMention = new scala.collection.mutable.LinkedHashMap[Span[Section,Token],Mention]
  //private val _phraseToMention = new scala.collection.mutable.LinkedHashMap[Phrase,Mention] // Used to index by this instead.  I think we can remove this now. -akm
  /** A mapping from entity.uniqueId to WithinDocEntity */
  private val _entities = new scala.collection.mutable.LinkedHashMap[String,WithinDocEntity]
  /** A mapping from entity key (i.e. an Int identifying the true entity) to the entity.uniqueId */
  private lazy val _entityKeyToId = new scala.collection.mutable.HashMap[Int,String]
  private var _entityCount = 0 // The number of WithinDocEntities ever created here
  /** A string that will be used as a prefix on the uniqueIds of the Mentions and WithinDocEntities created here. */
  def uniqueId: String = document.uniqueId // TODO Perhaps this should be something more safely unique if we save more than one WithinDocCoref objects per Document? -akm 
  /** Concrete implementation of WithinDocEntity that automatically stores itself in WithinDocCoref.entities. */
  protected class WithinDocEntity1(val uniqueId:String) extends WithinDocEntity(document) {
    def this() = this(WithinDocCoref.this.uniqueId + "//WithinDocEntity" + _entityCount) // TODO Is this what we want? -akm
    _entityCount += 1
    _entities(uniqueId) = this
    def coref: WithinDocCoref = WithinDocCoref.this
  }
  /** Concrete implementation of Mention that automatically stores itself in WithinDocCoref.mentions. */
  protected class Mention1(phrase:Phrase, entity:WithinDocEntity) extends Mention(phrase) {
    def this(phrase:Phrase, entityKey:Int) = this(phrase, entityFromKey(entityKey)) // Typically used for labeled data
    def this(phrase:Phrase, entityUniqueId:String) = this(phrase, entityFromUniqueId(entityUniqueId)) // Typically used for deserialization
    def this(phrase:Phrase) = this(phrase, null.asInstanceOf[WithinDocEntity]) // Typically used for new inference // TODO Should this be null, or a newly created blank Entity; See LoadConll2011 also.
    assert(entity == null || entity.asInstanceOf[WithinDocEntity1].coref == WithinDocCoref.this)
    _spanToMention(phrase.value) = this
    val uniqueId = WithinDocCoref.this.uniqueId + "//Mention(" + phrase.start + "," + phrase.length + ")" // TODO Is this what we want? -akm
    if (entity ne null) entity += this
    def coref: WithinDocCoref = WithinDocCoref.this
  }
  
  /** Given Span (typically the value of a Phrase), return the corresponding Mention.
      Note that Span is a case class, so the lookup is done by the span's boundaries, not by its identity. */
  def mention(span:Span[Section,Token]): Mention = _spanToMention(span)
  /** Return the Mention corresponding to the given Phrase.  If none present, return null.
      Note that since the lookup happens by the Phrase's Span value, the returned mention.phrase may be different than this method's argument. */
  def mention(phrase:Phrase): Mention = _spanToMention(phrase.value)
  
  /** Create a new Mention whose entity will be null. */
  def addMention(phrase:Phrase): Mention = _spanToMention.getOrElse(phrase.value, new Mention1(phrase))
  /** Create a new Mention with entity specified by given uniqueId. */
  def addMention(phrase:Phrase, entityId:String): Mention = { assert(!_spanToMention.contains(phrase.value)); new Mention1(phrase, entityId) }
  /** Create a new Mention with entity specified by given key. */
  def addMention(phrase:Phrase, entityKey:Int): Mention = { assert(!_spanToMention.contains(phrase.value)); new Mention1(phrase, entityKey) }
  /** Create a new Mention with the given entity, which must also be in this WithinDocCoref */
  def addMention(phrase:Phrase, entity:WithinDocEntity): Mention = new Mention1(phrase, entity)
  
  /** Remove a Mention from this coreference solution, and from its entity if it has one. */
  def deleteMention(mention:Mention): Unit = {
    if (mention.entity ne null) mention.entity -= mention
    _spanToMention.remove(mention.phrase.value)
  }
  
  /** Return all Mentions in this coreference solution. */
  def mentions: Seq[Mention] = _spanToMention.values.toVector
  /** Return a collection of WithinDocEntities managed by this coref solution.  Note that some of them may have no Mentions. */
  def entities: Iterable[WithinDocEntity] = _entities.values
  /** Create and return a new WithinDocEntity with uniqueId determined by the number entities created so far. */
  def newEntity(): WithinDocEntity = new WithinDocEntity1()
  /** Return the entity associated with the given uniqueId, or create a new entity if not found already among 'entities'. */
  def entityFromUniqueId(id:String): WithinDocEntity = _entities.getOrElse(id, new WithinDocEntity1(id))
  /** Return the entity associated with the given key, or create a new entity if not found alread among 'entities'. */
  def entityFromKey(key:Int): WithinDocEntity = { 
    val id = _entityKeyToId.getOrElse(key,null)
    val result = if (id eq null) new WithinDocEntity1 else _entities(id)
    _entityKeyToId(key) = result.uniqueId
    result
  }
  /** Return the entity associated with the given uniqueId.  Return null if not found. */
  def idToEntity(id:String): WithinDocEntity = _entities(id)
  /** Remove from the list of entities all entities that contain no mentions. */
  def trimEmptyEntities(): Unit = _entities.values.filter(_.mentions.size == 0).map(_.uniqueId).foreach(_entities.remove) // TODO But note that this doesn't purge _entityKeyToId; perhaps it should.
  /** Remove from all entities and mentions associated with entities that contain only one mention. */
  def removeSingletons():Unit ={
    _entities.values.filter(_.mentions.size == 1).map(_.uniqueId).foreach{
      id =>
        _entities(id).mentions.foreach(m => deleteMention(m))
        _entities.remove(id)
    }
  }

  /**Reset the clustered entities for this coref solution without losing mentions and their cached properties*/
  def resetPredictedMapping():Unit = {_entities.clear();mentions.foreach(_._setEntity(null));_entityCount = 0 }

  // Support for evaluation
  // These assure we ignore any singletons for conll scoring
  // TODO: Allow for ACE scoring where singletons are counted
  def clusterIds: Iterable[WithinDocEntity] = _entities.values.filterNot(_.isSingleton)
  def pointIds: Iterable[Phrase] = _spanToMention.values.filterNot(m => m.entity == null || m.entity.isSingleton).map(_.phrase)
  def pointIds(entityId:WithinDocEntity): Iterable[Phrase] = if(!entityId.isSingleton) entityId.mentions.map(_.phrase) else Seq()
  def intersectionSize(entityId1:WithinDocEntity, entityId2:WithinDocEntity): Int = if(!entityId1.isSingleton && !entityId2.isSingleton) entityId1.mentions.map(_.phrase.value).intersect(entityId2.mentions.map(_.phrase.value)).size else 0
  def clusterId(mentionId:Phrase): WithinDocEntity = {
    val mention = _spanToMention.getOrElse(mentionId.value,null)
    if(mention == null || mention.entity == null ||mention.entity.isSingleton) null
    else mention.entity
  }


}


// CrossDocEntity should be unified with Jack's new hcoref replacement.
// ids, including cross-doc ids will be part of this work.
trait CrossDocMention extends AbstractMention {
  def withinDocEntityId: String
}
trait CrossDocEntity extends AbstractEntity // ...



///** Categorical variable indicating whether the mention is a pronoun, nominal or named proper noun.
//    (Obviously different from MentionEntityType, which may indicate whether it is a person, location, organization, etc.) */
//class MentionType(val mention:AbstractMention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
//  def domain = OntonotesMentionTypeDomain
//}
///** The domain of MentionType, consisting of pronouns (PRO), nominals (NOM) and named proper nouns (NAM). */
//object OntonotesMentionTypeDomain extends CategoricalDomain(List("PRO", "NOM", "NAM"))


// // In case we need to put labels on Mentions or Entities in addition to their underlying Phrases. -akm
//class OntonotesEntityType(category:String) extends LabeledCategoricalVariable[String](category) {
//  def domain = OntonotesEntityTypeDomain
//}
//
//class PhraseOntonotesEntityType(val phrase:Phrase, value:String) extends OntonotesEntityType(value)
//class EntityOntonotesEntityType(val entity:AbstractEntity, value:String) extends OntonotesEntityType(value)
//class WithinDocEntityOntonotesEntityType(override val entity:WithinDocEntity, value:String) extends EntityOntonotesEntityType(entity, value)
//
//class EntityGender(val entity:AbstractEntity, value:String) extends Gender(value)
//class WithinDocEntityGender(override val entity:WithinDocEntity, value:String) extends EntityGender(entity, value)
////class CrossDocEntityGender(override val entity:CrossDocEntity, value:String) extends EntityGender(entity, value)
//
//class EntityNumber(val entity:AbstractEntity, value:String) extends Number(value)
//class WithinDocEntityNumber(override val entity:WithinDocEntity, value:String) extends EntityNumber(entity, value)

