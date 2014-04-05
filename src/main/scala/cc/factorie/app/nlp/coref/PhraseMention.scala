package cc.factorie.app.nlp.coref

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.util.{Attr,UniqueId,ImmutableArrayIndexedSeq}
import cc.factorie.variable._
import scala.collection.mutable.ArrayBuffer

/** Either an entity or a mention in an coreference or entity resolution model.
    @author Andrew McCallum */
trait Node extends UniqueId with Attr {
  type ParentType <: Node
  def parent: ParentType
  // ...
  //def parentId: String
}

/** A "mention" of an entity in a resolution problem.
    A leaf in a coreference hierarchy.
    This is the parent trait for mentions in both within-document coreference and cross-document entity resolution.
    @author Andrew McCallum */
trait AbstractMention extends Node with UniqueId {
  def parent: ParentType
  def entity: ParentType
  def string: String
}

trait AbstractEntity extends Node with UniqueId {
  def parent: ParentType
  def children: Iterable[Node]  // Immediate children
  def childIds: Iterable[String] = ???
  def mentions: Iterable[AbstractMention] // Leaves of tree
}


trait MentionCollection extends Iterable[PhraseMention]
class MentionList(mentions:Iterable[PhraseMention]) extends ImmutableArrayIndexedSeq(mentions) with MentionCollection
class MentionBuffer extends ArrayBuffer[PhraseMention] with MentionCollection




class OntonotesEntityType(category:String) extends LabeledCategoricalVariable[String](category) {
  def domain = OntonotesEntityTypeDomain
}
class PhraseOntonotesEntityType(val phrase:Phrase, value:String) extends OntonotesEntityType(value)
class EntityOntonotesEntityType(val entity:AbstractEntity, value:String) extends OntonotesEntityType(value)
class WithinDocEntityOntonotesEntityType(override val entity:WithinDocEntity, value:String) extends EntityOntonotesEntityType(entity, value)

class Gender extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = GenderDomain  
}
class PhraseGender(val phrase:Phrase, value:String) extends Gender(value)
class EntityGender(val entity:AbstractEntity, value:String) extends Gender(value)
class WithinDocEntityGender(override val entity:WithinDocEntity, value:String) extends EntityGender(entity, value)
//class CrossDocEntityGender(override val entity:CrossDocEntity, value:String) extends EntityGender(entity, value)

class EntityNumber(val entity:AbstractEntity, value:String) extends Number(value)
class WithinDocEntityNumber(override val entity:WithinDocEntity, value:String) extends EntityNumber(entity, value)

// Once the current "Mention" class is removed, we could consider a trait
// generic to mentions that are not necessarily Phrases in Documents.
// trait Mention { def string: String; ...and what else?... }
// Perhaps some simple coref inference could be based on these.
// Then PhraseMention would extend Mention.

/** A mention whose contents from from a nlp.phrase.Phrase.
    This should replace app.nlp.coref.mention.Mention. */
class PhraseMention(val phrase:Phrase) extends AbstractMention {
  type ParentType = WithinDocEntity
  val uniqueId = ???
  private var _entity:WithinDocEntity = null
  protected[coref] def _setEntity(e:WithinDocEntity): Unit = _entity = e
  def entity: ParentType = _entity
  def parent: ParentType = _entity
  def string = phrase.string
  // If number, gender and entity type are needed, put a CategoricalVariable subclass in the Attr
}


/** An entity whose evidence comes from within a single document.
    In other words a "within-document entity" (but DocEntity is easier to pronounce). */
class WithinDocEntity(val document:Document) extends AbstractEntity {
  type ParentType = WithinDocEntity
  private val _mentions = new scala.collection.mutable.LinkedHashSet[PhraseMention]
  val uniqueId = ???
  def parent: WithinDocEntity = null
  def mentions:Iterable[PhraseMention] = _mentions
  def children: Iterable[PhraseMention] = _mentions
  def +=(mention:PhraseMention): Unit = {
    assert(mention.phrase.document eq document)
    assert(!_mentions.contains(mention)) // No reason to do this; might catch a bug.
    if (mention.entity ne null) mention.entity._mentions -= mention
    _mentions += mention
    mention._setEntity(WithinDocEntity.this)
  }
  var canonicalName: String = null
  var canonicalMention: PhraseMention = null // TODO Is this necessary?
  def id: String = ???
  // If number, gender and entity type are needed, put a CategoricalVariable subclass in the Attr
}

class WithinDocEntities extends ArrayBuffer[WithinDocEntity] {
  
}

/** Inference should happen with respect to DocEntity directly.
    When done and evaluation (in terms of GenericEntityMap) is needed call this to get one. */
object WithinDocEntities {
  def toGenericEntityMap(entities:Iterable[WithinDocEntity]): GenericEntityMap[PhraseMention] = ???
}

// CrossDocEntity should be unified with Jack's new hcoref replacement.
// ids, including cross-doc ids will be part of this work.

