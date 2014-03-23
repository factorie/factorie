package cc.factorie.app.nlp.coref

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.util.{Attr,UniqueId,SubclassableVector}
import cc.factorie.variable._
import scala.collection.mutable.ArrayBuffer

/** Either an entity or a mention */
case class NodeId(string:String)
trait Node extends UniqueId[NodeId] with Attr {
  def parent: Node
  // ...
  //def parentId: String
}

class MentionId(string:String) extends NodeId(string)
trait Mention extends Node with UniqueId[MentionId] {
  val uniqueId: MentionId = ??? 
  def parent: Entity
  def entity: Entity
  def string: String
}

class MentionList(mentions:Iterable[Mention]) extends IndexedSeq[Mention] {
  protected val _mentions = mentions.toVector
  val length = _mentions.size
  def apply(index:Int): Mention = _mentions(index)
}
class PhraseMentionList(mentions:Iterable[PhraseMention]) extends MentionList(mentions) with IndexedSeq[PhraseMention] {
  override protected val _mentions = mentions.toVector
  override def apply(index:Int): PhraseMention = _mentions(index)
}

class EntityId(string:String) extends NodeId(string)
trait Entity extends Node with UniqueId[EntityId] {
  val uniqueId: EntityId = ??? 
  def parent: Entity
  def children: Iterable[Node]  // Immediate children
  def childIds: Iterable[NodeId] = ???
  def mentions: Iterable[Mention] // Leaves of tree
}

class OntonotesEntityType(category:String) extends LabeledCategoricalVariable[String](category) {
  def domain = OntonotesEntityTypeDomain
}
class PhraseOntonotesEntityType(val phrase:Phrase, value:String) extends OntonotesEntityType(value)
class EntityOntonotesEntityType(val entity:Entity, value:String) extends OntonotesEntityType(value)
class WithinDocEntityOntonotesEntityType(override val entity:WithinDocEntity, value:String) extends EntityOntonotesEntityType(entity, value)

class Gender extends CategoricalVariable[String] {
  def this(value:String) = { this(); _initialize(domain.index(value)) }
  def this(value:Int) = { this(); _initialize(value) }
  def domain = GenderDomain  
}
class PhraseGender(val phrase:Phrase, value:String) extends Gender(value)
class EntityGender(val entity:Entity, value:String) extends Gender(value)
class WithinDocEntityGender(override val entity:WithinDocEntity, value:String) extends EntityGender(entity, value)
//class CrossDocEntityGender(override val entity:CrossDocEntity, value:String) extends EntityGender(entity, value)

class EntityNumber(val entity:Entity, value:String) extends Number(value)
class WithinDocEntityNumber(override val entity:WithinDocEntity, value:String) extends EntityNumber(entity, value)

// Once the current "Mention" class is removed, we could consider a trait
// generic to mentions that are not necessarily Phrases in Documents.
// trait Mention { def string: String; ...and what else?... }
// Perhaps some simple coref inference could be based on these.
// Then PhraseMention would extend Mention.

/** A mention whose contents from from a nlp.phrase.Phrase.
    This should replace app.nlp.coref.mention.Mention. */
class PhraseMention(val phrase:Phrase) extends Mention {
  private var _entity:WithinDocEntity = null
  protected[coref] def _setEntity(e:WithinDocEntity): Unit = _entity = e
  def entity: WithinDocEntity = _entity
  def parent: WithinDocEntity = _entity
  def string = phrase.string
  // If number, gender and entity type are needed, put a CategoricalVariable subclass in the Attr
}


/** An entity whose evidence comes from within a single document.
    In other words a "within-document entity" (but DocEntity is easier to pronounce). */
class WithinDocEntity(val document:Document) extends Entity {
  private val _mentions = new scala.collection.mutable.LinkedHashSet[PhraseMention]
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

