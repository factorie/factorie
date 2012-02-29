/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
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
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}
import cc.factorie.util.{Cubbie,CubbieRefs}
import cc.factorie.util.Attr

/** A pointer from a child Entity (or Mention) to its parent Entity in an coreference problem. */
class EntityRef(theSrc:Entity, initialDst:Entity) extends ArrowVariable(theSrc, initialDst) {
  if (dst ne null) dst._addSubEntity(src)(null)
  override def set(e:Entity)(implicit d:DiffList): Unit = {
    if (e ne dst) {
      if (dst ne null) dst._removeSubEntity(src)
      e._addSubEntity(src)
      super.set(src, e)
    }
  }
  final def mention = src
  final def entity = dst
}

/** A trait for entities (and mentions, sub-entities and super-entities) in a coreference problem.
    Abstract classes here are string, superEntityRef, subEntities, _addSubEntity and _removeSubEntity. */
trait Entity extends Attr {
  @deprecated("Will be removed.  Entities are not guaranteed to have string names.") def string: String
  def id: Any = this // Override to make some persistent id
  /** Ensure that we return a non-null value. */
  private def _ensuredSubEntities: SubEntities = {
    var result = subEntities
    if (result eq null) {
      result = new SubEntities(this)
      attr += result
    }
    result
  } 
  def subEntities: SubEntities = attr[SubEntities]
  // The following two methods will work even if subEntities is null
  def subEntitiesSize: Int = { val se = subEntities; if (se eq null) 0 else se.size }
  def subEntitiesIterator: Iterator[Entity] = { val se = subEntities; if (se eq null) Iterator.empty else se.iterator }
  // Next two methods should only be called in EntityRef
  def _addSubEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredSubEntities.add(e)
  def _removeSubEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredSubEntities.remove(e)
  def superEntityRef: EntityRef = attr[EntityRef]
  def superEntity: Entity = { val ref = superEntityRef; if (ref eq null) null else superEntityRef.dst }
  def superEntityOption: Option[Entity] = { val ref = superEntityRef; if (ref eq null) None else if (ref.value eq null) None else Some(ref.dst) }
  final def setSuperEntity(e:Entity)(implicit d:DiffList): Unit = superEntityRef.set(e) // Just a convenient alias
  def exists: Boolean = superEntityRef.value != null || subEntitiesSize > 0
  def entityRoot: Entity = { val s = superEntity; if (s eq null) this else this.entityRoot }
  
  /** Recursively descend sub-entities to return only the Mentions */
  def mentions: Seq[Mention] = {
    var result = new ListBuffer[Mention]
    for (entity <- subEntitiesIterator) {
      entity match {
        case mention:Mention => result += mention
        case _ => {}
      }
      result ++= entity.mentions
    }
    result
  }
  def mentionsOfClass[A<:Mention](cls:Class[A]): Seq[A] = {
    var result = new ListBuffer[A]
    for (entity <- subEntitiesIterator) {
      if (cls.isAssignableFrom(entity.getClass))
        result += entity.asInstanceOf[A]
      result ++= entity.mentionsOfClass[A](cls)
    }
    result
  }
  def mentionsOfClass[A<:Mention](implicit m:Manifest[A]): Seq[A] = mentionsOfClass[A](m.erasure.asInstanceOf[Class[A]])
}

/** This variable should not be changed directly.  Change EntityRef variables, and they will automatically coordinate with SubEntities variables. */
class SubEntities(val entity:Entity) extends SetVariable[Entity]

abstract class EntityCubbie extends Cubbie {
  val entityRef = RefSlot("entityRef", () => newEntityCubbie)
  def newEntity: Entity
  def newEntityCubbie: EntityCubbie
  def storeEntity(e:Entity): this.type = {
    entityRef := e.superEntity.id
    finishStoreEntity(e)
    this
  }
  def finishStoreEntity(e:Entity): Unit = {}
  def fetchEntity(cr:CubbieRefs): Entity = {
    val e = newEntity
    e.setSuperEntity(cr(entityRef.value).asInstanceOf[Entity])(null)
    finishFetchEntity(e)
    e
  }
  def finishFetchEntity(e:Entity): Unit = {}
}




trait Mention extends Entity {
  // Just aliases for nicer names
  final def entity: Entity = superEntity
  final def entityRef: EntityRef = superEntityRef
  final def setEntity(e:Entity)(implicit d:DiffList): Unit = superEntityRef.set(e)
}


trait TokenSpanMention extends TokenSpan with Mention
abstract class TokenSpanMentionCubbie extends TokenSpanCubbie {
  // Unfortunately we can't inherit from both TokenSpanCubbie and EntityCubbie
  val entityRef = RefSlot("entityRef", () => newEntityCubbie)
  def newEntityCubbie: TokenSpanMentionCubbie
  def newTokenSpanMention(doc:Document, start:Int, length:Int): TokenSpanMention
  def storeTokenSpanMention(tsm:TokenSpan with Mention): this.type = {
    storeTokenSpan(tsm)
    entityRef := tsm.superEntity.id
    this
  }
  def fetchTokenSpanMention(doc:Document): TokenSpanMention = {
    val tsm = newTokenSpanMention(doc, start.value, length.value)
    throw new Error("Not yet implemented")
    tsm.entity.setSuperEntity(null)(null)
    finishFetchTokenSpan(tsm)
    tsm
  }
}

class StringEntity(s:String) extends EntityWithName {
  val name = new EntityName(this, s)
  @deprecated("Use name.value instead") def string = name.value  
}
class StringMention(s:String) extends StringEntity(s) with Mention
trait EntityWithName extends Entity {
  def name: EntityName
}
class EntityName(val entity:Entity, s:String) extends StringVariable(s)
@deprecated("Use StringEntity instead.") class EntityVariable(s:String) extends StringEntity(s) 


class StringEntityCubbie extends Cubbie {
  val name = StringSlot("name")
  val entityRef = RefSlot("entityRef", () => new StringEntityCubbie)
  def newObject(s:String): StringMention = new StringMention(s) 
  def store(sm:StringMention): this.type = {
    name := sm.name.value
    entityRef := sm.entity.id
    this
  }
  def fetch(cr:CubbieRefs): StringMention = {
    val sm = newObject(name.value)
    sm.setSuperEntity(cr(entityRef.value).asInstanceOf[Entity])(null)
    sm
  }
}




/*class EntityVariable(var initialCanonicalString:String) extends SetVariable[Entity] with Entity {
  def this() = this("")
  attr += new EntityName(this, initialCanonicalString)
  def name = attr[EntityName] 
  def string = name.value
  @deprecated("Use name instead") def canonical = name
  @deprecated("Use name instead") def canonicalString: String = canonical.value
  //def mentions: scala.collection.Set[Entity] = value
  val superEntityRef = new EntityRef(this, null)
  def subEntities = value
  def _addSubEntity(e:Entity)(implicit d:DiffList): Unit = add(e)
  def _removeSubEntity(e:Entity)(implicit d:DiffList): Unit = remove(e)

  //def closestPreviousMention(position:Int): Entity = mentions.filter(_.start < position).toSeq.sortBy(- _.start).head
  override def toString = "Entity("+string+":"+mentions.toSeq.size+")"
}


class EntityVariableCubbie extends Cubbie {
  val string = StringSlot("string")
  val entityRef = RefSlot("entityRef", () => new EntityVariableCubbie)
  def store(ev:EntityVariable): this.type = {
    string := ev.canonicalString
    entityRef := ev.superEntity.id
    this
  }
  def fetch(cr:CubbieRefs): EntityVariable = {
    val ev = new EntityVariable(string.value)
    ev.setSuperEntity(cr(entityRef.value).asInstanceOf[EntityVariable])(null)
    ev
  }
}
*/

