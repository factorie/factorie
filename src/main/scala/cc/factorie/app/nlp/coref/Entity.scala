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
      val old = dst
      if (dst ne null) {
        dst._removeSubEntity(src)
        dst.removedChildHooks(src,d)
      }
      e._addSubEntity(src)
      e.addedChildHooks(src,d)
      super.set(e)
      src.changedSuperEntityHooks(old,e,d)
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
  attr+=new EntityRef(this,null)
  val removedChildHooks = new cc.factorie.util.Hooks2[Entity,DiffList]
  val addedChildHooks = new cc.factorie.util.Hooks2[Entity,DiffList]
  val changedSuperEntityHooks = new cc.factorie.util.Hooks3[Entity,Entity,DiffList]

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

  //def removedChildHook(removed:Entity,d:DiffList):Unit = {}
  //def addedChildHook(added:Entity,d:DiffList):Unit = {}
  //def changedSuperEntityHook(oldSuper:Entity,newSuper:Entity,d:DiffList):Unit = {}


  
/*
  //todo: rename connected or something more literal
  def exists: Boolean = (superEntityRef != null && superEntityRef.dst != null) || subEntitiesSize > 0
  def entityRoot: Entity = if (isEntity) this else superEntity.entityRoot
  def isEntity:Boolean = (superEntityRef == null || superEntityRef.dst == null)
  def isMention:Boolean = this.isInstanceOf[Mention]
//  def entityRoot: Entity = { val s = superEntity; if (s eq null) this else s.entityRoot }
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
  */
  @deprecated("Use isConnected instead") def exists: Boolean = superEntityRef.value != null || subEntitiesSize > 0
  def isConnected: Boolean = (superEntity ne null) || subEntitiesSize > 0 
  def entityRoot: Entity = { val s = superEntity; if (s eq null) this else this.entityRoot }
  def isRoot:Boolean = (superEntityRef == null || superEntityRef.dst == null)
  def isLeaf:Boolean = subEntitiesSize==0
  var isObserved:Boolean = false
  var treatAsObserved:Boolean=false

  /** Recursively descend sub-entities and return only those matching criterion */
  def filterDescendants(test:Entity=>Boolean): Seq[Entity] = subEntitiesIterator.filter(test).toSeq
  def descendantsOfClass[A<:Entity](cls:Class[A]): Seq[A] = {
    var result = new ListBuffer[A]
    for (entity <- subEntitiesIterator) {
      if (cls.isAssignableFrom(entity.getClass))
        result += entity.asInstanceOf[A]
      result ++= entity.descendantsOfClass[A](cls)
    }
    result
  }
  /*
  def mentionsOfClass[A<:Mention](implicit m:Manifest[A]): Seq[A] = mentionsOfClass[A](m.erasure.asInstanceOf[Class[A]])
  def removeChildTrigger(removed:Entity)(implicit d:DiffList):Unit ={}
  def addChildTrigger(added:Entity)(implicit d:DiffList):Unit = {}
  def changeSuperEntityRef(oldValue:Entity,newEntity:Entity)(implicit d:DiffList):Unit ={}
*/
  def descendantsOfClass[A<:Entity](implicit m:Manifest[A]): Seq[A] = descendantsOfClass[A](m.erasure.asInstanceOf[Class[A]])
}

/** This variable should not be changed directly.  Change EntityRef variables, and they will automatically coordinate with SubEntities variables. */
class SubEntities(val entity:Entity) extends SetVariable[Entity]



// Cubbie storage
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

