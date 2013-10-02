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

package cc.factorie.app.nlp.hcoref
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer,LinkedList}
import cc.factorie.util.{Cubbie,CubbieRefs}
import cc.factorie.util.Attr
import annotation.tailrec
import cc.factorie.variable.{Var, SetVariable, ArrowVariable, DiffList}
import scala.reflect.ClassTag

/** A pointer from a child Entity (or Mention) to its parent Entity in an coreference problem. */
class EntityRef(theSrc:Entity, initialDst:Entity) extends ArrowVariable(theSrc, initialDst) {
  if (dst ne null) dst._addChildEntity(src)(null)
  override def set(e:Entity)(implicit d:DiffList): Unit = {
    if (e ne dst) {
      val old = dst
      if (dst ne null) {
        dst._removeChildEntity(src)
        dst.removedChildHook(src)
      }
      super.set(e)
      src.changedParentEntityHook(old,e)
      if(e!=null){
        e._addChildEntity(src)
        e.addedChildHook(src)
      }
    }
  }
  final def mention = src
  final def entity = dst
}




/** A trait for entities (and mentions, sub-entities and parent-entities) in a coreference problem.
    Abstract classes here are string, parentEntityRef, childEntities, _addChildEntity and _removeChildEntity. */
trait Entity extends Attr {
  @deprecated("Will be removed.  Entities are not guaranteed to have string names.") def string: String
  def id: Any = this // Override to make some persistent id
  attr += new EntityRef(this,null)
  def initializeAttributesOfStructure():Unit = {}
  def removedChildHook(entity:Entity)(implicit d:DiffList)={}
  def addedChildHook(entity:Entity)(implicit d:DiffList)={}
  def changedParentEntityHook(oldEntity:Entity,newEntity:Entity)(implicit d:DiffList)={}
  /** Ensure that we return a non-null value. */
  private def _ensuredChildEntities: ChildEntities = {
    var result = childEntities
    if (result eq null) {
      result = new ChildEntities(this)
      attr += result
    }
    result
  }
  def childEntities: ChildEntities = attr[ChildEntities]
  // The following two methods will work even if childEntities is null
  def childEntitiesSize: Int = { val se = childEntities; if (se eq null) 0 else se.size }
  def childEntitiesIterator: Iterator[Entity] = { val se = childEntities; if (se eq null) Iterator.empty else se.iterator }
  //the other iterator will break if you call setParentEntity(someEntity)(d) on a child while you are iterating over it, a common use case in MCMC.
  def safeChildEntitiesSeq: Seq[Entity]={val r = new ArrayBuffer[Entity];for(e<-childEntitiesIterator)r += e;r}
  // Next two methods should only be called in EntityRef
  def _addChildEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredChildEntities.add(e)
  def _removeChildEntity(e:Entity)(implicit d:DiffList): Unit = _ensuredChildEntities.remove(e)
  def parentEntityRef: EntityRef = attr[EntityRef]
  def parentEntity: Entity = { val ref = parentEntityRef; if (ref eq null) null else parentEntityRef.dst }
  def parentEntityOption: Option[Entity] = { val ref = parentEntityRef; if (ref eq null) None else if (ref.value eq null) None else Some(ref.dst) }
  final def setParentEntity(e:Entity)(implicit d:DiffList): Unit = parentEntityRef.set(e) // Just a convenient alias
  def isConnected: Boolean = (parentEntity ne null) || childEntitiesSize > 0 || isObserved
  //def entityRoot: Entity = { val s = parentEntity; if (s eq null) this else this.entityRoot }©
  def entityRoot: Entity = if (isRoot) this else parentEntity.entityRoot
  def isRoot:Boolean = parentEntityRef == null || parentEntityRef.dst == null
  def isLeaf:Boolean = childEntitiesSize==0
  var isObserved:Boolean = false
  //var treatAsObserved:Boolean=false
  /** Recursively descend sub-entities and return only those matching criterion */
  def filterDescendants(test:Entity=>Boolean): Seq[Entity] = childEntitiesIterator.filter(test).toSeq
  def descendantsOfClass[A<:Entity](cls:Class[A]): Seq[A] = {
    var result = new ListBuffer[A]
    result += this.asInstanceOf[A]
    for (entity <-childEntitiesIterator) {
      //if (cls.isAssignableFrom(entity.getClass))
      //  result += entity.asInstanceOf[A]
      result ++= entity.descendantsOfClass[A](cls)
    }
    result
  }
  def numLeaves:Int = {
    var result = 0
    incIfLeaf(this)
    def incIfLeaf(e:Entity):Unit ={
      if(e.isLeaf)result += 1
      for(c <- e.childEntitiesIterator)incIfLeaf(c)
    }
    result
  }

  /*
  def descendantsOfClass[A<:Entity](cls:Class[A],result:ListBuffer[A] = new ListBuffer[A]): Seq[A] = {
    if(cls.isAssignableFrom(entityRoot.getClass))result += this.asInstanceOf[A]
    for (entity <-childEntitiesIterator) {
      result ++= entity.descendantsOfClass[A](cls,result)
    }
    result
  }
  */
  def descendantsOfClass[A<:Entity](implicit m:ClassTag[A]): Seq[A] = descendantsOfClass[A](m.runtimeClass.asInstanceOf[Class[A]])
  final def depth:Int = if(parentEntity eq null)0 else 1 + parentEntity.depth
  //def depth:Int = depth(0)
  //def depth(d:Int):Int = if(parentEntity==null)0 else parentEntity.depth(d+1)
  def getAncestor(numGenerationsBack:Int):Entity = if(numGenerationsBack==0)this else getAncestor(numGenerationsBack - 1)
}

/** This variable should not be changed directly.  Change EntityRef variables, and they will automatically coordinate with ChildEntities variables. */
class ChildEntities(val entity:Entity) extends SetVariable[Entity]

/**An attribute that knows what entity it belongs to*/
trait EntityAttr extends Var {
  def entity: Entity
}

// Cubbie storage
abstract class EntityCubbie extends Cubbie {
  val entityRef = RefSlot("entityRef", () => newEntityCubbie)
  def newEntity: Entity
  def newEntityCubbie: EntityCubbie
  def storeEntity(e:Entity): this.type = {
    if(e.parentEntity!=null)entityRef := e.parentEntity.id
    finishStoreEntity(e)
    this
  }
  def finishStoreEntity(e:Entity): Unit = {}
  def fetchEntity(cr:CubbieRefs=null): Entity = {
    val e = newEntity
    if(cr!=null && entityRef.cubbie._map.contains(entityRef.name))
      e.setParentEntity(cr(entityRef.value).asInstanceOf[Entity])(null)
    finishFetchEntity(e)
    e
  }
  def finishFetchEntity(e:Entity): Unit = {}
}

