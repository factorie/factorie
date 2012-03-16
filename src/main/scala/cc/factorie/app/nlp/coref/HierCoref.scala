package cc.factorie.app.nlp.coref
import cc.factorie._
import collection.mutable.{HashSet, ArrayBuffer}

class EntityExists(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
class IsEntity(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
class IsMention(val entity:Entity,initialValue:Boolean) extends BooleanVariable(initialValue)
class Dirty(val entity:Entity) extends IntegerVariable(0){def reset()(implicit d:DiffList):Unit=this.set(0)(d);def ++()(implicit d:DiffList):Unit=this.set(intValue+1)(d)} //convenient for determining whether an entity needs its attributes recomputed
abstract class HierEntity(isMent:Boolean=false) extends Entity{
  isObserved=isMent
  def isEntity = attr[IsEntity]
  def isMention = attr[IsMention]
  def exists = attr[EntityExists]
  def dirty = attr[Dirty]
  attr += new EntityExists(this,this.isConnected)
  attr += new IsEntity(this,this.isRoot)
  attr += new IsMention(this,this.isObserved)
  attr += new Dirty(this)
  override def removedChildHook(entity:Entity)(implicit d:DiffList)={super.removedChildHook(entity);exists.set(this.isConnected)(d);dirty++}
  override def addedChildHook(entity:Entity)(implicit d:DiffList)={super.addedChildHook(entity);exists.set(this.isConnected)(d);dirty++}
  override def changedSuperEntityHook(oldEntity:Entity,newEntity:Entity)(implicit d:DiffList){super.changedSuperEntityHook(oldEntity,newEntity);isEntity.set(this.isRoot)(d);exists.set(this.isConnected)(d)}
}

abstract class HierEntityCubbie extends EntityCubbie{
  val isMention = BooleanSlot("isMention")
  override def finishFetchEntity(e:Entity):Unit ={
    e.attr[IsMention].set(isMention.value)(null)
    e.isObserved=isMention.value
    e.attr[IsEntity].set(e.isRoot)(null)
    e.attr[EntityExists].set(e.isConnected)(null)
  }
  override def finishStoreEntity(e:Entity):Unit ={
    isMention := e.attr[IsMention].booleanValue
  }
}

abstract class HierCorefSampler[T<:HierEntity](model:TemplateModel) extends SettingsSampler[Null](model, null) {
  def newEntity:T
  //def reestimateAttributes(e:T):Unit 
  protected var entities:ArrayBuffer[T] = null
  protected var deletedEntities:ArrayBuffer[T] = null
  def getEntities = entities.filter(_.isConnected)
  def getDeletedEntities = {
    //performMaintenance(entities)
    //deletedEntities
    null
  }
  def infer(numSamples:Int):Unit ={}
  /**Returns a random entity that 'exists'*/
  //def nextEntity:T = nextEntity(null.asInstanceOf[T])
  def nextEntity:T=nextEntity(null.asInstanceOf[T])
  def nextEntity(context:T):T=sampleEntity(entities)
  def sampleAttributes(e:T)(implicit d:DiffList):Unit //= {e.dirty.reset}
  protected def sampleEntity(samplePool:ArrayBuffer[T]) = {
    var tries = 4
    var e:T = null.asInstanceOf[T]
    while({tries-= 1;tries} >= 0 && (e==null || !e.isConnected)){e = samplePool(random.nextInt(samplePool.size));if(tries==1)performMaintenance(samplePool)}
    if(!e.isConnected)throw new Exception("NOT CONNECTED")
    e
  }
  def setEntities(ents:Iterable[T]) = {entities = new ArrayBuffer[T];for(e<-ents)addEntity(e);deletedEntities = new ArrayBuffer[T]}
  /**Garbage collects all the deleted entities from the master list of entities*/
  def performMaintenance(es:ArrayBuffer[T]):Unit ={
    //println("Performing maintenance")
    var oldSize = es.size
    val cleanEntities = new ArrayBuffer[T]
    cleanEntities ++= es.filter(_.isConnected)
    deletedEntities ++= es.filter(!_.isConnected)
    es.clear
    es++=cleanEntities
   // println("  removed "+(oldSize-es.size)+ " disconnected entities.")
  }
  /**This function randomly generates a list of jumps/proposals to choose from.*/
  def settings(c:Null) : SettingIterator = new SettingIterator {
    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit];
    val entity1 = nextEntity
    val entity2 = nextEntity(entity1)
    if (entity1.entityRoot.id != entity2.entityRoot.id) { //sampled nodes refer to different entities
      if(!isMention(entity1)){
        changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)} //what if entity2 is a mention?
        if(entity1.id != entity1.entityRoot.id) //avoid adding the same jump to the list twice
          changes += {(d:DiffList) => mergeLeft(entity1.entityRoot.asInstanceOf[T],entity2)(d)} //unfortunately casting is necessary unless we want to type entityRef/superEntity/subEntity
      }
      if(entity1.superEntity==null && entity2.superEntity==null)
        changes += {(d:DiffList) => mergeUp(entity1,entity2)(d)}
    } else { //sampled nodes refer to same entity
      changes += {(d:DiffList) => splitRight(entity1,entity2)(d)}
      changes += {(d:DiffList) => splitRight(entity2,entity1)(d)}
      if(entity1.superEntity != null && !entity1.isObserved)
        changes += {(d:DiffList) => {collapse(entity1)(d)}}
    }
    if(entity1.dirty.value>0)changes += {(d:DiffList) => sampleAttributes(entity1)(d)}
    if(entity1.entityRoot.id != entity1.id && entity1.entityRoot.attr[Dirty].value>0)changes += {(d:DiffList) => sampleAttributes(entity1.entityRoot.asInstanceOf[T])(d)}

    changes += {(d:DiffList) => {}} //give the sampler an option to reject all other proposals
    var i = 0
    def hasNext = i < changes.length
    def next(d:DiffList) = { val d = new DiffList; changes(i).apply(d); i += 1; d }
    def reset = i = 0
  }
  /**Removes an intermediate node in the tree, merging that nodes children to their grandparent.*/
  def collapse(entity:T)(implicit d:DiffList):Unit ={
    if(entity.superEntity==null)throw new Exception("Can't collapse a node that is the root of a tree.")
    val oldParent = entity.superEntity
    entity.subEntitiesIterator.foreach(_.setSuperEntity(entity.superEntity)(d))
    entity.setSuperEntity(null)(d)
  }
  /**Peels off the entity "right", does not really need both arguments unless we want to error check.*/
  def splitRight(left:T,right:T)(implicit d:DiffList):Unit ={
    val oldParent = right.superEntity
    right.setSuperEntity(null)(d)
    structurePreservationForEntityThatLostSubEntity(oldParent)(d)
  }
  /**Jump function that proposes merge: entity1<----entity2*/
  def mergeLeft(entity1:T,entity2:T)(implicit d:DiffList):Unit ={
    val oldParent = entity2.superEntity
    entity2.setSuperEntity(entity1)(d)
    structurePreservationForEntityThatLostSubEntity(oldParent)(d)
  }
  /**Jump function that proposes merge: entity1--->NEW-SUPER-ENTITY<---entity2 */
  def mergeUp(e1:T,e2:T)(implicit d:DiffList):T = {
    val oldParent1 = e1.superEntity
    val oldParent2 = e2.superEntity
    val result = newEntity
    e1.setSuperEntity(result)(d)
    e2.setSuperEntity(result)(d)
    structurePreservationForEntityThatLostSubEntity(oldParent1)(d)
    structurePreservationForEntityThatLostSubEntity(oldParent2)(d)
    result
  }
  /**Ensure that chains are not created in our tree. No dangling sub-entities either.*/
  protected def structurePreservationForEntityThatLostSubEntity(e:Entity)(implicit d:DiffList):Unit ={
    if(e!=null && e.subEntitiesSize<=1){
      for(subEntity <- e.subEntities)
        subEntity.setSuperEntity(e.superEntity)
      e.setSuperEntity(null)(d)
    }
  }
  /**Identify entities that are created by accepted jumps so we can add them to our master entity list.*/
  override def proposalHook(proposal:Proposal) = {
    super.proposalHook(proposal)
    val newEntities = new HashSet[T]
    proposal.diff.undo //an entity that does not exit in the current world is one that was newly created by the jump
    for(diff<-proposal.diff){
      diff.variable match{
        case sub:SubEntities => if(!sub.entity.isConnected)newEntities += sub.entity.asInstanceOf[T] //cast could be avoided if sub entities were typed
        case _ => {}
      }
    }
    proposal.diff.redo
    for(entity<-newEntities)addEntity(entity)
  }
  def addEntity(e:T):Unit ={entities += e}
  def isMention(e:Entity):Boolean = e.isObserved
}
