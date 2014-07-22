package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.{SettingsSampler, Proposal}
import scala.collection.mutable.{HashMap, ArrayBuffer, HashSet}
import cc.factorie._

/**
  * @author John Sullivan
 */
trait LegacyCanopyGenerator[Vars <: NodeVariables[Vars] with Canopy] extends ContextGenerator[Vars] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  val entMap = new HashMap[String, Node[Vars]]()
  protected var canopies = new HashMap[String,ArrayBuffer[Node[Vars]]]()
  protected var entities:ArrayBuffer[Node[Vars]] = ArrayBuffer[Node[Vars]]()
  protected var deletedEntities:ArrayBuffer[Node[Vars]] = ArrayBuffer[Node[Vars]]()
  mentions foreach addEntity

  proposalHooks += {proposal:Proposal[(Node[Vars], Node[Vars])] =>
    val newEntities = new HashSet[Node[Vars]]
    for(diff<-proposal.diff){
      diff.variable match {
        case v:Node[Vars]#Exists => diff.undo()
        //case v:Node[Vars]#IsMention => diff.undo()
        case _ => {}
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case children:Node[Vars]#Children => if(!children.node.existsVar.booleanValue && !children.node.exists)newEntities += children.node
        case _ => {}
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case v:Node[Vars]#Exists => diff.redo()
        //case v:IsEntity => diff.redo()
        case _ => {}
      }
    }

    for(entity<-newEntities)addEntity(entity)
  }
  /*
  override def proposalHook(proposal:Proposal[(Node[Vars], Node[Vars])]) = {
    //super.proposalHook(proposal)
    val newEntities = new HashSet[Node[Vars]]
    for(diff<-proposal.diff){
      diff.variable match {
        case v:Node[Vars]#Exists => diff.undo()
        //case v:Node[Vars]#IsMention => diff.undo()
        case _ => {}
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case children:Node[Vars]#Children => if(!children.node.existsVar.booleanValue && !children.node.exists)newEntities += children.node
        case _ => {}
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case v:Node[Vars]#Exists => diff.redo()
        //case v:IsEntity => diff.redo()
        case _ => {}
      }
    }

    for(entity<-newEntities)addEntity(entity)
  }
  */
  def addEntity(e:Node[Vars]):Unit ={
    entities += e
    //for(cname <- e.canopyAttributes.map(_.canopyName)){
    entMap.put(e.id.toString, e)
    for(cname <- e.variables.canopies){
      canopies.getOrElse(cname,{val a = new ArrayBuffer[Node[Vars]];canopies(cname)=a;a}) += e
    }
  }

  def nextEntityPair:(Node[Vars],Node[Vars]) = {
    //println("Generating contexts from depths: %s".format(entities.groupBy(_.depth).mapValues(_.size).toSeq))
    var e1 = nextEntity(null)
    var e2 = nextEntity(e1)
    //while(e1!=null && !e1.moveable)e1=e1.parentEntity.asInstanceOf[KBEntity]
    //while(e2!=null && !e2.moveable)e2=e2.parentEntity.asInstanceOf[KBEntity]
    //if(e1==null || e2==null)nextEntityPair else (e1,e2)
    (e1,e2)
  }

  //var numContexts = 0

  def nextContext = {
    //if(numContexts % 100 == 0) {
    //  println("generated %d contexts".format(numContexts))
    //}
    //numContexts += 1
    nextEntityPair
  }


  def sampleCanopyName(context:Node[Vars]):String = context.variables.canopies.sampleUniformly(random)
  def nextEntity(context:Node[Vars]=null.asInstanceOf[Node[Vars]]):Node[Vars] = {
    var result:Node[Vars]=null.asInstanceOf[Node[Vars]]
    val src = "wp"
    if(context==null)result = sampleEntity(entities)
    else {
      var count = 0
      val cname = sampleCanopyName(context)
      //val cname = context.canopyAttributes.sampleUniformly(random).canopyName
      val canopy = canopies.getOrElse(cname,{val c = new ArrayBuffer[Node[Vars]];c+=context;c})
      result= if(canopy.size>0) sampleEntity(canopy) else sampleEntity(entities)//{val c = new ArrayBuffer[E];c+=context;c})
      /*
      if(context != null && context.entityRoot.attr[SourceBagVar].value(src)>0){
        println(context.entityRoot.attr[SourceBagVar].value.asHashMap)
        throw new Exception ("got here")
        while(result != null && result.entityRoot.attr[SourceBagVar].value(src)>0 && count<10){
          val cname = sampleCanopyName(context)//context.canopyAttributes.sampleUniformly(random).canopyName
          val canopy = canopies.getOrElse(cname,{val c = new ArrayBuffer[KBEntity];c+=context;c})
          result= if(canopy.size>0)sampleEntity(canopy) else sampleEntity(entities)
          count+=1
        }
      }
      */
    }
    //if(result==null)result = context
    if(result.children.size > 1) {
      //println("sampled a node with %d children".format(result.childEntitiesSize))
    }

    result
  }

  protected def sampleEntity(samplePool:ArrayBuffer[Node[Vars]]) = {
    val initialSize = samplePool.size
    var tries = 5
    var e = null.asInstanceOf[Node[Vars]]
    while({tries-=1;tries} >= 0 && (e==null || !e.exists) && samplePool.size>0){
      e = samplePool(random.nextInt(samplePool.size))
      if(tries==1)performMaintenance(samplePool)
    }
    //if(e!=null && !e.isConnected)throw new Exception("NOT CONNECTED")
    if(e != null && !e.exists)e=null.asInstanceOf[Node[Vars]]
    e
  }
  def performMaintenance(es:ArrayBuffer[Node[Vars]]):Unit ={
    //println("Performing maintenance")
    var oldSize = es.size
    val cleanEntities = new ArrayBuffer[Node[Vars]]
    cleanEntities ++= es.filter(_.exists)
    deletedEntities ++= es.filter(!_.exists)
    es.clear()
    es++=cleanEntities
    //println("  removed "+(oldSize-es.size)+ " disconnected entities. new size:"+es.size)
  }

}
