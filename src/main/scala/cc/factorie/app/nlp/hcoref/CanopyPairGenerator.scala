package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.{SettingsSampler, Proposal}
import scala.collection.mutable
import cc.factorie._

/**
  * @author John Sullivan
 */
trait Canopy {
  def canopies:Seq[String]
}

trait SingularCanopy extends Canopy {
  final def canopies = Seq(canopy)

  def canopy:String
}

trait CanopyPairGenerator[Vars <: NodeVariables[Vars] with Canopy] extends PairGenerator[Vars] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  val entMap = new mutable.HashMap[String, Node[Vars]]()
  protected var canopies = new mutable.HashMap[String,mutable.ArrayBuffer[Node[Vars]]]()
  protected var entities = mutable.ArrayBuffer[Node[Vars]]()
  protected var deletedEntities = mutable.ArrayBuffer[Node[Vars]]()
  mentions foreach addEntity

  proposalHooks += {proposal:Proposal[(Node[Vars], Node[Vars])] =>
    val newEntities = mutable.HashSet[Node[Vars]]()
    for(diff<-proposal.diff){
      diff.variable match {
        case v:Node[Vars]#Exists => diff.undo()
        case _ => ()
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case children:Node[Vars]#Children => if(!children.node.existsVar.booleanValue && !children.node.exists)newEntities += children.node
        case _ => ()
      }
    }
    for(diff<-proposal.diff){
      diff.variable match{
        case v:Node[Vars]#Exists => diff.redo()
        case _ => ()
      }
    }

    for(entity<-newEntities)addEntity(entity)
  }

  def addEntity(e:Node[Vars]):Unit ={
    entities += e
    entMap.put(e.id.toString, e)
    for(cname <- e.variables.canopies){
      canopies.getOrElse(cname,{val a = new mutable.ArrayBuffer[Node[Vars]];canopies(cname)=a;a}) += e
    }
  }

  def nextEntityPair:(Node[Vars],Node[Vars]) = {
    val e1 = nextEntity(null)
    val e2 = nextEntity(e1)
    e1 -> e2
  }

  def nextContext = nextEntityPair


  def sampleCanopyName(context:Node[Vars]):String = context.variables.canopies.sampleUniformly(random)
  def nextEntity(context:Node[Vars]=null.asInstanceOf[Node[Vars]]):Node[Vars] = {
    var result:Node[Vars]=null.asInstanceOf[Node[Vars]]
    if(context==null)result = sampleEntity(entities)
    else {
      val cname = sampleCanopyName(context)
      val canopy = canopies.getOrElse(cname,{val c = new mutable.ArrayBuffer[Node[Vars]];c+=context;c})
      result= if(canopy.size>0) sampleEntity(canopy) else sampleEntity(entities)

    }

    result
  }

  protected def sampleEntity(samplePool:mutable.ArrayBuffer[Node[Vars]]) = {
    var tries = 5
    var e = null.asInstanceOf[Node[Vars]]
    while({tries-=1;tries} >= 0 && (e==null || !e.exists) && samplePool.size>0){
      e = samplePool(random.nextInt(samplePool.size))
      if(tries==1)performMaintenance(samplePool)
    }
    if(e != null && !e.exists)e=null.asInstanceOf[Node[Vars]]
    e
  }
  def performMaintenance(es:mutable.ArrayBuffer[Node[Vars]]):Unit ={
    val cleanEntities = mutable.ArrayBuffer[Node[Vars]]()
    cleanEntities ++= es.filter(_.exists)
    deletedEntities ++= es.filter(!_.exists)
    es.clear()
    es++=cleanEntities
  }

}
