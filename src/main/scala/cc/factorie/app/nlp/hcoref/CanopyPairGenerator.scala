package cc.factorie.app.nlp.hcoref

import cc.factorie.infer.{Proposal, SettingsSampler}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author John Sullivan
 */
trait Canopy {
  def canopies:Iterable[String]
}

trait SingularCanopy extends Canopy {
  final def canopies = Seq(canopy)

  def canopy:String
}

trait CanopyPairGenerator[Vars <: NodeVariables[Vars] with Canopy] extends PairGenerator[Vars] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  protected var canopies = new mutable.HashMap[String,mutable.ArrayBuffer[Node[Vars]]]()
  var entities = mutable.ArrayBuffer[Node[Vars]]()
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
    for(cname <- e.variables.canopies){
      canopies.getOrElse(cname,{val a = new mutable.ArrayBuffer[Node[Vars]];canopies(cname)=a;a}) += e
    }
  }

  def nextEntityPair:(Node[Vars],Node[Vars]) = {
    val e1 = getEntity(None)
    val e2 = getEntity(Some(e1))
    e1 -> e2
  }

  def nextContext = nextEntityPair

  @tailrec
  private def getEntity(context:Option[Node[Vars]]):Node[Vars] = context match {
    case Some(n1) =>
      val nodeCanopies = n1.variables.canopies.toSeq

      val candidates = canopies(nodeCanopies(random.nextInt(nodeCanopies.size)))

      if(candidates.size <= 1) {
        getEntity(None)
      } else {
        var e = candidates(random.nextInt(candidates.size))
        var i = 0
        while(!e.exists) {
          i += 1
          e = candidates(random.nextInt(candidates.size))
          if(i % 5 == 0) {
            cleanEntities()
          }
        }
        e
      }
    case None =>
      var e = entities(random.nextInt(entities.size))
      var i = 0
      while(!e.exists) {
        i += 1
        e = entities(random.nextInt(entities.size))
        if(i % 5 == 0) {
          cleanEntities()
        }
      }
      e
  }

  private def cleanEntities(): Unit = {
    val existingEnts = new mutable.ArrayBuffer[Node[Vars]]
    val iter = entities.iterator
    while(iter.hasNext) {
      val e = iter.next()
      if(e.exists) {
        existingEnts += e
      }
    }
    entities = existingEnts
  }

}
