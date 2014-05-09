package cc.factorie.app.nlp.xcoref


import cc.factorie._
import cc.factorie.infer.{Proposal, SettingsSampler}
import scala.collection.mutable

/**
 * @author John Sullivan
 */
trait Canopy {
  def canopies:Seq[String]
}

trait CanopyPairGenerator[Vars <: NodeVariables[Vars] with Canopy] extends PairContextGenerator[Vars] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  proposalHooks += { p:Proposal[(Node[Vars], Node[Vars])] =>
    val (e1, e2) = p.context
    e1.parent match {
      case Some(p) => addToCanopy(p)
      case None => e2.parent match {
        case Some(p) => addToCanopy(p)
        case None => Unit
      }
    }
  }

  protected def addToCanopy(n:Node[Vars]) {

    n.variables.canopies.foreach { canopy =>
      if(canopyMap.contains(canopy)) {
        canopyMap.put(canopy, canopyMap(canopy) += n)
      }
    }
  }

  protected var canopyMap = new mutable.HashMap[String,mutable.ArrayBuffer[Node[Vars]]].withDefault(_ => new mutable.ArrayBuffer[Node[Vars]]())

  mentions.foreach{
    mention => mention.variables.canopies.foreach{
      canopy => canopyMap.put(canopy, canopyMap(canopy) += mention)
    }
  }

  canopyMap = canopyMap.filter(_._2.size > 1)

  def nextInCanopy(context:Node[Vars]):Node[Vars] = {
    val canopies = context.variables.canopies.flatMap(canopyMap.get) //.sampleUniformly(random).sampleUniformly(random)
    if(canopies.size == 0) {
      randomNode
    } else {
      canopies.sampleUniformly(random).sampleUniformly(random)
    }
  }

  override def nextContext: (Node[Vars], Node[Vars]) = {
    if(mentions.size == 1) {throw new Error("Cannot sample pairs from a single node")}
    val n1 = randomNode
    var n2 = nextInCanopy(n1)
    while(n1 == n2) {
      n2 = nextInCanopy(n1)
    }
    n1 -> n2
  }
}
