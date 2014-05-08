package cc.factorie.app.nlp.xcoref

import cc.factorie._
import cc.factorie.infer.{Proposal, SettingsSampler}
import scala.collection.mutable

/**
 * @author John Sullivan
 */
trait DeterministicPairGenerator[Vars <: NodeVariables[Vars]] extends PairContextGenerator[Vars]{
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  var i = 0
  def mentionSequence:IndexedSeq[(String, String, String)]

  var nextId:String = "NONE SET"

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    val (e1, e2) = p.context
    e1.parent match {
      case Some(parent) => mentionMap.put(parent.id, parent)
      case None => Unit
    }
    e2.parent match {
      case Some(parent) => mentionMap.put(parent.id, parent)
      case None => Unit
    }
  }

  private val mentionMap = mutable.HashMap[String, Node[Vars]]()
  mentionMap ++= mentions.map(m => m.id -> m)

  override def nextContext:(Node[Vars], Node[Vars]) = {
    val e1 = sampleEntity
    var e2 = sampleEntity
    while(e1 == e2) {
      e2 = sampleEntity
    }

    val (e1Id, e2Id, parentId) = mentionSequence(i)
    i += 1
    nextId = parentId
    mentionMap.getOrElse(e1Id,sampleEntity) -> mentionMap.getOrElse(e2Id, sampleEntity)
  }

  //protected val _allEntities = mutable.ArrayBuffer[Node[Vars]]()

  _allEntities ++= mentions

  //def addEntity(e:Node[Vars]) {_allEntities += e}

  def allEntities:Iterable[Node[Vars]] = _allEntities

  def performMaintenance {
    val cleanEntities = new mutable.ArrayBuffer[Node[Vars]]
    cleanEntities ++= _allEntities.filter(_.exists)
    _allEntities.clear()
    _allEntities ++= cleanEntities
  }

  def sampleEntity:Node[Vars] = {
    var tries = 5
    val numEnts = _allEntities.size
    var e: Node[Vars] = null.asInstanceOf[Node[Vars]]
    while({tries -=1; tries} >= 0 && (e == null || !e.exists)) {
      e = _allEntities(random.nextInt(numEnts))
      if(tries==1) {
        performMaintenance
      }
    }
    e

  }

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    val (e1, e2) = p.context
    e1.parent match {
      case Some(ent) => addEntity(ent)
      case None => e2.parent match {
        case Some(ent) => addEntity(ent)
        case None => Unit
      }
    }
  }
}
