/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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

import cc.factorie.infer.{Proposal, SettingsSampler}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

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

private[hcoref] class EntityPairGenHashSet[Vars <: NodeVariables[Vars]](mentionSize:Int)(implicit random:Random) extends mutable.HashSet[Node[Vars]] {
  override def initialSize = (mentionSize * 1.5).toInt

  def sample:Node[Vars] = {
    var cand = table(random.nextInt(table.length))
    while(cand == null ) {
      cand = table(random.nextInt(table.length))
    }
    cand.asInstanceOf[Node[Vars]]
  }
}

trait CanopyPairGenerator[Vars <: NodeVariables[Vars] with Canopy] extends PairGenerator[Vars] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  protected var canopies = new mutable.HashMap[String,EntityPairGenHashSet[Vars]]()
  var entities = new EntityPairGenHashSet[Vars](mentions.size)
  var nonexistentEnts = new mutable.HashSet[Node[Vars]]
  mentions foreach addEntity

  proposalHooks += {proposal:Proposal[(Node[Vars], Node[Vars])] =>
    val iter = proposal.diff.iterator
    while(iter.hasNext) {
      val diff = iter.next()
      if(diff.variable.isInstanceOf[Node[Vars]#Exists]) {
        val v = diff.variable.asInstanceOf[Node[Vars]#Exists]
        val newValue = v.booleanValue
        diff.undo()
        val oldValue = v.booleanValue
        diff.redo()
        if(newValue != oldValue) {
          if(newValue) {
            addEntity(v.node)
          } else {
            nonexistentEnts += v.node
          }
        }
      }
    }
  }

  def addEntity(e:Node[Vars]):Unit ={
    entities += e
    val iter = e.variables.canopies.iterator
    while(iter.hasNext) {
      val canopy = iter.next()
      val canopyEnts = canopies.getOrElse(canopy, new EntityPairGenHashSet[Vars](5))
      canopyEnts += e
      canopies(canopy) = canopyEnts
    }
  }

  def nextEntityPair:(Node[Vars],Node[Vars]) = {
    val e1 = getEntity(null)
    val e2 = getEntity(e1)
    e1 -> e2
  }

  def nextContext = nextEntityPair

  @tailrec
  private def getEntity(context:Node[Vars]):Node[Vars] = if(context != null) {
    val nodeCanopies = context.variables.canopies.toSeq

    val candidates = canopies(nodeCanopies(random.nextInt(nodeCanopies.size)))

    if(candidates.size <= 1) {
      getEntity(null)
    } else {
      var e = candidates.sample
      var i = 0
      while(!e.exists) {
        i += 1
        e = candidates.sample
        if(i % 5 == 0) {
          cleanEntities()
        }
      }
      e
    }
  } else {
    var e = entities.sample
    var i = 0
    while(!e.exists) {
      i += 1
      e = entities.sample
      if(i % 5 == 0) {
        cleanEntities()
      }
    }
    e
  }

  private def cleanEntities(): Unit = {
    val iter = nonexistentEnts.iterator
    while(iter.hasNext) {
      entities remove iter.next()
    }
    nonexistentEnts = new mutable.HashSet[Node[Vars]]
  }

}
