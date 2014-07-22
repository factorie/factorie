/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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

import scala.util.Random
import scala.collection.mutable
import cc.factorie.infer.{Proposal, SettingsSampler}

/**
 * @author John Sullivan
 */
trait ContextGenerator[Vars <: NodeVariables[Vars]] {
  def nextContext:(Node[Vars], Node[Vars])
  def iterations:Int
  def mentions:Iterable[Node[Vars]]

  def contexts:Iterable[(Node[Vars], Node[Vars])] = new Iterator[(Node[Vars], Node[Vars])] {

    var index = 0

    def hasNext: Boolean = index < iterations

    def next(): (Node[Vars], Node[Vars]) = if(hasNext) {
      index += 1
      nextContext
    } else {
      throw new NoSuchElementException("Max iterations exceeded %d" format iterations)
    }
  }.toStream
}

trait PairContextGenerator[Vars <: NodeVariables[Vars]] extends ContextGenerator[Vars]{
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    val newEntities = mutable.HashSet[Node[Vars]]()



    val (e1, e2) = p.context
    e1.parent match {
      case Some(parent) if parent.exists => newEntities += parent
      case None => e2.parent match {
        case Some(parent) if parent.exists => newEntities += parent
        case None => ()
      }
    }

    p.diff.collect{
      case c:Node[Vars]#Children if c.node.exists && !c.node.isMention => newEntities += c.node
      //addEntity(c.node)
    }

    newEntities.foreach(addEntity)
    /*
      p.diff match {
        case v:Verbosity => if(v.moveType == "Merge Up") {
          println("After merging up we have %d entities (%d before)".format(_allEntities.size, numEnts))
        }
        case _ => ()
      }
      */
  }


  protected val _allEntities = mutable.ArrayBuffer[Node[Vars]]()
  def addEntity(e:Node[Vars]) {_allEntities += e}

  _allEntities ++= mentions

  private def performMaintenance {
    val cleanEntities = new mutable.ArrayBuffer[Node[Vars]]
    cleanEntities ++= _allEntities.filter(_.exists)
    _allEntities.clear()
    _allEntities ++= cleanEntities
  }

  var numContexts = 0

  implicit def random:Random
  override def nextContext:(Node[Vars], Node[Vars]) = {
    val n1 = randomNode
    var n2 = randomNode
    while(n1 == n2) {
      n2 = randomNode
    }
    if(numContexts % 100 == 0) {
      println("generated %d contexts".format(numContexts))
    }
    numContexts += 1
    //println("Generated a context: %s".format(n1 -> n2))
    n1 -> n2
  }
  var multiSamples = 0

  def randomNode:Node[Vars] = {
    var tries = 5
    var e = null.asInstanceOf[Node[Vars]]
    while({tries -=1; tries} >= 0 && (e == null || !e.exists)) {
      e = _allEntities.toSeq(random.nextInt(_allEntities.size))
      if(tries==1) {
        performMaintenance
        tries = 5
      }
    }
    if(e.children.size > 1) {
      multiSamples += 1
    }
    e
  }


}