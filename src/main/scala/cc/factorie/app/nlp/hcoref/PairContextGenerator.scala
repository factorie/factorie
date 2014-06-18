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
trait PairContextGenerator[Vars <: NodeVariables[Vars]] {
  this:SettingsSampler[(Node[Vars], Node[Vars])] =>

  proposalHooks += {p:Proposal[(Node[Vars], Node[Vars])] =>
    p.diff.collect{
      case c:Node[Vars]#Children if c.node.exists && !c.node.isMention => addEntity(c.node)
    }
  }

  def mentions:Iterable[Node[Vars]]

  protected val _allEntities = mutable.ArrayBuffer[Node[Vars]]()
  def addEntity(e:Node[Vars]) {_allEntities += e}

  _allEntities ++= mentions

  private def performMaintenance {
    val cleanEntities = new mutable.ArrayBuffer[Node[Vars]]
    cleanEntities ++= _allEntities.filter(_.exists)
    _allEntities.clear()
    _allEntities ++= cleanEntities
  }


  def iterations:Int
  implicit def random:Random
  def nextContext:(Node[Vars], Node[Vars]) = {
    val n1 = randomNode
    var n2 = randomNode
    while(n1 == n2) {
      n2 = randomNode
    }
    n1 -> n2
  }

  def randomNode:Node[Vars] = {
    var tries = 5
    var e = null.asInstanceOf[Node[Vars]]
    while({tries -=1; tries} >= 0 && (e == null || !e.exists)) {
      e = _allEntities(random.nextInt(_allEntities.size))
      if(tries==1) {
        performMaintenance
        tries = 5
      }
    }
    e
  }

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