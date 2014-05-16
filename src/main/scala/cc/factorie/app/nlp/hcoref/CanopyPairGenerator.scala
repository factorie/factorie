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
