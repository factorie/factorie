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

import cc.factorie.infer.{Proposal, SettingsSampler}
import scala.util.Random
import cc.factorie.util.Hooks1
import scala.reflect.ClassTag

/**
 * User:harshal, John Sullivan
 * Date: 10/28/13
 */
abstract class CorefSampler[Vars <: NodeVariables[Vars]](override val model:CorefModel[Vars], val mentions:Iterable[Node[Vars]], val iterations:Int)(implicit override val random:Random, val varsTag:ClassTag[Vars])
  extends SettingsSampler[(Node[Vars], Node[Vars])](model) {
  this: PairGenerator[Vars] with MoveGenerator[Vars] =>

  this.temperature = 0.001

  val beforeInferHooks = new Hooks1[Unit]
  protected def beforeInferHook = beforeInferHooks()

  def infer {
    beforeInferHook
    contexts foreach process
  }

}

trait AutoStoppingSampler[Vars <: NodeVariables[Vars]] extends CorefSampler[Vars] {
  this: PairGenerator[Vars] with MoveGenerator[Vars] =>

  def autoStopThreshold:Int

  private var runOfEmptyProposals = 0


  override def processProposals(props: Seq[Proposal[(Node[Vars], Node[Vars])]]) = {
    if(props.size == 1) { // a proposal of size one is 'empty' because NoMove is always a valid choice
      runOfEmptyProposals += 1
    } else {
      runOfEmptyProposals = 0
    }
    super.processProposals(props)
  }

  override def infer = {
    beforeInferHook
    var step = 0

    while (step < iterations && runOfEmptyProposals < autoStopThreshold) {
      process(nextContext)
      step += 1
    }
    println("Stopping automatically after %d steps".format(step))
  }
}