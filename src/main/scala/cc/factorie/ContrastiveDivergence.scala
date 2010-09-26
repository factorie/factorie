/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie
import cc.factorie.la._

/** Implements Geoff Hinton's Constrastive Divergence, obtaining a gradient after one step away from the true configuration.
    This implementation assumes that the initial configuration is the truth. 
    @author Andrew McCallum
    @since 0.8
 */
abstract class ContrastiveDivergence[C](model:Model) extends MHSampler[C](model) {
  type TemplatesToUpdate = DotTemplate
  def updateWeights: Unit
  var difflist : DiffList = null
  
  override def postProcessHook(context:C, d:DiffList) : Unit = {
    super.postProcessHook(context, d)
    difflist = d
    updateWeights // This will result in a call to addGradient
  }

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit = {
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += (f.statistics.vector * -rate))
    difflist.undo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += (f.statistics.vector *  rate))
  }  
}
