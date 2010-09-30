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

/** Collins' structured-perceptron */
abstract class StructuredPerceptron[V<:Variable with TrueSetting](model:Model) extends GradientAscentUpdates {
  //type TemplatesToUpdate = DotTemplate
  var learningMargin = 1.0 // TODO not currently used
  private var difflist: DiffList = null
  
  // Abstract method to be provided elsewhere
  def predict(vs:Seq[V]): Unit
  
  def process(vs:Seq[V]): Unit = {
    predict(vs)
    difflist = new DiffList
    vs.foreach(_.setToTruth(difflist))
    difflist.undo // TODO Consider commenting this out, but consider carefully.  Dangerous for "addGradient" to have side-effects.
    if (difflist.size > 0)
      updateWeights // This will result in a call to "addGradient"
  }

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit = {
    if (!difflist.done) difflist.redo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistics.vector *  rate)
    difflist.undo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistics.vector * -rate)
  }

}


// TODO What would a MaximumMargin trait look like?
