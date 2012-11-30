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
import cc.factorie.optimize._
import cc.factorie.la._
//import collection.mutable.HashMap

/** Collins' structured-perceptron.
    @author Andrew McCallum */
// TODO Make a Piece version of this and get rid of this one. -akm
abstract class StructuredPerceptron[V<:LabeledMutableVar[_]](val model:Model, val optimizer:GradientOptimizer) {
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  var rate = 1.0
  /** Method should set variables to model's MAP solution. */
  def predict(vs:Seq[V]): Unit

  def process(vs:Seq[V]): Unit = {
    predict(vs)
    // TODO To get a margin, we would need 2-best maximization here.
    val difflist = new DiffList
    vs.foreach(_.setToTarget(difflist))
    if (difflist.size > 0) {
      val gradient: WeightsTensor = model.newBlankSparseWeightsTensor
      model.factorsOfFamilies(difflist, familiesToUpdate).foreach(f => gradient(f.family).+=(f.currentStatistics, rate))
      difflist.undo
      model.factorsOfFamilies(difflist, familiesToUpdate).foreach(f => gradient(f.family).+=(f.currentStatistics, -rate))
      optimizer.step(model.weightsTensor, gradient, Double.NaN, Double.NaN)
    }
  }
}
