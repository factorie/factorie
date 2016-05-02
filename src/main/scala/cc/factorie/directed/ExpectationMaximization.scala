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

package cc.factorie.directed

import cc.factorie.infer._
import cc.factorie.model.Model
import cc.factorie.variable.{DiscreteVar, DiscreteVariable, Var}

/** The expectation-maximization method of inference.
    maximizing is the collection of variables that will be maximized.   
    meanField contains the variables for which to get expectations.
    You can provide your own Maximizer; other wise an instance of the default MaximizeSuite is provided. */
class EMInferencer[V<:Var,W<:DiscreteVar,M<:Model](val maximizing:Iterable[V], val marginalizing:Iterable[W], val model:M, val expecter:Infer[Iterable[W],M], val maximizer:Infer[Iterable[V],M]) {
  var summary: Summary = null
  def eStep(): Unit = summary = expecter.infer(marginalizing.toSeq, model)
  // The "foreach and Seq(v)" below reflect the fact that in EM we maximize the variables independently of each other 
  def mStep(): Unit = maximizing.foreach(v => maximizer.infer(Seq(v), model, summary).setToMaximize(null)) // This "get" will fail if the Maximizer was unable to handle the request
  def process(iterations:Int): Unit = for (i <- 0 until iterations) { eStep; mStep } // TODO Which should come first?  mStep or eStep?
  def process(): Unit = process(100) // TODO Make a reasonable convergence criteria
}

object EMInferencer {
  def apply[V<:Var](maximizing:Iterable[V], varying:Iterable[DiscreteVariable], model:Model, maximizer:Maximize[Iterable[V],Model] = Maximize, infer:Infer[Iterable[DiscreteVar],Model] = InferByBPTree) =
    new EMInferencer(maximizing, varying, model, infer, maximizer)
}
