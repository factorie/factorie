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

package cc.factorie.directed

import cc.factorie._

/** The expectation-maximization method of inference.
    maximizing is the collection of variables that will be maximized.   
    meanField contains the variables for which to get expectations.
    You can provide your own Maximizer; other wise an instance of the default MaximizeSuite is provided. */
class EMInferencer[V <: Var, W <: DiscreteVar](val maximizing: Iterable[V], val marginalizing: Iterable[W], val infer: Infer, val model: Model, val maximizer: Maximize = Maximize) {
  var summary: Summary = null
  def eStep: Unit = summary = infer.infer(marginalizing, model).head
  // The "foreach and Seq(v)" below reflect the fact that in EM we maximize the variables independently of each other 
  def mStep: Unit = maximizing.foreach(v => maximizer.infer(Seq(v), model).get.setToMaximize(null)) // This "get" will fail if the Maximizer was unable to handle the request
  def process(iterations:Int): Unit = for (i <- 0 until iterations) { eStep; mStep } // TODO Which should come first?  mStep or eStep?
  def process: Unit = process(100) // TODO Make a reasonable convergence criteria
}
object EMInferencer {
  def apply[V <: Var](maximizing: Iterable[V], varying: Iterable[DiscreteVariable], model: Model, maximizer: Maximize = Maximize, infer: Infer = InferByBPTreeSum) =
    new EMInferencer(maximizing, varying, infer, model, maximizer)
}

object InferByEM {
  def apply(maximize: Iterable[Var], varying: Iterable[DiscreteVariable], model: Model, maximizer: Maximize = Maximize): Summary = {
    val inferencer = new EMInferencer(maximize, varying, InferByBPTreeSum, model, maximizer)
    inferencer.process
    inferencer.summary
  }
}

class InferByEM(varying: Iterable[DiscreteVariable]) extends Infer {
  def infer(variables:Iterable[Var], model:Model): Option[Summary] = {
    val inferencer = new EMInferencer(variables, varying, InferByBPTreeSum, model, Maximize)
    try { inferencer.process } catch { case e:Error => return None } // Because the maximizer might fail
    Some(inferencer.summary)
  }
}