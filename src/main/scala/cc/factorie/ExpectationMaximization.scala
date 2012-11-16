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

package cc.factorie.generative
import cc.factorie._

/** The expectation-maximization method of inference.
    maximizing is the collection of variables that will be maximized.   
    meanField contains the variables for which to get expectations.
    You can provide your own Maximizer; other wise an instance of the default MaximizeSuite is provided. */
class EMInferencer[V<:Variable,M<:MeanField](val maximizing:Iterable[V], val meanField:M, val model:Model, val maximizer: Maximize = Maximize) {
  def eStep: Unit = meanField.updateQ
  // The "foreach and Seq(v)" below reflect the fact that in EM we maximize the variables independently of each other 
  def mStep: Unit = maximizing.foreach(v => maximizer.infer(Seq(v), model, meanField.summary).get.setToMaximize(null)) // This "get" will fail if the Maximizer was unable to handle the request
  def process(iterations:Int): Unit = for (i <- 0 until iterations) { eStep; mStep } // TODO Which should come first?  mStep or eStep?
  def process: Unit = process(100) // TODO Make a reasonable convergence criteria
}
object EMInferencer {
  def apply[V<:Variable](maximizing:Iterable[V], varying:Iterable[DiscreteVariable], model:Model, maximizer:Maximize = Maximize) = 
    new EMInferencer(maximizing, new DiscreteMeanField(varying, model), model, maximizer)
}

object InferByEM extends Infer {
  def apply(maximize:Iterable[Variable], varying:Iterable[DiscreteVariable], model:Model, maximizer:Maximize = Maximize): DiscreteSummary1[DiscreteVariable] = {
    val meanField = new DiscreteMeanField(varying, model)
    val inferencer = new EMInferencer(maximize, meanField, model, maximizer)
    inferencer.process
    meanField.summary
  }
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = {
    summary match {
      case ds:DiscreteSummary1[DiscreteVariable] => {
        val meanField = new DiscreteMeanField(model, ds)
        val inferencer = new EMInferencer(variables, meanField, model, Maximize)
        try { inferencer.process } catch { case e:Error => return None } // Because the maximizer might fail
        Some(meanField.summary)
      }
      case _ => None
    }
  }
}
