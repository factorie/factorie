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

/** A simple, preliminary expectation-maximization implementation.
    Currently it only works for discrete expectations and maximizing things handled by the default Maximizer. */
class EMInferencer(val maximize:Iterable[Variable], val varying:Iterable[DiscreteVariable], model:Model) {
  val meanField = new DiscreteMeanFieldInferencer(varying, model)
  def eStep: Unit = meanField.updateQ
  def mStep: Unit = maximize.foreach(v => Maximize(Seq(v), model, meanField.summary))
  def process(iterations:Int): Unit = for (i <- 0 until iterations) { mStep; eStep }
}

object InferByEM {
  def apply(maximize:Iterable[Variable], varying:Iterable[DiscreteVariable], model:Model): DiscreteSummary1[DiscreteVariable] = {
    val inferencer = new EMInferencer(maximize, varying, model)
    inferencer.process(100) // TODO Make a clever convergence criteria.
    inferencer.meanField.summary
  } 
}
