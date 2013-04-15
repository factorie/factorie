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
import cc.factorie.generative._
import scala.collection.mutable.HashMap

/** An inferencer for mean field inference.
    Note that this is not just the set of marginals for the Q distribution (that is the embedded Summary);
    It is also the procedure for updating the Q. */
trait MeanField {
  def updateQ: Unit
  def summary: Summary[Marginal]
}

/** Performs naive mean field inference with a Q Summary that is a set of independent Discrete distributions */
class DiscreteMeanField[V<:DiscreteVar](val model:Model, val summary:DiscreteSummary1[V]) extends MeanField {
  def this(vs:Iterable[V], model:Model) = this(model, new DiscreteSummary1(vs))
  def updateQ(d:V): Unit = {
    val marginal = summary.marginal(d)
    val p = marginal.proportions
    val distribution = new cc.factorie.la.DenseTensor1(p.size)
    for (f <- model.factors(d)) {
      val vars = (f.variables.toSet - d).intersect(summary.variables.toSet).toSeq
      val marginals = vars.map(summary.marginal(_))
      for (values <- f.valuesIterator) {
        var marg = 1.0
        for (i <- 0 until vars.length) {
          marg *= marginals(i).proportions(values(vars(i)).asInstanceOf[V#Value].intValue)
        }
        distribution(values(d).intValue) += marg*f.assignmentScore(values)
      }
    }
    distribution.expNormalize()
    p.masses := distribution
  }
  def updateQ: Unit = summary.variables.foreach(updateQ(_))
}

object InferByMeanField {
  def inferencer[V<:DiscreteVar](variables:Iterable[V], model:Model): DiscreteMeanField[V] = new DiscreteMeanField(variables, model)
  def apply[V<:DiscreteVar](variables:Iterable[V], model:Model): DiscreteSummary1[V] = {
    val inf = inferencer(variables, model)
    for (i <- 0 until 50) inf.updateQ // TODO Replace with a proper convergence criterion!!!
    inf.summary
  }
}
