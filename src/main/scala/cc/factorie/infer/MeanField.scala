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

package cc.factorie.infer

import cc.factorie.directed._
import scala.collection.mutable.HashMap
import cc.factorie.variable._
import cc.factorie.model.Model

/** An inferencer for mean field inference.
    Note that this is not just the set of marginals for the Q distribution (that is the embedded Summary);
    It is also the procedure for updating the Q. */
trait MeanField {
  def updateQ(): Unit
  def summary: Summary
}

/** Performs naive mean field inference with a Q Summary that is a set of independent Discrete distributions */
class DiscreteMeanField[V<:DiscreteVar](val model:Model, val summary:DiscreteSummary1[V]) extends MeanField {
  def this(vs:Iterable[V], model:Model) = this(model, new DiscreteSummary1(vs))
  def updateQ(d:V): Unit = {
    val marginal = summary.marginal(d)
    val p = marginal.proportions
    val distribution = new cc.factorie.la.DenseTensor1(p.size)
    for (f <- model.factors(d)) {
      val vars = (f.variables.toSet - d).intersect(summary.variableSet.asInstanceOf[Set[Var]]).toSeq
      for (value <- 0 until d.domain.size) {
        if (vars.length == 0) {
          distribution(value) += f.assignmentScore(new Assignment1[V](d, d.domain(value).asInstanceOf[V#Value]))
        } else if (vars.length == 1) {
          val v0 = vars.head
          val m0 = summary.marginal(v0)
          for (value0 <- 0 until v0.asInstanceOf[DiscreteVar].domain.size) {
            distribution(value) += m0.proportions(value0) * f.assignmentScore(new Assignment2[V,V](d, d.domain(value).asInstanceOf[V#Value], v0.asInstanceOf[V], v0.asInstanceOf[DiscreteVar].domain(value0).asInstanceOf[V#Value]))
          }
        } else if (vars.length == 2) {
          val v0 = vars.head
          val m0 = summary.marginal(v0)
          val v1 = vars(1)
          val m1 = summary.marginal(v1)
          for (value0 <- 0 until v0.asInstanceOf[DiscreteVar].domain.size; value1 <- 0 until v1.asInstanceOf[DiscreteVar].domain.size) {
            distribution(value) += m0.proportions(value0) * m1.proportions(value1) * f.assignmentScore(new Assignment3[V,V,V](d, d.domain(value).asInstanceOf[V#Value], v0.asInstanceOf[V], v0.asInstanceOf[DiscreteVar].domain(value0).asInstanceOf[V#Value], v1.asInstanceOf[V], v1.asInstanceOf[DiscreteVar].domain(value1).asInstanceOf[V#Value]))
          }
        } else throw new Error("Mean field currently doesn't work on factors with 3 or more varying neighbors. " + f.getClass.getName)
      }
    }
    distribution.expNormalize()
    p.masses := distribution
  }
  def updateQ: Unit = summary.variables.foreach(updateQ(_))
}

object InferByMeanField extends Infer[Iterable[DiscreteVar],Model] {
  def inferencer[V<:DiscreteVar](variables:Iterable[V], model:Model): DiscreteMeanField[V] = new DiscreteMeanField(variables, model)
  def apply[V<:DiscreteVar](variables:Iterable[V], model:Model): DiscreteSummary1[V] = {
    val inf = inferencer(variables, model)
    for (i <- 0 until 5) inf.updateQ // TODO Replace with a proper convergence criterion!!!
    inf.summary
  }
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Multivariate case yet implemented.")
    apply(variables, model)
  }
}
