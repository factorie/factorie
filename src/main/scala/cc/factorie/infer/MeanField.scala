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

package cc.factorie.infer

import cc.factorie.model.Model
import cc.factorie.variable._

/** An inferencer for mean field inference.
    Note that this is not just the set of marginals for the Q distribution (that is the embedded Summary);
    It is also the procedure for updating the Q. */
trait MeanField {
  def updateQ(): Unit
  def summary: Summary
}

/** Performs naive mean field inference with a Q Summary that is a set of independent Discrete distributions */
class DiscreteMeanField(val model:Model, val summary:DiscreteSummary1[MutableDiscreteVar]) extends MeanField {
  def this(vs:Iterable[MutableDiscreteVar], model:Model) = this(model, new DiscreteSummary1(vs))
  def updateQ(d:MutableDiscreteVar): Unit = {
    val marginal = summary.marginal(d)
    val p = marginal.proportions
    val distribution = new cc.factorie.la.DenseTensor1(p.size)
    for (f <- model.factors(d)) {
      val vars = (f.variables.toSet - d).intersect(summary.variableSet.asInstanceOf[Set[Var]]).toSeq
      for (value <- 0 until d.domain.size) {
        if (vars.length == 0) {
          distribution(value) += f.assignmentScore(new Assignment1[d.type](d, d.domain(value).asInstanceOf[d.Value]))
        } else if (vars.length == 1) {
          val v0 = vars.head
          val m0 = summary.marginal(v0)
          for (value0 <- 0 until v0.asInstanceOf[DiscreteVar].domain.size) {
            distribution(value) += m0.proportions(value0) * f.assignmentScore(new Assignment2[d.type, v0.type](d, d.domain(value).asInstanceOf[d.Value], v0, v0.asInstanceOf[DiscreteVar].domain(value0).asInstanceOf[v0.Value]))
          }
        } else if (vars.length == 2) {
          val v0 = vars.head
          val m0 = summary.marginal(v0)
          val v1 = vars(1)
          val m1 = summary.marginal(v1)
          for (value0 <- 0 until v0.asInstanceOf[DiscreteVar].domain.size; value1 <- 0 until v1.asInstanceOf[DiscreteVar].domain.size) {
            distribution(value) += m0.proportions(value0) * m1.proportions(value1) * f.assignmentScore(new Assignment3[d.type,v0.type,v1.type](d, d.domain(value).asInstanceOf[d.Value], v0, v0.asInstanceOf[DiscreteVar].domain(value0).asInstanceOf[v0.Value], v1, v1.asInstanceOf[DiscreteVar].domain(value1).asInstanceOf[v1.Value]))
          }
        } else throw new Error("Mean field currently doesn't work on factors with 3 or more varying neighbors. " + f.getClass.getName)
      }
    }
    distribution.expNormalize()
    p.masses := distribution
  }
  def updateQ(): Unit = summary.variables.foreach(updateQ)
}

object InferByMeanField extends Infer[Iterable[MutableDiscreteVar],Model] {
  def inferencer(variables:Iterable[MutableDiscreteVar], model:Model): DiscreteMeanField = new DiscreteMeanField(variables, model)
  def apply(variables:Iterable[MutableDiscreteVar], model:Model): DiscreteSummary1[MutableDiscreteVar] = {
    val inf = inferencer(variables, model)
    for (i <- 0 until 5) inf.updateQ() // TODO Replace with a proper convergence criterion!!!
    inf.summary
  }
  def infer(variables:Iterable[MutableDiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Multivariate case yet implemented.")
    apply(variables, model)
  }
}
