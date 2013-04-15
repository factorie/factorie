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
      f match {
        case f: Factor1[V] =>
        for (s <- p.activeDomain) distribution(s) += f.score(d.domain(s).asInstanceOf[V#Value])
        case f: Factor2[_,_] =>
          val isFirstVariable = f._1 eq d
          val other = if (isFirstVariable) f._2 else f._1
          if (other.isInstanceOf[V] && summary._marginals1.contains(other.asInstanceOf[V])) {
            val fa = f.asInstanceOf[Factor2[V,V]]
            val otherMarginal = summary.marginal(other)
            for (s <- fa.valuesIterator)
              if (isFirstVariable)
                distribution(s.value1.intValue) += otherMarginal.proportions(s.value2.intValue) * fa.score(s.value1, s.value2)
              else
                distribution(s.value2.intValue) += otherMarginal.proportions(s.value1.intValue) * fa.score(s.value1, s.value2)
          } else {
            for (s <- p.activeDomain) distribution(s) += f.assignmentScore(new Assignment1[V](d, d.domain(s).asInstanceOf[V#Value]))
          }
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
