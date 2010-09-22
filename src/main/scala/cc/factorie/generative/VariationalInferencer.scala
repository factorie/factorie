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
import scala.collection.mutable.HashMap

/** Inferencer with some variational distributions.  
    Currently only supports variational approximations over single variables at a time.
    @author Andrew McCallum */
// TODO Consider variational approximations over sets of variables by defining something like qMap:Map[Factor,Variable] ??
// In any case, I like the idea of using Factor to represent the collection of variables in variational approximation
trait VariationalInferencer {
  /** A map from a variable to its varitional representation as distribution Q. */
  def qMap: scala.collection.Map[Variable,Variable]
  def qOrSelf(v:Variable): Variable = qMap.getOrElse(v, v)
  def q[V<:Variable with QDistribution](v:V) = qMap(v).asInstanceOf[V#QType]
  def qOrNull(v:Variable): Variable = qMap.getOrElse(v, null.asInstanceOf[Variable]) // TODO Redundant with above method?
}

@deprecated("Just a temporary placeholder.  Very much unfinished.")
class MeanFieldInferencer[A<:Variable with QDistribution](variables:Iterable[A], model:Model = cc.factorie.generative.defaultGenerativeModel) {
  private val _q = new HashMap[Variable,Variable] // 2nd is actually the Q distribution
  variables.foreach(v => _q(v) = v.newQ)
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  def updateQ(v:A): Unit = {
    (v, _q(v)) match {
      case (v:DiscreteVariable, d:DenseProportions) => {
        val distribution = new Array[Double](v.domainSize)
        for (i <- 0 until v.domainSize) {
          val diff = new DiffList
          v.set(i)(diff)
          val factors = diff.factorsOf[Template](model)
          val neighbors = factors.flatMap(_.variables).filter(_ != v).toSet
          if (neighbors.exists(_.isInstanceOf[Variable with QDistribution])) throw new Error("Not yet implemented neighboring mean fields.")
          val modelScore = diff.scoreAndUndo(model)
          distribution(i) = modelScore
        }
        maths.expNormalize(distribution)
        d.set(distribution)(null)
      }
      case _ => throw new Error("MeanField does not know how to handle a variable of type "+v.getClass)
    }
  }
}


/*
class MeanFieldInferencer1[A<:Variable with IterableSettings](model:Model, variables:Iterable[Variable with QDistribution]) {
  type VQ = Variable with QDistribution
  private val _q = new HashMap[Variable,Distribution[_]]
  variables.foreach(v => _q(v) = v.newQ)
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  
  def infer(v:VQ): Unit = {
    require(_q.containsKey(v))
  }
  def sample(v:A)(implicit d:DiffList): Unit = {
    val factors = model.factors(v)
    val neighbors = factors.flatMap(_.variables).unique
    val variablesNeighbors = neighbors.filter(_q.containsKey(_))
    variablesNeighbors.foreach(_.preChange(v))
    proposals = for (diff <- v.settings) yield {
      val factors = diff.factorsOf[Template](model)
      val modelScore = diff.scoreAndUndo(model)
      new Proposal(diff, modelScore, Double.NaN, modelScore)
    }
    val proposal = sampleExpProportionally(proposals, _.acceptanceScore)
    proposal.diff.redo
    variablesNeighbors.foreach(_.postChange(v))
  }

}
*/
