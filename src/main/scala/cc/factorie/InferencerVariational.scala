/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.HashMap


class MeanFieldInferencer[A<:Variable with QDistribution](model:Model, variables:Iterable[A]) {
  private val _q = new HashMap[Variable,Variable] // 2nd is actually a Distribution[_]
  variables.foreach(v => _q(v) = v.newQ)
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  def updateQ(v:A): Unit = {
    (v, _q(v)) match {
      case (v:DiscreteVariable, d:DenseProportions) => {
        val distribution = new Array[Double](v.domainSize)
        for (i <- 0 until v.domainSize) {
          val diff = new DiffList
          v.setByIndex(i)(diff)
          val factors = diff.factorsOf[Template](model)
          val neighbors = factors.flatMap(_.variables).filter(_ != v).toSet
          if (neighbors.exists(_.isInstanceOf[Variable with QDistribution])) throw new Error("Not yet implemented neighboring mean fields.")
          val modelScore = diff.scoreAndUndo(model)
          distribution(i) = modelScore
        }
        Maths.expNormalize(distribution)
        d.set(distribution)(null)
      }
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
      new Proposal(diff, modelScore, Math.NaN_DOUBLE, modelScore)
    }
    val proposal = sampleExpProportionally(proposals, _.acceptanceScore)
    proposal.diff.redo
    variablesNeighbors.foreach(_.postChange(v))
  }

}
*/
