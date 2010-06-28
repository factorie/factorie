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
class CollapsedVariationalBayes[A<:Variable with QDistribution](model:Model, variables:Iterable[A]) {
  private val _q = new HashMap[Variable,Variable]
  variables.foreach(v => _q(v) = v.newQ)
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  def setMaxMarginals(implicit d:DiffList = null): Unit = {
    _q.
  }
  def process(v:A): Unit = {
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.template.getClass.getName < f2.template.getClass.getName)
    factors match {
      case List(factor1:GeneratedValueTemplate#Factor, factor2:MixtureChoiceVariableTemplate#Factor) {
        // Variational Bayes order 0 approximation
        val parent = v.proportions
        val domainSize = v.domain.size
        val distribution = new Array[Double](domainSize)
        var sum = 0.0
        val vq = q(factor2.v1)

        // If collapsed, decrement counts.  
        parent match {
          case collapsedParent:DirichletMultinomial => forIndex(vq.size)(i => collapsedParent.increment(i, -vq(i)))
          case _ => new Error // TODO Change this to just do nothing?
        }
        // The next line calls ParameterRef.set, which calls Parameter.removeChild, which decrements counts
        v.setToNull

        // Build the distribution from which we will sample
        if (v.gatedRefs.size == 1) {
          val outcome: MixtureOutcome = v.gatedRefs.first.asInstanceOf[GatedParameterRef[Proportions,MixtureOutcome]].child
          for (i <- 0 until domainSize) {
            distribution(i) = parent.pr(i) * outcome.prFromMixtureComponent(i)
            sum += distribution(i)
          }
        } else {
          val refs = v.gatedRefs.map(_ match { case gpr:GatedParameterRef[Proportions,MixtureOutcome] => gpr })
          val outcomes: Seq[MixtureOutcome] = refs.map(_.child)
          for (i <- 0 until domainSize) {
            distribution(i) = parent.pr(i) * outcomes.foldLeft(1.0)((prod,o) => prod * o.prFromMixtureComponent(i))
            sum += distribution(i)
          }
        }

        forIndex(distribution.size)(i => distribution(i) /= sum)
        vq := distribution
        
          // Sample
          //println("MixtureChoiceCollapsedGibbsSamplerHandler distribution = "+(distribution.toList.map(_ / sum)))
          val newValue = Maths.nextDiscrete(distribution, sum)(Global.random)
          //println("MixtureChoiceCollapsedGibbsSamplerHandler newValue="+newValue)

          // Set the new value; this will change ref.value, calling Parameter.addChild, which increments counts; it is paired with setToNull
          v.setByIndex(newValue)
          // If collapsed, increment counts
          parent match {
            case collapsedParent:DirichletMultinomial => forIndex(vp.size)(i => collapsedParent.increment(i, vq(i)))
            case _ => new Error // TODO Change this to just do nothing?
          }
      }
    }
}
*/

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
