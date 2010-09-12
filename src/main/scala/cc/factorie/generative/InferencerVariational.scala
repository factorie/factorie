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


class MeanFieldInferencer[A<:Variable with QDistribution](variables:Iterable[A], model:Model = cc.factorie.generative.defaultGenerativeModel) {
  private val _q = new HashMap[Variable,Variable] // 2nd is actually a Distribution[_]
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
        Maths.expNormalize(distribution)
        d.set(distribution)(null)
      }
    }
  }
}


class CollapsedVariationalBayes[A<:Variable with QDistribution](collapse:Iterable[CollapsibleParameter], marginalize:Iterable[A], model:Model = cc.factorie.generative.defaultGenerativeModel) {
  private val _c = new HashMap[Parameter,Parameter]
  private val _q = new HashMap[Variable,Variable]
  def collapsedMap = _c
  def qMap = _q
  collapse.foreach(v => _c(v) = v.newCollapsed)
  marginalize.foreach(v => _q(v) = v.newQ)
  def collapsed[V<:CollapsibleParameter](v:V) = _c(v).asInstanceOf[V#CollapsedType]
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  def collapsedp(v:Parameter): Parameter = _c.getOrElse(v, v)
  def collapsedp2[P<:Parameter](v:P): P = _c.getOrElse(v, v).asInstanceOf[P]
  def qp(v:Variable) = _q.getOrElse(v, v)
  def setMaxMarginals(implicit d:DiffList = null): Unit = {
    throw new Error("Not yet implemented")
  }
  def children(p:Parameter): Iterable[GeneratedVar] = throw new Error

  def process(v:A): Unit = {
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.template.getClass.getName < f2.template.getClass.getName)
    /*factors match {
      case List(factor1:GeneratedVarTemplate#Factor, factor2:MixtureChoiceVariableTemplate#Factor) => {
        val v = factor2.n1.asInstanceOf[MixtureChoiceVariable]
        // Variational Bayes order 0 approximation
        assert(v.isInstanceOf[MixtureChoiceVariable])
        val parent = v.proportions
        val collapsedParent = collapsedp2[Proportions](parent) // possibly collapsed
        val vq:MutableProportions = q(v) // This will throw error if 'v' is not in our _q map.  // TODO Is this the behavior we want?
        val domainSize = v.domainSize
        val distribution = new Array[Double](domainSize)
        var sum = 0.0

        // If parent of v is collapsed, decrement counts.  
        collapsedParent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, -1.0) // Here 'vq' is a Proportions, so updates all dimensions
          case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
          case _ => {} // Do nothing
        }

        if (v.gatedRefs.size == 1) {
          // MixtureChoice v controls only one GatedRefVariable
          val ref = v.gatedRefs.head.asInstanceOf[GatedParameterRef[Proportions,MixtureOutcome]]
          val refchild = ref.child.asInstanceOf[MixtureOutcome]
          val refchildq = qp(refchild).asInstanceOf[MixtureOutcome]
          assert(refchild == refchildq) // We don't yet know how to handle marginzalized 'z' and marginalized 'w'
          // If parent of outcome 'refchild' is collapsed, decrement counts
          forIndex(domainSize)(i => {
            val refparent = ref.valueForIndex(i)
            val refparentc = collapsedp(refparent)
            if (refparent ne refparentc) refparentc match { case p:CollapsedParameter => p.updateChildStats(refchildq, -vq(i)) }
          })
          // Calculate the marginal distribution vq
          forIndex(domainSize)(i => {
            distribution(i) = collapsedParent.pr(i) * refchild.prFromMixtureComponent(i)
            sum += distribution(i)
          })
          // Update distribution in vq
          vq.set(distribution)(null)
          // If parent of outcome is collapsed, increment counts
          forIndex(domainSize)(i => {
            val refparent = ref.valueForIndex(i)
            val refparentc = collapsedp(refparent)
            if (refparent ne refparentc) refparentc match { case p:CollapsedParameter => p.updateChildStats(refchildq, vq(i)) }
          })
        } else {
          // MixtureChoice v controls multiple GatedRefVariables
          throw new Error("multi-choice not yet implemented")
          val refs = v.gatedRefs.map(_ match { case gpr:GatedParameterRef[Proportions,MixtureOutcome] => gpr })
          val refparents = refs.map((r:GatedParameterRef[Proportions,MixtureOutcome]) => collapsedp(r.value))
          val refchildren = refs.map((r:GatedParameterRef[Proportions,MixtureOutcome]) => qp(r.child).asInstanceOf[MixtureOutcome])
          // If parents of outcomes are collapsed, decrement counts
          for (ref <- refs; val refparent = collapsedp(ref.value); if (refparent ne ref.value))
            refparent match { case p:CollapsedParameter => p.updateChildStats(qp(ref.child).asInstanceOf[MixtureOutcome], -1.0) }
          for (i <- 0 until domainSize) {
            distribution(i) = collapsedParent.pr(i) * refchildren.foldLeft(1.0)((prod,o) => prod * o.prFromMixtureComponent(i))
            sum += distribution(i)
          }
          // Update distribution in vq
          vq.set(distribution)(null)
          // If parent of outcome is collapsed, increment counts
          for (ref <- refs; val refparent = collapsedp(ref.value); if (refparent ne ref.value))
            refparent match { case p:CollapsedParameter => p.updateChildStats(qp(ref.child).asInstanceOf[MixtureOutcome], 1.0) }
        }

        // If parent of v is collapsed, increment counts
        collapsedParent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, 1.0) // Here 'vq' is a Proportions, so updates all dimensions
          case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
          case _ => {} // Do nothing
        }
        
        true
      }
    } */
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
