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

package cc.factorie.directed

import cc.factorie.infer.{DiscreteSeqMarginal, Summary1}
import cc.factorie.model.Factor
import cc.factorie.variable._

import scala.collection.mutable.ArrayBuffer

//import cc.factorie.la.ArrayLA.Implicits._

class CollapsedVariationalBayes(collapse:Iterable[Var], marginalize:Iterable[Var], model:DirectedModel) {
  val handlers = new ArrayBuffer[CollapsedVariationalBayesHandler]
  //def defaultHandlers = List(GeneratedVariableCollapsedVariationalBayesHandler, MixtureChoiceCollapsedVariationalBayesHandler)
  def defaultHandlers = throw new Error("Not yet implemented")
  handlers ++= defaultHandlers

//  private val _c = new HashMap[Variable,Variable]
//  private val _q = new HashMap[Variable,Variable]
//  def collapsedMap = _c
//  def qMap = _q
//  //collapse.foreach(v => _c(v) = v.newCollapsed)
//  val collapser = new Collapse(model)
//  collapse.foreach(v => collapser(Seq(v)))
//  marginalize.foreach(v => _q(v) = v.newQ)
//  //def collapsed[V<:CollapsibleParameter](v:V) = _c(v).asInstanceOf[V#CollapsedType]
//  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
//  def collapsedOrSelf(v:Variable): Variable = _c.getOrElse(v, v)
//  def collapsedp2[P<:Variable](v:P): P = _c.getOrElse(v, v).asInstanceOf[P]
//  def collapsedOrNull(v:Variable): Variable = _c.getOrElse(v, null.asInstanceOf[Variable])
//  def qp(v:Variable) = _q.getOrElse(v, v)
//  def setMaxMarginals(implicit d:DiffList = null): Unit = {
//    throw new Error("Not yet implemented")
//  }
//  def children(p:Variable): Iterable[Variable] = throw new Error

  def process(v:MutableVar): DiffList = {
    //assert(!v.isInstanceOf[CollapsedVar]) // We should never be processing a CollapsedVariable
    // Get factors, in sorted order of the their classname
    val factors = model.factors(Seq(v)).toSeq.sortWith((f1:Factor,f2:Factor) => f1.factorName < f2.factorName)
    var done = false
    val handlerIterator = handlers.iterator
    val d = new DiffList
    while (!done && handlerIterator.hasNext) {
      done = handlerIterator.next().process(v, factors, this)(d)
    }
    if (!done) throw new Error("CollapsedVariationalBayes: No sampling method found for variable "+v+" with factors "+factors.map(_.factorName).mkString("List(",",",")"))
    d
  }
}

trait CollapsedVariationalBayesHandler {
  def process(v:Var, factors:Seq[Factor], cvb:CollapsedVariationalBayes)(implicit d:DiffList): Boolean
}


// Collapses LDA's theta, but not phi; gets marginals for Zs
class PlatedGateCollapsedVariationalBayes(val model:DirectedModel, val summary:Summary1[DiscreteSeqVariable,DiscreteSeqMarginal[DiscreteSeqVariable]] = new Summary1[DiscreteSeqVariable,DiscreteSeqMarginal[DiscreteSeqVariable]]) {
  
  def infer(gates:DiscreteSeqVariable, iterations:Int): Unit = {
    val factors = model.factors(gates)
    require(factors.size == 2) 
    val gFactor = factors.collectFirst({case f:PlatedDiscrete.Factor => f}).get
    val mFactor = factors.collectFirst({case f:PlatedDiscreteMixture.Factor => f}).get
    require(gFactor._1 == mFactor._3) // both should equal gates
    val gateDomainSize = gates.domain.elementDomain.size
    val alpha1 = 1.0 / gateDomainSize // + 0.1 for smoothing?
    val theta: Proportions = gFactor._2.value
    var gatesMarginal = summary.marginal(gates)
    if (gatesMarginal eq null) {
      gatesMarginal = new DiscreteSeqMarginal(gates, Seq.fill(gates.length)(new DenseProportions1(gateDomainSize, alpha1))) // all Z marginals initialized to uniform
      summary += gatesMarginal
      theta := gates.length.toDouble / gateDomainSize // initialize theta to uniform with appropriate mass
    }
    val q = new cc.factorie.la.DenseTensor1(gateDomainSize)
    var iteration = 0
    while (iteration < iterations) {
      var i = 0
      // Loop over each "word" in the DiscreteSeqVariable
      while (i < gates.length) {
        val wi = mFactor._1.intValue(i) // the intValue of the Discrete
        val oldQ: Proportions = gatesMarginal.proportionsSeq(i)
        // Remove old mass
        theta -= oldQ
        // Loop over each possible value for this word's gate
        var j = 0
        while (j < gateDomainSize) {
          q(j) = mFactor._2(i).value(j) * gFactor._2.value(j)
          j += 1
        }
        q.normalize()
        theta += q
        i += 1
      }
    }
  }
  def maximize(gates:DiscreteSeqVariable): Unit = {
    val gatesMarginal = summary.marginal(gates)
    var i = 0
    while (i < gates.length) {
      gates.set(i, gatesMarginal.proportionsSeq(i).maxIndex)(null)
      i += 1
    }
  }
}



/*
object GeneratedVariableCollapsedVariationalBayesHandler extends CollapsedVariationalBayesHandler {
  def process(v:Variable, factors:Seq[Factor], cvb:CollapsedVariationalBayes)(implicit d:DiffList): Boolean = {
    factors match {
      case Seq(factor1:GeneratedVarTemplate#Factor) => { 
        v match {
          case v:Discrete => {
            val parent = v.proportions
            val collapsedParent = cvb.collapsedp2[Proportions](parent) // possibly collapsed
            val vq:MutableProportions = cvb.q(v) // This will throw error if 'v' is not in our _q map.  // TODO Is this the behavior we want?
            val domainSize = v.domain.size
            val distribution = new Array[Double](domainSize)
            var sum = 0.0
            assert(!cvb.qMap.contains(parent), "Does not handle variational child with variational parent.")

            // If parent of v is collapsed, decrement counts.  
            collapsedParent match {
              case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, -1.0) // Here 'vq' is a Proportions, so updates all dimensions
              case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
              case _ => {} // Parent is not collapsed, not need to do anything
            }

            // Calculate the marginal distribution vq
            forIndex(domainSize)(i => {
              distribution(i) = collapsedParent.pr(i)
              sum += distribution(i)
            })

            // Update distribution in vq
            if (sum == 0)
              // This can happen for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
              vq.set(new UniformProportions(distribution.length))(d)
            else {
              cc.factorie.maths./=(distribution,sum) // Normalize the array, using ArrayLA.Implicits
              vq.set(distribution)(d)
            }

            // If parent of v is collapsed, increment counts
            collapsedParent match {
              case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, 1.0) // Here 'vq' is a Proportions, so updates all dimensions
              case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
              case _ => {} // Parent is not collapsed, not need to do anything
            }

            true
          }
          case v:GaussianVar => {
            throw new Error("CVB Gaussian not yet implemented.")
          }
          case _ => false
        }
      }
      case _ => false
    }
  }
}

object MixtureChoiceCollapsedVariationalBayesHandler extends CollapsedVariationalBayesHandler {
  def process(v1:Variable, factors:Seq[Factor], cvb:CollapsedVariationalBayes)(implicit d:DiffList): Boolean = {
    factors match {
      case Seq(factor1:GeneratedVarTemplate#Factor, factor2:GeneratedVarTemplate#Factor) => {
        v1 match {
          case v:MixtureChoiceVariable => {
            // Variational Bayes order 0 approximation
            val parent = v.proportions
            val collapsedParent = cvb.collapsedOrSelf(parent).asInstanceOf[Proportions] // possibly collapsed
            val vq:MutableProportions = cvb.q(v) // This will throw error if 'v' is not in our _q map.  // TODO Is this the behavior we want?
            val domainSize = v.domain.size
            val distribution = new Array[Double](domainSize)
            var sum = 0.0
            assert(!cvb.qMap.contains(parent), "Does not handle variational child with variational parent.")

            // If parent of v is collapsed, decrement counts.  
            collapsedParent match {
              case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, -1.0) // Here 'vq' is a Proportions, so updates all dimensions
              case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
              case _ => {} // Parent is not collapsed, not need to do anything
            }

            if (v.outcomes.size == 1) {
              // MixtureChoice v controls only one MixtureOutcome
              val outcome = v.outcomes.head
              val outcomeQ = cvb.qp(outcome) // possibly variational

              // If parents of outcomes are collapsed, decrement counts
              forIndex(v.domain.size)(i => {
                v.set(i)(null)
                for (chosenParent <- outcome.chosenParents) cvb.collapsedOrNull(chosenParent) match {
                  case p:CollapsedParameter => { p.updateChildStats(outcomeQ, -vq(i)) }
                  case _ => {}
                }
              })

              // Calculate the marginal distribution vq
              forIndex(domainSize)(i => {
                distribution(i) = collapsedParent.pr(i) * outcome.prFromMixtureComponent(cvb.collapsedMap, i)
                sum += distribution(i)
              })

              // Update distribution in vq
              if (sum == 0)
                // This can happen for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
                vq.set(new UniformProportions(distribution.length))(d)
              else {
                cc.factorie.maths./=(distribution,sum) // Normalize the array, using ArrayLA.Implicits
                vq.set((distribution.toSeq))(d)
              }

              // If parents of outcomes are collapsed, increment counts
              forIndex(v.domain.size)(i => {
                v.set(i)(null)
                for (chosenParent <- outcome.chosenParents) cvb.collapsedOrNull(chosenParent) match {
                  case p:CollapsedParameter => { p.updateChildStats(outcomeQ, vq(i)) }
                  case _ => {}
                }
              })

            } else {
              // More than one outcome associated with this MixtureChoice
              throw new Error("Not yet implemented.")
            }

            // If parent of v is collapsed, increment counts.  
            collapsedParent match {
              case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(vq, 1.0) // Here 'vq' is a Proportions, so updates all dimensions
              case collapsedParent:CollapsedParameter => throw new Error("Do not know how to handle this CollapsedParameter type.")
              case _ => {} // Parent is not collapsed, not need to do anything
            }

            true
          }

          case _ => false
        }
      }
      case _ => false
    }
  }
}

*/
