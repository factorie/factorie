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
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue, ArrayBuffer}
//import cc.factorie.la.ArrayLA.Implicits._

class CollapsedVariationalBayes(collapse:Iterable[CollapsibleParameter], marginalize:Iterable[Variable with QDistribution], model:Model = cc.factorie.generative.defaultGenerativeModel) {
  val handlers = new ArrayBuffer[CollapsedVariationalBayesHandler]
  def defaultHandlers = List(GeneratedVariableCollapsedVariationalBayesHandler, MixtureChoiceCollapsedVariationalBayesHandler)
  handlers ++= defaultHandlers

  private val _c = new HashMap[Parameter,Parameter]
  private val _q = new HashMap[Variable,Variable]
  def collapsedMap = _c
  def qMap = _q
  collapse.foreach(v => _c(v) = v.newCollapsed)
  marginalize.foreach(v => _q(v) = v.newQ)
  def collapsed[V<:CollapsibleParameter](v:V) = _c(v).asInstanceOf[V#CollapsedType]
  def q[V<:Variable with QDistribution](v:V) = _q(v).asInstanceOf[V#QType]
  def collapsedOrSelf(v:Parameter): Parameter = _c.getOrElse(v, v)
  def collapsedp2[P<:Parameter](v:P): P = _c.getOrElse(v, v).asInstanceOf[P]
  def collapsedOrNull(v:Parameter): Parameter = _c.getOrElse(v, null.asInstanceOf[Parameter])
  def qp(v:Variable) = _q.getOrElse(v, v)
  def setMaxMarginals(implicit d:DiffList = null): Unit = {
    throw new Error("Not yet implemented")
  }
  def children(p:Parameter): Iterable[GeneratedVar] = throw new Error

  def process(v:GeneratedVariable): DiffList = {
    assert(!v.isInstanceOf[CollapsedVariable]) // We should never be processing a CollapsedVariable
    // Get factors, in sorted order of the their classname
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.template.getClass.getName < f2.template.getClass.getName)
    var done = false
    val handlerIterator = handlers.iterator
    val d = new DiffList
    while (!done && handlerIterator.hasNext) {
      done = handlerIterator.next.process(v, factors, this)(d)
    }
    if (!done) throw new Error("CollapsedVariationalBayes: No sampling method found for variable "+v+" with factors "+factors.map(_.template.getClass.getName).mkString("List(",",",")"))
    d
  }
}

trait CollapsedVariationalBayesHandler {
  def process(v:Variable, factors:Seq[Factor], cvb:CollapsedVariationalBayes)(implicit d:DiffList): Boolean
}

object GeneratedVariableCollapsedVariationalBayesHandler extends CollapsedVariationalBayesHandler {
  def process(v:Variable, factors:Seq[Factor], cvb:CollapsedVariationalBayes)(implicit d:DiffList): Boolean = {
    factors match {
      case Seq(factor1:GeneratedVarTemplate#Factor) => { 
        v match {
          case v:Discrete => {
            val parent = v.proportions
            val collapsedParent = cvb.collapsedp2[Proportions](parent) // possibly collapsed
            val vq:MutableProportions = cvb.q(v) // This will throw error if 'v' is not in our _q map.  // TODO Is this the behavior we want?
            val domainSize = v.domainSize
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
            val domainSize = v.domainSize
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
              forIndex(v.domainSize)(i => {
                v.set(i)(null)
                for (chosenParent <- outcome.chosenParents) cvb.collapsedOrNull(chosenParent) match {
                  case p:CollapsedParameter => { /*println("CollapsedGibbsSampler -"+vq+" p="+p); */ p.updateChildStats(outcomeQ, -vq(i)) }
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
              forIndex(v.domainSize)(i => {
                v.set(i)(null)
                for (chosenParent <- outcome.chosenParents) cvb.collapsedOrNull(chosenParent) match {
                  case p:CollapsedParameter => { /*println("CollapsedGibbsSampler +"+vq+" p="+p); */ p.updateChildStats(outcomeQ, vq(i)) }
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

