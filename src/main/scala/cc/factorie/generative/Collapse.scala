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
import scala.collection.mutable.ArrayBuffer

/* Contains various recipes that "collapse" variables 
   by setting the value of variables to an internal state 
   that allows CollapsedGibbsSampler or CollapsedVariationalBayes to treat
   them as collapsed for their inference.
   @author Andrew McCallum */
class Collapse(val model:GenerativeModel) {
  val collapsers = new ArrayBuffer[Collapser] ++= Seq(DenseCountsProportionsCollapser, DenseCountsProportionsMixtureCollapser)
  def apply(variables:Seq[Variable]): Unit = {
    val factors = model.factors(variables)
    // This next line does the collapsing
    val option = collapsers.find(_.collapse(variables, factors, model))
    if (option == None) throw new Error("No collapser found for factors "+factors.take(10).map(_ match { case f:Family#Factor => f.family.getClass; case f:Factor => f.getClass }).mkString(" "))
  }
}
//object Collapse extends Collapse(defaultGenerativeModel) 

trait Collapser {
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def collapse(variables:Seq[Variable], factors:Seq[Factor], model:GenerativeModel): Boolean
}

object DenseCountsProportionsCollapser extends Collapser {
  def collapse(variables:Seq[Variable], factors:Seq[Factor], model:GenerativeModel): Boolean = {
    if (variables.size != 1) return false
    variables.head match {
      case p:ProportionsVar => {
        p.tensor.zero()
        for (f <- factors) f match {
          //case f:Discrete.Factor if (f.family == Discrete) => p.increment(f._1.intValue, 1.0)(null)
          case f:Discrete.Factor => p.tensor.+=(f._1.intValue, 1.0)
          //case f:PlatedDiscrete.Factor => forIndex(f._1.length)(i => f._2.asInstanceOf[DenseCountsProportions].increment(f._1(i).intValue, 1.0)(null))
          case f:PlatedDiscrete.Factor => forIndex(f._1.length)(i => p.tensor.+=(f._1(i).intValue, 1.0))
          //case f:Dirichlet.Factor if (f.family == Dirichlet) => p.increment(f._2)(null)
          case f:Dirichlet.Factor => p.tensor match {
            case pt:DenseProportions1 => pt.+=(f._2.tensor)
            case pt:SortedSparseCountsProportions1 if (model.parentFactor(p) eq f) => pt.prior = f._2.tensor
          }
          case _ => { println("DenseCountsProportionsCollapser unexpected factor "+f); return false }
        }
        true
      }
      case _ => { /* println("DenseCountsProportionsCollapser unexpected Proportions "+variables.head.getClass);*/ false }
    }
  }
}

object DenseCountsProportionsMixtureCollapser extends Collapser {
  def collapse(variables:Seq[Variable], factors:Seq[Factor], model:GenerativeModel): Boolean = {
    if (variables.size != 1) return false
    variables.head match {
      case m:Mixture[ProportionsVar] => {
        if (!m(0).isInstanceOf[ProportionsVar]) return false // Because JVM erasure doesn't actually check the [DenseCountsProportions] above
        m.foreach(p => { p.tensor.zero(); model.parentFactor(p) match { case f:Dirichlet.Factor => p.tensor.+=(f._2.tensor) } } )
        // TODO We really should create a mechanism indicating that a variable/factor is deterministic 
        //  and GenerativeModel.normalize should expand the factors to include neighbors of these,
        //  then include Dirichlet.factor in the match statement below.
        for (f <- factors) f match {
          //case f:MixtureComponent.Factor => {}
          case f:Mixture.Factor => {}
          case f:DiscreteMixture.Factor => m(f._3.intValue).tensor.+=(f._1.intValue, 1.0)
          case f:PlatedDiscreteMixture.Factor => forIndex(f._1.length)(i => m(f._3(i).intValue).tensor.+=(f._1(i).intValue, 1.0))
          case f:Factor => { println("DenseCountsProportionsMixtureCollapser unexpected factor "+f); return false }
        }
        true
      }
      case _ => false
    }
  }
}

