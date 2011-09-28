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
class Collapse(val model:Model = GenerativeModel) {
  val collapsers = new ArrayBuffer[Collapser] ++= Seq(DenseCountsProportionsCollapser, DenseCountsProportionsMixtureCollapser)
  def apply(variables:Seq[GeneratedVar]): Unit = {
    val factors = model.factors(variables)
    // This next line does the collapsing
    val option = collapsers.find(_.collapse(variables, factors))
    if (option == None) throw new Error("No collapser found for factors "+factors.take(10).map(_ match { case f:Family#Factor => f.family.getClass; case f:Factor => f.getClass }).mkString)
  }
}
object Collapse extends Collapse(GenerativeModel) 

trait Collapser {
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def collapse(variables:Seq[Variable], factors:Seq[Factor]): Boolean
}

object DenseCountsProportionsCollapser extends Collapser {
  def collapse(variables:Seq[Variable], factors:Seq[Factor]): Boolean = {
    if (variables.size != 1) return false
    variables.head match {
      case p:DenseCountsProportions => {
        p.zero()
        for (f <- factors) f match {
          //case f:Discrete.Factor if (f.family == Discrete) => p.increment(f._1.intValue, 1.0)(null)
          case f:Discrete.Factor => p.increment(f._1.intValue, 1.0)(null)
          //case f:PlatedDiscrete.Factor => forIndex(f._1.length)(i => f._2.asInstanceOf[DenseCountsProportions].increment(f._1(i).intValue, 1.0)(null))
          case f:PlatedDiscrete.Factor => forIndex(f._1.length)(i => p.increment(f._1(i).intValue, 1.0)(null))
          //case f:Dirichlet.Factor if (f.family == Dirichlet) => p.increment(f._2)(null)
          case f:Dirichlet.Factor => p.increment(f._2)(null)
          case _ => { println("DenseCountsProportionsCollapser unexpected factor "+f); return false }
        }
        true
      }
      case _ => false
    }
  }
}

object DenseCountsProportionsMixtureCollapser extends Collapser {
  def collapse(variables:Seq[Variable], factors:Seq[Factor]): Boolean = {
    if (variables.size != 1) return false
    variables.head match {
      case m:Mixture[DenseCountsProportions] => {
        if (!m(0).isInstanceOf[DenseCountsProportions]) return false // Because JVM erasure doesn't actually check the [DenseCountsProportions] above
        m.foreach(p => { p.zero(); p.parentFactor match { case f:Dirichlet.Factor => p.increment(f._2)(null) } } )
        // TODO We really should create a mechanism indicating that a variable/factor is deterministic 
        //  and GenerativeModel.normalize should expand the factors to include neighbors of these,
        //  then include Dirichlet.factor in the match statement below.
        for (f <- factors) f match {
          //case f:MixtureComponent.Factor => {}
          case f:Mixture.Factor => {}
          case f:DiscreteMixture.Factor => m(f._3.intValue).increment(f._1.intValue, 1.0)(null)
          case f:PlatedDiscreteMixture.Factor => forIndex(f._1.size)(i => m(f._3(i).intValue).increment(f._1(i).intValue, 1.0)(null))
          case f:Factor => { println("DenseCountsProportionsMixtureCollapser unexpected factor "+f); return false }
        }
        true
      }
      case _ => false
    }
  }
}

