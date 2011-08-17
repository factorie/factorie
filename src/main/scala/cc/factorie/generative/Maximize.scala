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

/* Contains various recipes to maximize the value of variables to maximize some objective, 
   usually maximum likelihood. 
   @author Andrew McCallum */
class Maximize(val model:Model = GenerativeModel) {
  def defaultMaximizers = Seq(DiscreteMaximizer, GatedDiscreteMaximizer)
  val maximizers = new scala.collection.mutable.ArrayBuffer[Maximizer] ++ defaultMaximizers
  def apply(variables:Seq[GeneratedVar]): Unit = {
    // The handlers can be assured that the Seq[Factor] will be sorted alphabetically by class name
    val factors = model.factors(variables)
    // This next line does the maximization
    val option = maximizers.find(maximizer => maximizer.maximize(variables, factors))
    if (option == None) throw new Error("No maximizer found for factors "+factors.take(10).map(_ match { case f:Family#Factor => f.family.getClass; case f:Factor => f.getClass }).mkString)
  }
}
object Maximize extends Maximize(GenerativeModel)

trait Maximizer {
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def maximize(variables:Seq[Variable], factors:Seq[Factor]): Boolean
}

object DiscreteMaximizer extends Maximizer {
  def maximize(variables:Seq[Variable], factors:Seq[Factor]): Boolean = {
    if (factors.size != 1) return false
    assert(variables.size == 1)
    (variables.head, factors.head) match {
      case (v:MutableDiscreteVar, factor:Discrete.Factor) => { v.set(factor._2.value.maxInt)(null); true }
      case _ => false
    }
  }
}

object GatedDiscreteMaximizer extends Maximizer {
  def maximize(variables:Seq[Variable], factors:Seq[Factor]): Boolean = {
    if (variables.size != 1 || factors.size != 2 || !variables.head.isInstanceOf[Gate]) return false
    (variables.head, factors(1), factors(2)) match {
      case (gate:Gate, df:Discrete.Factor, dmf:DiscreteMixture.Factor) => {
        var max = Double.NegativeInfinity
        var maxi = 0
        val statistics = dmf.statistics(dmf.values)
        forIndex(gate.domain.size)(i => {
          val pr = df._2(i) * dmf.family.prChoosing(statistics, i)
          if (pr > max) { max = pr; maxi = i }
        })
        gate.set(maxi)(null)
        true
      }
      case _ => false
    }
  }
}

