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

package cc.factorie.model

import cc.factorie._
import cc.factorie.util.SingletonIndexedSeq
import cc.factorie.variable.{DiscreteVar, Var}

import scala.collection.mutable.Set

// Factor Templates are able to create factors in a factor graph on-the-fly as necessary.
// A factor template specifies:
// (1) a description of the arbitrary relationship among its variable neighbors
// (2) a sufficient statistics function that maps those neighbors to the statistics necessary to return a real-valued score
// (3) an aggregator for multiple statistics of the same template
// (4) a function mapping those aggregated statistics to a real-valued score
// (5) optionally, the parameters used in the function to calculate that score;
//     (alternatively the score may be calculated using parameter stored externally to the Template,
//      or in some fixed way without learned parameters).

// Future, discussed with Sebastian:
// class SymbolicTemplate extends NeighborAwareTemplate[SymbolicPredicate] 
// Term (e.g. real-valued term), tree-based expression, sub-expressions
//  depends on notion of state: each template/term takes a state and maps to double-valued score
// SymbolicPredicate, SymbolicConstant, LogicalGroundAtom extends BooleanVariable with Seq[SymbolicConstant]



/** A template for creating Factors.  The creation of Factors is keyed by neighboring variables. 
    Superclass of Template1, Template2, Template3, Template4.
    They are templates that are also Model's with Variable context.
    Its subclasses use "unroll*" methods, which given on of the Factor's variables, finding the other neighboring variables.
    The "unroll(Var)" method here can be used (or overridden in subclasses) to return Factors keyed by non-neighbor variables.
    @author Andrew McCallum
*/
trait Template extends FamilyWithNeighborDomains with FamilyWithNeighborClasses with Model {
  type FactorType <: Factor

  /** Implement this method to create and return all Factors of this Template touching the given Var.
      Users of the Template should not call this method directly, however, because 
      this method's implementation is permitted to return duplicate Factors.  
      Instead call factors(Var), which will deduplicate the Factors (using the helper function addFactors).*/
  def unroll(v:Var): Iterable[FactorType]

  /** Not intended to be called by users. This method calls unroll and efficiently deduplicates the resulting
      Factors by adding them to the result Set. */
  override def addFactors(v:Var, result:Set[model.Factor]): Unit = {
    unroll(v) match { case fs:IterableSingleFactor[_] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  // TODO In case we make a Template that inherits from ModelWithContext.  Should this be TemplateWithContext? 
  //def addFactorsOfContext(c:Variable, result:Set[cc.factorie.Factor]): Unit = addFactors(c, result)
  
  /** Users should call this method to create and return all Factors of this Template touching the given Var. */
  override final def factors(v:Var): Iterable[FactorType] = {
    val result = new collection.mutable.LinkedHashSet[model.Factor]
    addFactors(v, result)
    result.asInstanceOf[Iterable[FactorType]]
  }
  
  final def factors(variables:Iterable[Var]): Iterable[FactorType] = variables match {
    case variables:SingletonIndexedSeq[Var] => factors(variables.elt)
    case variables => {
      val result = new collection.mutable.LinkedHashSet[model.Factor]
      variables.foreach(v => addFactors(v, result))
      result.asInstanceOf[Iterable[FactorType]]
    }
  }
    
  /** A Factor Template has just one Factor Family: itself. */
  def families: Seq[Family] = Seq(Template.this)

  /** Causes future calls to factor.valuesIterator to limit the returned values to 
      those value combinations seen in the current values of the variables in factors touching "vars".
      Note that this will limit the values of all DiscreteVar neighbors of the resulting factors,
      not just the "vars" argument. */
  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit
  // Alternative name? -akm
  //def addLimitedDiscreteCurrentValues(vars:Iterable[Var]): Unit
}

