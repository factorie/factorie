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

package cc.factorie

import collection.mutable.HashSet
import scala.collection.mutable.Set

/** A template for creating Factors.  The creation of Factors is keyed by some context of arbitrary type C. */
// TODO No need for this.  Just use ModelWithFactorType
//trait Template[C,F<:Factor] {
//  //type FactorType <: Factor
//  def addFactorsOfContext(c:C, result:Set[F]): Unit 
//  def factorsOfContext(c:C): Iterable[F] = { val result = newContextFactorsCollection; addFactorsOfContext(c, result); result }
//  // TODO Move this to Model
//  //def itemizedModel(c:C): ItemizedModel = new ItemizedModel(factorsOfContext(c))
//  protected def newContextFactorsCollection: Set[F] = new collection.mutable.LinkedHashSet[F]
//}

// Future, discussed with Sebastian:
// class SymbolicTemplate extends NeighborAwareTemplate[SymbolicPredicate] 
// Term (e.g. real-valued term), tree-based expression, sub-expressions
//  depends on notion of state: each template/term takes a state and maps to double-valued score
// SymbolicPredicate, SymbolicConstant, LogicalGroundAtom extends BooleanVariable with Seq[SymbolicConstant]



object Template {
  var enableCachedStatistics: Boolean = true
}

// Factor Templates are able to create factors in a factor graph on-the-fly as necessary.
// A factor template specifies:
// (1) a description of the arbitrary relationship among its variable neighbors
// (2) a sufficient statistics function that maps those neighbors to the statistics necessary to return a real-valued score
// (3) an aggregator for multiple statistics of the same template
// (4) a function mapping those aggregated statistics to a real-valued score
// (5) optionally, the parameters used in the function to calculate that score;
//     (alternatively the score may be calculated using parameter stored externally to the Template,
//      or in some fixed way without learned parameters).


/** Superclass of Template1, Template2, Template3, Template4.
    They are templates that are also Model's with Variable context.
    Its subclasses use "unroll*" methods, which given on of the Factor's variables, finding the other neighboring variables.
    @author Andrew McCallum
*/
trait ModelAsTemplate extends Model with FamilyWithNeighborDomains with FamilyWithNeighborClasses { thisTemplate =>
  // TODO This method is a little messy.  Clean up this situation. -akm
  def addFactors(v:Var, result:Set[cc.factorie.Factor]): Unit
  //def addFactorsOfContext(c:Variable, result:Set[cc.factorie.Factor]): Unit = addFactors(c, result)
  def factors(v:Var): Iterable[FactorType] = {
    val result = new collection.mutable.LinkedHashSet[cc.factorie.Factor]
    addFactors(v, result)
    result.asInstanceOf[Iterable[FactorType]]
  }

  /** Called in implementations of factors(Variable) to give the variable a chance
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Var): Iterable[Var] = v.unrollCascade
  def tryCascade: Boolean = true //hasContainerNeighbors
  def hasContainerNeighbors: Boolean = {
    println("TemplateModel hasContainerNeighbors neighborClasses: "+neighborClasses.mkString(","))
//    this match {
//      case t:Template1[_] => {
//        println("TemplateModel Template1 neighborClass1 = "+t.neighborClass1)
//        //println("TemplateModel Template1 nm1.erasure = "+t.nm1.erasure)
//      }
//      case _ => {}
//    }
    //neighborClasses.exists(classOf[ContainerVariable[_]].isAssignableFrom(_))
    throw new Error("Not yet implemented.  Why am I getting nulls in neighborClasses?")
    false // TODO Fix the above line
  }
  
  /** Causes future calls to factor.valuesIterator to limit the returned values to 
      those value combinations seen in the current values of the variables in factors touching "vars". */
  def limitDiscreteValuesIteratorAsIn(vars:Iterable[DiscreteVar]): Unit = {}
  def families: Seq[Family] = Seq(this)
}

