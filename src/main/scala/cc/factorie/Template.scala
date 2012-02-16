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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._

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


/** The template for creating factors, given on of its variables, finding the other neighboring variables.
    @Andrew McCallum
*/
trait Template extends FamilyWithNeighborDomains { thisTemplate =>
  //type FamilyType <: Template // like a self-type
  /** If true, method "factors" will only create Factors for variables whose domains match neighborDomains. */
  var matchNeighborDomains = true
  def factors(v: Variable): Iterable[FactorType] // TODO Consider returning Iterable[Factor]
  /**A version of factors that takes the Diff object instead of just the variable */
  def factors(d: Diff): Iterable[FactorType] = if (d.variable == null) Nil else factors(d.variable)
  def factors(difflist: DiffList): Iterable[FactorType] = {
    //var result = new LinkedHashSet[Factor]()
    var result = new HashSet[FactorType]()
    for (diff <- difflist; factor <- factors(diff)) { if (factor eq null) throw new Error("Template.factors returned null Factor") else result += factor }
    //difflist.foreach(diff => result ++= factors(diff))
    result
  }
  def factors(variables:Iterable[Variable]): Iterable[FactorType] = {
    if (variables.size == 1) return factors(variables.head) // Efficiently avoids the HashSet.
    //var result = new LinkedHashSet[FactorType]()
    var result = new HashSet[FactorType]()
    for (v <- variables; factor <- factors(v)) { if (factor eq null) throw new Error("Template.factors returned null Factor") else result += factor }
    result
  }
  /** Called in implementations of factors(Variable) to give the variable a chance
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Variable): Iterable[Variable] = v.unrollCascade
  
  /** Causes future calls to factor.valuesIterator to limit the returned values to 
      those value combinations seen in the current values of the variables in factors touching "vars". */
  def limitDiscreteValuesIteratorAsIn(vars:Iterable[DiscreteVar]): Unit = {}
}


trait VectorTemplate extends VectorFamily with Template



trait DotTemplate extends DotFamily with Template {
  //type FamilyType <: DotTemplate
}



// Shortcuts for templates whose statistics are a subset of their neighbors, coming from the end of the neighbor list.
// AKM: Let me know if you think it would be more sensible to have them come from the beginning instead.

abstract class Template2WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest] extends Template2[N1,N2] with Statistics1[N2#Value] {
  def statistics(v:Values) = Stat(v._2)
}
abstract class Template3WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics1[N3#Value] {
  def statistics(v:Values) = Stat(v._3)
}
abstract class Template3WithStatistics2[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics2[N2#Value,N3#Value] {
  def statistics(v:Values) = Stat(v._2, v._3)
}

/*
trait FooTemplate3 {
  def valuesIterator(value1:Option[N1#Value], value2:Option[N2#Value], value3:Option[N3#Value]): Iterator[this.type.Values]
  def discreteValuesIterator(valueOptions: Option[DiscreteValue]*): Iterator[Values]
  def discreteValuesIterator(factor:Factor, fixed: (Variable,Any)*): Iterator[Values]
  def discreteValuesIterator(factor:Factor, vary:Iterable[DiscreteVar]): Iterator[Values]
  //def argMaxMessage(f:Factor, m1:N1#MessageType, m2:N2#MessageType)
}
*/

//class TreeTemplate extends Template1[Vars[EdgePresence]] { }

//class FooTemplate {}
//val t1 = new FooTemplate
//t1.statistics(values:t1.Values)
