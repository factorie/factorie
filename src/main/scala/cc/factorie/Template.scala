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

/** A template for creating Factors.  The creation of Factors is keyed by some context of arbitrary type C. */
// TODO Make this ContextTemplate[C,F<:Factor] because this is like a Map from C to Fs.
trait ContextTemplate[C] extends FamilyWithNeighborDomains {
  def factors(c:C): Iterable[FactorType]
  //def factors(cs:Iterable[C]): Iterable[FactorType] = dedup(cs.flatMap(factors(_))) // TODO Do we also want a method like this?
  def itemizedModel(c:C): ItemizedModel = new ItemizedModel(factors(c))
}

/** A template for creating Factors, where there is a mapping from variables neighboring the factors to the contexts necessary to create those Factors. */
trait NeighborAwareTemplate[C] extends ContextTemplate[C] with Model {
  // Map from a variable back to a context from which we can get its neighboring factors
  def contexts(v:Variable): Iterable[C]
  //def factorsWithDuplicates(v:Variable): Iterable[FactorType] = context(v).flatMap(factors(_))
}

// Future:
// class SymbolicTemplate extends NeighborAwareTemplate[SymbolicPredicate] 
// Term (e.g. real-valued term), tree-based expression, sub-expressions
//  depends on notion of state: each template/term takes a state and maps to double-valued score
// SymbolicPredicate, SymbolicConstant, LogicalGroundAtom extends BooleanVariable with Seq[SymbolicConstant]

// class Templates[C] extends scala.collection.mutable.ArrayBuffer[TemplateA[C]] { def factors(c:C) = flatMap(_.factors(c)) }

// Example usage of ContextTemplate: 
//class ChainTransitionTemplate[Y<:DiscreteVar](val yDomain:DiscreteDomain) extends FamilyWithDotStatistics2[Y,Y] with ContextTemplate[Seq[Y]] {
//  def statisticsDomains = ((yDomain , yDomain))
//  def factors(c:Seq[Y]) = for (pair <- c.sliding(2).toSeq) yield Factor(pair(0), pair(1)) 
//}
//class ChainTransitionTemplateAlt[Y<:DiscreteVar](val yDomain:DiscreteDomain, val seq:Seq[Y]) extends FamilyWithDotStatistics2[Y,Y] with ContextTemplate[Int] {
//  def statisticsDomains = ((yDomain , yDomain))
//  def factors(c:Int) = Factor(seq(c-1), seq(c)) 
//}
//class ChainObservationTemplate[Y<:DiscreteVar,X<:DiscreteTensorVar](val yDomain:DiscreteDomain, xDomain:DiscreteTensorDomain) extends FamilyWithDotStatistics2[Y,X] with ContextTemplate[(Seq[Y],Seq[X])] {
//  def statisticsDomains = ((yDomain , xDomain))
//  def factors(c:(Seq[Y],Seq[X])) = for (pair <- c._1.zip(c._2)) yield Factor(pair._1, pair._2) 
//}
//object ContextTemplateTest {
//  object LabelDomain extends CategoricalDomain[String]
//  class Label(s:String) extends LabelVariable(s) { def domain = LabelDomain }
//  object FeaturesDomain extends CategoricalTensorDomain[String]
//  class Features(f:Seq[String]) extends BinaryFeatureVectorVariable(f) { def domain = FeaturesDomain }
//  val tt = new ChainTransitionTemplate[Label](LabelDomain)
//  val ot = new ChainObservationTemplate[Label,Features](LabelDomain, FeaturesDomain)
//  val labels = for (i <- 0 until 10) yield new Label(i.toString)
//  val features = for (i <- 0 until 10) yield new Features(Seq(i.toString))
//  val model = new FactorModel(tt.factors(labels) ++ ot.factors((labels, features)))
//}


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


/** The template for creating factors, using "unroll*" methods which given on of the Factor's variables, finding the other neighboring variables.
    @author Andrew McCallum
*/
// TODO Make this Template[F] because this is a container/generator of Factors F
trait Template extends NeighborAwareTemplate[Variable] { thisTemplate =>
  def contexts(v:Variable): Iterable[Variable] = Seq(v)
  // Member type FactorType is defined so we will know the Factor type, which enables code like "factor.statistics.vector"
  override def factorsWithDuplicates(v:Variable): Iterable[FactorType]
  override def factorsWithDuplicates(vs:Iterable[Variable]): Iterable[FactorType] = super.factorsWithDuplicates(vs).asInstanceOf[Iterable[FactorType]]
  override def factorsWithDuplicates(d:Diff): Iterable[FactorType] = super.factorsWithDuplicates(d).asInstanceOf[Iterable[FactorType]] //if (d.variable == null) Nil else factors(d.variable)
  override def factorsWithDuplicates(difflist:DiffList): Iterable[FactorType] = super.factorsWithDuplicates(difflist).asInstanceOf[Iterable[FactorType]]
  override def factors(v:Variable): Iterable[FactorType] = super.factors(v).asInstanceOf[Iterable[FactorType]]
  override def factors(variables:Iterable[Variable]): Iterable[FactorType] = super.factors(variables).asInstanceOf[Iterable[FactorType]]
  override def factors(d:Diff): Iterable[FactorType] = super.factors(d).asInstanceOf[Iterable[FactorType]] //if (d.variable == null) Nil else factors(d.variable)
  override def factors(difflist:DiffList): Iterable[FactorType] = super.factors(difflist).asInstanceOf[Iterable[FactorType]]
  /** Called in implementations of factors(Variable) to give the variable a chance
      to specify additional dependent variables on which factors(Variable) should also be called. */
  def unrollCascade(v:Variable): Iterable[Variable] = v.unrollCascade
  
  /** Causes future calls to factor.valuesIterator to limit the returned values to 
      those value combinations seen in the current values of the variables in factors touching "vars". */
  def limitDiscreteValuesIteratorAsIn(vars:Iterable[DiscreteVar]): Unit = {}
  override def families: Seq[Family] = Seq(this)
}



// Shortcuts for templates whose statistics are a subset of their neighbors, coming from the end of the neighbor list.
// AKM: Let me know if you think it would be more sensible to have them come from the beginning instead.

//abstract class Template2WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest] extends Template2[N1,N2] with Statistics1[N2#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value) = Stat(v2)
//}
//abstract class Template3WithStatistics1[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics1[N3#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Stat(v3)
////  def statistics(v:Values) = Stat(v._3)
//}
//abstract class Template3WithStatistics2[N1<:Variable:Manifest,N2<:Variable:Manifest,N3<:Variable:Manifest] extends Template3[N1,N2,N3] with Statistics2[N2#Value,N3#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Stat(v2, v3)
////  def statistics(v:Values) = Stat(v._2, v._3)
//}



