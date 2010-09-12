/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random

@DomainInSubclasses
trait DiscreteMixtureVar extends GeneratedDiscreteVariable with MixtureOutcome {
  choice.addOutcome(this)
  components.addChild(this)(null)
  def choice: MixtureChoiceVariable
  def components: FiniteMixture[Proportions]
  def proportions = components(choice.intValue)
  def prFromMixtureComponent(index:Int): Double = components(index).pr(intValue)
  def prFromMixtureComponent(map:scala.collection.Map[Parameter,Parameter], index:Int): Double = map.getOrElse(components(index), components(index)).asInstanceOf[Proportions].pr(intValue)
  def parentsFromMixtureComponent(index:Int) = List(components(index))
  def chosenParents = List(components(choice.intValue))
  override def parents = components +: super.parents // super.parents match { case list:List[Parameter] => components :: list; case seq:Seq[Parameter] => components +: seq }
}

@DomainInSubclasses
class DiscreteMixture(val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable with MixtureOutcome with DiscreteMixtureVar

@DomainInSubclasses
class CategoricalMixture[A](val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] with DiscreteMixtureVar

// Outcome, MixtureChoice, Parents
// Common pattern:  mean -> MixtureComponents -> Gaussian




// Old-style

/*class DenseDirichletMixture(val components:Seq[Proportions], prec:RealValueParameter, val choice:MixtureChoiceVariable, p:Seq[Double] = Nil)
extends DenseDirichlet(components(choice.intValue), prec, p) with MixtureOutcome {
  override protected val meanRef: ParameterRef[Proportions,Dirichlet with MixtureOutcome] = new GatedParameterRef(components, choice, this)
  override def mean_=(p2:Proportions)(implicit d:DiffList = null) = throw new Error
  def prFromMixtureComponent(index:Int): Double = math.exp(logpr(components(index), precision))
}*/

