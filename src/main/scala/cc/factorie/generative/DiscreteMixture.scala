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
  def prFromMixtureComponent(map:scala.collection.Map[Parameter,Parameter], index:Int): Double = map.getOrElse(components(index), components(index)).asInstanceOf[Proportions].pr(intValue) // TODO make more efficient
  def parentsFromMixtureComponent(index:Int) = List(components(index))
  def chosenParents = List(components(choice.intValue))
  override def parents = List(components)
    // Above was: components +: super.parents // super.parents match { case list:List[Parameter] => components :: list; case seq:Seq[Parameter] => components +: seq }
}

@DomainInSubclasses
class DiscreteMixture(val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable with MixtureOutcome with DiscreteMixtureVar

@DomainInSubclasses
class CategoricalMixture[A](val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] with DiscreteMixtureVar

