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
import scala.collection.mutable.{ArrayBuffer,Stack}

@DomainInSubclasses
trait MixtureChoiceVariable extends GeneratedDiscreteVariable with NoVariableCoordination {
  // 'outcomes' are a more efficient alternative to 'children' for small sets of outcomes.
  private var _outcomes: List[MixtureOutcome] = Nil
  def outcomes = _outcomes
  def addOutcome(o:MixtureOutcome): Unit = {
    //assert(o.mixtureSize == domainSize)
    _outcomes = o :: _outcomes
  }
  // To ensure that no subclassers attempt variable-value coordination
  // NOTE: Even though a MixtureChoiceVariable obviously effects the choice of parents of another variable
  // changes to the value of a MixtureChoiceVariable do not actually change the factor structure of the model.
  // @see GeneratedVarTemplate
  @inline final override def set(i:Int)(implicit d:DiffList): Unit = super.set(i)(d)
}
/** A mixture indicator discrete variable, with value generated from Proportions */
@DomainInSubclasses
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVariable
// TODO Consider instead:  With the two arg lists we can use the arguments of the first in the second.  Consider doing this for all initial values of GenerativeVariables
// No, because this would require syntax new MixtureChoice(proportions)().  Yuck.
//abstract class MixtureChoice(p:Proportions)(value:Int = p.sampleInt) extends Discrete(p, value) with MixtureChoiceVariable

/** A mixture indicator discrete variable, with value generated from a mixture of Proportions with component selected by 'choice' */
@DomainInSubclasses
abstract class MixtureChoiceMixture(ps:FiniteMixture[Proportions], choice:MixtureChoiceVariable, initialValue:Int = 0) extends DiscreteMixture(ps, choice, initialValue) with MixtureChoiceVariable


// TODO Consider renaming this "MixtureVar" or "MixtureGeneratedVar".  Yes!
/** A variable that has been generated from a mixture component.  
    The selection of component from the collection of mixture components is governed by the variable 'choice' */
trait MixtureOutcome extends GeneratedVar {
  //def components: Seq[MixtureComponents[_]]
  // Note that this restricts us to having just one MixtureChoice per MixtureOutcome; but this handles the common cases I can think of.
  def choice: MixtureChoiceVariable
  // Just those parents whose selection is controled by 'choice'
  def chosenParents: Seq[Parameter]
  def prFromMixtureComponent(index:Int): Double
  def prFromMixtureComponent(map:scala.collection.Map[Parameter,Parameter], index:Int): Double
  def parentsFromMixtureComponent(index:Int): Seq[Parameter]
}



trait MixtureComponents[+P<:Parameter] extends scala.collection.IndexedSeq[P] with SeqEqualsEq[P] with Parameter with GeneratedVar {
  override def isDeterministic = true
  def components: Seq[P]
  def length = components.length
  def apply(index:Int) = components(index)
  def parents = components
  def pr = 1.0 // TODO something else?
  def prFrom(parents:Seq[Parameter]) = 1.0 // TODO something else?
  def childrenOf[P2>:P](p:P2): Iterable[GeneratedVar] = {
    val index = components.indexOf(p)
    children.filter(_.isInstanceOf[MixtureOutcome]).asInstanceOf[Iterable[MixtureOutcome]].filter(_.choice.intValue == index)
  }
}


class FiniteMixture[+P<:Parameter](theComponents:Seq[P]) extends MixtureComponents[P] {
  val components = theComponents.toIndexedSeq
  components.foreach(_.addChild(this)(null))
}
object FiniteMixture {
  def apply[P<:Parameter](n:Int)(constructor: =>P): FiniteMixture[P] = new FiniteMixture[P](for (i <- 1 to n) yield constructor)
}


/** An arbitrary-sized set of mixture components that is actually the same component "repeated".
    Use, for example, when the mixture of Gaussians have different means, but should all share the same variance. */
// TODO Consider renaming UniformMixture
class UnaryMixture[+P<:Parameter](repeatedComponent:P) extends MixtureComponents[P] {
  val components = List(repeatedComponent)
  override def apply(index:Int) = repeatedComponent
  override def length = throw new Error("TODO: Not sure what length should be.")
  override def childrenOf[P2>:P](p:P2): Iterable[GeneratedVar] = children.filter(_.isInstanceOf[MixtureOutcome])
}
//object UnaryMixture { def apply[P<:Parameter](constructor: =>P): UnaryMixture[P] = new UnaryMixture[P](constructor) }

// TODO Not yet finished
class InfiniteMixture[P<:Parameter](constructor: =>P) extends MixtureComponents[P] {
  val components = new ArrayBuffer[P] // TODO Perhaps we should project this onto a seq without holes?
  private val _emptySlots = new Stack[Int]
  def addComponent: Int = 
    if (_emptySlots.isEmpty) { components += constructor; components.length - 1 }
    else { val i = _emptySlots.pop; components(i) = constructor; i }
}
//object InfiniteMixture { def apply[P<:Parameter](constructor: =>P) = new InfiniteMixture[P](constructor) }





// Old-style implementation of Mixtures, using Gates
/*
trait MixtureChoiceVariable extends GeneratedDiscreteVariable with Gate
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVariable
abstract class MixtureChoiceMixture(ps:Seq[Proportions], choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteMixture(ps, choice, value) with MixtureChoiceVariable

trait MixtureOutcome extends GeneratedVar {
  def prFromMixtureComponent(index:Int): Double
}
//class MixtureComponentRef[P<:Parameter,C<:MixtureOutcome](p:P, override val child:C) extends ParameterRef(p, child)

class MixtureChoiceVariableTemplate extends TemplateWithStatistics1[MixtureChoiceVariable] {
  def score(s:Stat) = 0 // s.s1.logpr comes from GeneratedVariableTemplate; gateRefs similarly
  //def score(s:Stat) = { val mc = s.s1; mc.gateRefs.reduceLeft((sum,ref) => sum + mc.value.logpr(ref.outcome)) }
}

trait DiscreteMixtureVariable extends GeneratedDiscreteVariable with MixtureOutcome {
  def choice: MixtureChoiceVariable
  def components: Seq[Proportions]
  private val proportionsRef: GatedParameterRef[Proportions,DiscreteMixtureVariable] = new GatedParameterRef(components, choice, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList = null) = { assert(p2 == null || p2.length <= domainSize); proportionsRef.set(p2) }
  override def parentRefs = List(proportionsRef)
  def prFromMixtureComponent(index:Int): Double = components(index).pr(intValue)
}
class DiscreteMixture(val components:Seq[Proportions], val choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteVariable(value) with DiscreteMixtureVariable 
class CategoricalMixture[A<:AnyRef](val components:Seq[Proportions], val choice:MixtureChoiceVariable, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] with DiscreteMixtureVariable 
*/
