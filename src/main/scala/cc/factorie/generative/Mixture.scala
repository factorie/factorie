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

trait MixtureGenerativeTemplate[V<:MixtureGeneratedVar] extends GenerativeTemplate[V] {
  def prChoosing(s:StatType, mixtureIndex:Int): Double
  def logprChoosing(s:StatType, mixtureIndex:Int): Double
  def sampledValueChoosing(s:StatType, mixtureIndex:Int): V#Value
}

trait MixtureGeneratedVar extends GeneratedVar {
  override val generativeTemplate: MixtureGenerativeTemplate[_<:MixtureGeneratedVar]
  //def generativeFactor: MixtureGenerativeTemplate[MixtureGeneratedVar]#Factor
  // TODO Needed?  Remove this method from subclasses?
  //def components: MixtureComponents[Parameter]
  def choice: MixtureChoiceVar
  def chosenParents: Seq[Parameter]
  // TODO Can we get rid of these casts?
  def prChoosing(mixtureIndex:Int): Double = throw new Error //generativeTemplate.prChoosing(generativeFactor.statistics.asInstanceOf[generativeTemplate.StatType], mixtureIndex)
  def logprChoosing(mixtureIndex:Int): Double = throw new Error // generativeTemplate.logprChoosing(generativeFactor.statistics.asInstanceOf[generativeTemplate.StatType], mixtureIndex)
  def sampledValueChoosing(mixtureIndex:Int): Value = throw new Error //generativeTemplate.sampledValueChoosing(generativeFactor.statistics.asInstanceOf[generativeTemplate.StatType], mixtureIndex)
}

trait MixtureChoiceVar extends DiscreteVariable with MutableVar {
  // 'outcomes' are a more efficient alternative to 'children' for small sets of outcomes.
  private var _outcomes: List[MixtureGeneratedVar] = Nil
  def outcomes = _outcomes
  def outcomesOfClass[A<:MixtureGeneratedVar](implicit m:Manifest[A]): Iterable[A] = outcomes.filter(o => m.erasure.isAssignableFrom(o.getClass)).asInstanceOf[Iterable[A]]
  def addOutcome(o:MixtureGeneratedVar): Unit = {
    //assert(o.mixtureSize == domainSize)
    _outcomes = o :: _outcomes
  }
  def prChoosing(value:Int): Double
  // To ensure that no subclassers attempt variable-value coordination
  // NOTE: Even though a MixtureChoiceVariable obviously effects the choice of parents of another variable
  // changes to the value of a MixtureChoiceVariable do not actually change the factor structure of the model.
  // @see GeneratedVarTemplate
  //@inline final override def set(i:DiscreteValue)(implicit d:DiffList): Unit = super.set(i)(d)
}

/** A mixture indicator discrete variable, with value generated from Proportions */
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVar {
  def prChoosing(value:Int): Double = p(value)
}

/** A mixture indicator discrete variable, with value generated from a mixture of Proportions with component selected by 'choice' */
abstract class MixtureChoiceMixture(ps:FiniteMixture[Proportions], choice:MixtureChoiceVar, value:Int = 0) extends DiscreteMixture(ps, choice, value) with MixtureChoiceVar {
  //def prChoosing(value:Int): Double = ps(choice.intValue).apply(value)
  def prChoosing(value:Int, mixtureIndex:Int): Double = ps(mixtureIndex).apply(value)
}

/*class MixtureComponentsTemplate[P<:Parameter:Manifest] extends GenerativeTemplateWithStatistics2[MixtureComponents[P],Vars[P]] {
  def unroll1(m:MixtureComponents[P]) = Factor(m, Vars.fromSeq(m.components))
  def unroll2(v:Vars[P]) = throw new Error
  override def unroll2s(p:P) = for (m <- p.childrenOfClass[MixtureComponents[P]]) yield Factor(m, Vars.fromSeq(m.components))
  //def unroll2(p:Nothing) = throw new Error
  def logpr(s:StatType) = 0.0
  def pr(s:StatType) = 1.0
  def sampledValue(s:StatType) = throw new Error("Not yet implemented.")
}*/

class MixtureComponentsTemplate extends GenerativeTemplateWithStatistics1[MixtureComponents[Parameter]] {
  def logpr(s:StatType) = 0.0
  def pr(s:StatType) = 1.0
  def sampledValue(s:StatType) = throw new Error("Not yet implemented.")
}

trait MixtureComponents[+P<:Parameter] extends scala.collection.IndexedSeq[P] with SeqEqualsEq[P] with Parameter with GeneratedVar with VarAndValueGenericDomain[MixtureComponents[P],Seq[P]] {
  override def isDeterministic = true
  def components: IndexedSeq[P]
  def value = components.asInstanceOf[Value]
  def length = components.length
  def apply(index:Int) = components(index)
  override def parents = components
  override def pr = 1.0 // TODO something else?
  /** Return the MixtureGeneratedVar's whose corresponding 'choice' indicates that they were generated by component 'p' */
  def childrenOf[P2>:P](p:P2): Iterable[MixtureGeneratedVar] = {
    val index = components.indexOf(p)
    children.filter(_.isInstanceOf[MixtureGeneratedVar]).asInstanceOf[Iterable[MixtureGeneratedVar]].filter(_.choice.intValue == index)
  }
}


class FiniteMixture[+P<:Parameter:Manifest](theComponents:Seq[P]) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = theComponents.toIndexedSeq
  components.foreach(_.addChild(this)(null))
}
object FiniteMixture {
  def apply[P<:Parameter:Manifest](n:Int)(constructor: =>P): FiniteMixture[P] = new FiniteMixture[P](for (i <- 1 to n) yield constructor)
}


/** An arbitrary-sized set of mixture components that is actually the same component "repeated".
    Use, for example, when the mixture of Gaussians have different means, but should all share the same variance. */
// TODO Consider renaming UniformMixture
class UnaryMixture[+P<:Parameter:Manifest](repeatedComponent:P) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = IndexedSeq(repeatedComponent)
  override def apply(index:Int) = repeatedComponent
  override def length = throw new Error("TODO: Not sure what length should be.")
  override def childrenOf[P2>:P](p:P2): Iterable[MixtureGeneratedVar] = children.filter(_.isInstanceOf[MixtureGeneratedVar]).asInstanceOf[Iterable[MixtureGeneratedVar]]
}
//object UnaryMixture { def apply[P<:Parameter](constructor: =>P): UnaryMixture[P] = new UnaryMixture[P](constructor) }

// TODO Not yet finished
class InfiniteMixture[P<:Parameter:Manifest](constructor: =>P) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = new ArrayBuffer[P] // TODO Perhaps we should project this onto a seq without holes?
  private val _emptySlots = new Stack[Int]
  def addComponent: Int = 
    if (_emptySlots.isEmpty) { components += constructor; components.length - 1 }
    else { val i = _emptySlots.pop; components(i) = constructor; i }
}
//object InfiniteMixture { def apply[P<:Parameter](constructor: =>P) = new InfiniteMixture[P](constructor) }
