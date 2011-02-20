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

trait MixtureGenerativeTemplate extends GenerativeTemplate {
  type TemplateType <: MixtureGenerativeTemplate
  type ChildType <: MixtureGeneratedVar
  def prChoosing(s:StatisticsType, mixtureIndex:Int): Double
  def prChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): Double = 
    prChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
  def logprChoosing(s:StatisticsType, mixtureIndex:Int): Double
  def logprChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): Double = 
    logprChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
  def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): ChildType#Value
  def sampledValueChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): ChildType#Value =
    sampledValueChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
}

trait MixtureGeneratedVar extends GeneratedVar {
  override val generativeTemplate: MixtureGenerativeTemplate
  def choice: MixtureChoiceVar
  def chosenParents: Seq[Parameter]
  def prChoosing(mixtureIndex:Int): Double = generativeTemplate.prChoosing(generativeFactor.statistics, mixtureIndex)
  def logprChoosing(mixtureIndex:Int): Double = generativeTemplate.logprChoosing(generativeFactor.statistics, mixtureIndex)
  def sampledValueChoosing(mixtureIndex:Int): Value = generativeTemplate.sampledValueChoosing(generativeFactor.statistics, mixtureIndex).asInstanceOf[Value]  // TODO Can we get rid of this cast?
}

trait MixtureChoiceVar extends MutableDiscreteVar with MutableGeneratedVar /*with MutableGeneratedDiscreteVar*/ {
  // 'outcomes' are a more efficient alternative to 'children' for small sets of outcomes.
  private var _outcomes: List[MixtureGeneratedVar] = Nil
  def outcomes = _outcomes
  def outcomesOfClass[A<:MixtureGeneratedVar](implicit m:Manifest[A]): Iterable[A] = outcomes.filter(o => m.erasure.isAssignableFrom(o.getClass)).asInstanceOf[Iterable[A]]
  def addOutcome(o:MixtureGeneratedVar): Unit = {
    //assert(o.mixtureSize == domainSize)
    _outcomes = o +: _outcomes
  }
  def removeOutcome(o:MixtureGeneratedVar): Unit = {
    _outcomes = _outcomes.filterNot(_ == o)
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


abstract class MixtureChoiceMux(p:Proportions, intValues:Seq[Int]) extends DiscreteMux(p, intValues) with MixtureChoiceVar {
  def prChoosing(value:Int): Double = p(value)
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

class MixtureComponentsTemplate extends GenerativeTemplateWithStatistics1[AbstractMixtureComponents] {
  def logpr(s:StatisticsType) = 0.0
  def pr(s:StatisticsType) = 1.0
  def sampledValue(s:StatisticsType) = throw new Error("Not yet implemented.")
}

trait AbstractMixtureComponents extends scala.collection.IndexedSeq[Parameter] with Parameter with GeneratedVar
trait MixtureComponents[+P<:Parameter] extends scala.collection.IndexedSeq[P] with AbstractMixtureComponents
with SeqEqualsEq[P] with Parameter with GeneratedVar with VarAndValueGenericDomain[MixtureComponents[P],Seq[P]] 
{
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

trait CollapsedMixtureComponents[+P<:CollapsedParameter] extends MixtureComponents[P]

class FiniteMixture[+P<:Parameter](theComponents:Seq[P]) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = theComponents.toIndexedSeq
  components.foreach(_.addChild(this)(null))
}
object FiniteMixture {
  def apply[P<:Parameter](n:Int)(constructor: =>P): FiniteMixture[P] = new FiniteMixture[P](for (i <- 1 to n) yield constructor)
}

class CollapsibleFiniteMixture[+P<:CollapsibleParameter](theComponents:Seq[P]) extends FiniteMixture(theComponents) with CollapsibleParameter with VarWithCollapsedType[CollapsedFiniteMixture[P#CollapsedType]] {
  def newCollapsed = new CollapsedFiniteMixture(components.map(_.newCollapsed))
  def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
    case cfm:CollapsedFiniteMixture[_] => forIndex(components.length)(i => components(i).setFrom(cfm(i)))
  }
}
object CollapsibleFiniteMixture {
  def apply[P<:CollapsibleParameter](n:Int)(constructor: =>P): CollapsibleFiniteMixture[P] = new CollapsibleFiniteMixture[P](for (i <- 1 to n) yield constructor)
}
class CollapsedFiniteMixture[+P<:CollapsedParameter](theComponents:Seq[P]) extends FiniteMixture(theComponents) with CollapsedParameter {
  def clearChildStats: Unit = components.foreach(_.clearChildStats)
  def updateChildStats(child:Variable, weight:Double): Unit = {
    child match {
      case mgv:MixtureGeneratedVar => {
        //println("CollapsedFiniteMixture.updateChildStats "+mgv+" choice="+mgv.choice)
        components(mgv.choice.intValue).updateChildStats(mgv, weight)
      }
      case _ => {} // TODO Should we really not throw an error here?
    }
  }
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
