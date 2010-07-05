/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random

trait MixtureChoiceVariable extends GeneratedDiscreteVariable with Gate
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVariable
abstract class MixtureChoiceMixture(ps:Seq[Proportions], choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteMixture(ps, choice, value) with MixtureChoiceVariable

trait MixtureOutcome extends GeneratedValue {
  def prFromMixtureComponent(index:Int): Double
}
//class MixtureComponentRef[P<:Parameter,C<:MixtureOutcome](p:P, override val child:C) extends ParameterRef(p, child)
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

/*class DenseDirichletMixture(val components:Seq[Proportions], prec:RealValueParameter, val choice:MixtureChoiceVariable, p:Seq[Double] = Nil)
extends DenseDirichlet(components(choice.intValue), prec, p) with MixtureOutcome {
  override protected val meanRef: ParameterRef[Proportions,Dirichlet with MixtureOutcome] = new GatedParameterRef(components, choice, this)
  override def mean_=(p2:Proportions)(implicit d:DiffList = null) = throw new Error
  def prFromMixtureComponent(index:Int): Double = math.exp(logpr(components(index), precision))
}*/



class MixtureChoiceVariableTemplate extends TemplateWithStatistics1[MixtureChoiceVariable] {
  def score(s:Stat) = 0 // s.s1.logpr comes from GeneratedVariableTemplate; gateRefs similarly
  //def score(s:Stat) = { val mc = s.s1; mc.gateRefs.reduceLeft((sum,ref) => sum + mc.value.logpr(ref.outcome)) }
}



/*
trait MixtureComponent extends Parameter {
  def parent: MixtureComponents[P]
  override def addChild(v:GeneratedValue)(implicit d:DiffList): Unit = parent.addChild(v)
  override def removeChild(v:GeneratedValue)(implicit d:DiffList): Unit = parent.removeChild(v)
  override def children: Iterable[GeneratedValue] = parent.childrenOf(this)
  def weightedChildren: Iterable[(MixtureOutcome,Double)]
}
trait MixtureComponents[P<:Parameter] extends Seq[P] with Parameter
class FiniteMixture[P<:Parameter](val components:Seq[P]) extends MixtureComponents[P] {
  components.foreach(_.addChild(this))
  def length = components.length
  def apply(index:Int) = components(index)
  def childrenOf(p:P): Iterable[GeneratedValue] = {
    val index = components.indexOf(p)
    children.filter(_.isInstanceOf[MixtureOutcome]).asInstanceOf[Iterable[MixtureOutcome]].filter(_.choice.intValue == index)
  }
}
object FiniteMixture {
  def apply[P<:Parameter](n:Int)(constructor: =>P): FiniteMixture[P] = new FiniteMixture[P](for (i <- 1 to n) yield constructor())
}
class DiscreteMixture(val components:FiniteMixture[Proportions], val choice:MixtureChoice, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable with MixtureOutcome {
  choice.addChild(this)
  def proportions = components(choice.intValue)
  def prFromMixtureComponent(index:Int) = components(index).pr(intValue)
}

class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with IntegerValueParameter

trait MixtureOutcome extends GeneratedValue {
  def choice: MixtureChoice
  def prFromMixtureComponent(index:Int): Double
}

class MixtureChoiceTemplate extends TemplateWithStatistics3s[GeneratedValue,MixtureChoice,Parameter] { 
  def unroll1(v:GeneratedValue) = Factor(v, v match { case v:MixtureOutcome => v.choice; case _ => null }, v.parents)
  def unroll2(c:MixtureChoice) = c.children.map(v => Factor(v, c, v.parents))
  def unroll3(p:Parameter) = p.children.map(v => Factor(v, v match { case v:MixtureOutcome => v.choice; case _ => null }, v.parents))
  def score(s:Stat) = 0.0 // s.s1.logpr comes from GeneratedVariableTemplate; gateRefs similarly
  //def score(s:Stat) = { val mc = s.s1; mc.gateRefs.reduceLeft((sum,ref) => sum + mc.value.logpr(ref.outcome)) }
}
*/
