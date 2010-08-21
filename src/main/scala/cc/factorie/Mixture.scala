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

/*
trait MixtureChoiceVariable extends GeneratedDiscreteVariable with Gate
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVariable
abstract class MixtureChoiceMixture(ps:Seq[Proportions], choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteMixture(ps, choice, value) with MixtureChoiceVariable

trait MixtureOutcome extends GeneratedVar {
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
*/

/*class DenseDirichletMixture(val components:Seq[Proportions], prec:RealValueParameter, val choice:MixtureChoiceVariable, p:Seq[Double] = Nil)
extends DenseDirichlet(components(choice.intValue), prec, p) with MixtureOutcome {
  override protected val meanRef: ParameterRef[Proportions,Dirichlet with MixtureOutcome] = new GatedParameterRef(components, choice, this)
  override def mean_=(p2:Proportions)(implicit d:DiffList = null) = throw new Error
  def prFromMixtureComponent(index:Int): Double = math.exp(logpr(components(index), precision))
}*/



/*class MixtureChoiceVariableTemplate extends TemplateWithStatistics1[MixtureChoiceVariable] {
  def score(s:Stat) = 0 // s.s1.logpr comes from GeneratedVariableTemplate; gateRefs similarly
  //def score(s:Stat) = { val mc = s.s1; mc.gateRefs.reduceLeft((sum,ref) => sum + mc.value.logpr(ref.outcome)) }
}
*/



trait MixtureChoiceVariable extends GeneratedDiscreteVariable {
  // 'outcomes' are a more efficient alternative to 'children' for small sets of outcomes.
  private var _outcomes: List[MixtureOutcome] = Nil
  def outcomes = _outcomes
  def addOutcome(o:MixtureOutcome): Unit = {
    assert(o.mixtureSize == domainSize)
    _outcomes = o :: _outcomes
  }
}
abstract class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with MixtureChoiceVariable
abstract class MixtureChoiceMixture(ps:FiniteMixture[Proportions], choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteMixture(ps, choice, value) with MixtureChoiceVariable

// TODO Consider renaming this "MixtureVar".  Yes!
// Note that this restricts us to having just one MixtureChoice per MixtureOutcome; but this handles the common cases I can think of.
trait MixtureOutcome extends GeneratedVar {
  //def components: Seq[MixtureComponents[_]]
  def choice: MixtureChoiceVariable
  // Just those parents whose selection is controled by 'choice'
  def chosenParents: Seq[Parameter]
  def mixtureSize: Int
  def prFromMixtureComponent(index:Int): Double
}

/** Revert equals/hashCode behavior of Seq[A] to the default Object.
    WARNING: This doesn't actually satisfy commutativity with a Seq[A]. :-( */
trait SeqEqualsEq[+A] extends scala.collection.Seq[A] {
  override def equals(that:Any): Boolean = that match {
    case that:Seq[A] => this eq that
    case _ => false
  }
  override def hashCode: Int = java.lang.System.identityHashCode(this)
}
trait MixtureComponents[+P<:Parameter] extends scala.collection.immutable.IndexedSeq[P] with SeqEqualsEq[P] with Parameter {
  def childrenOf[P2>:P](p:P2): Iterable[GeneratedVar]
  /*def iterator: Iterator[P] = new Iterator[P] {
    var i = -1
    def hasNext = i < length
    def next = { i += 1; apply(i) }
  }*/
}
class FiniteMixture[+P<:Parameter](components:Seq[P]) extends MixtureComponents[P] with GeneratedVar {
  private val _components = components.toIndexedSeq
  _components.foreach(_.addChild(this)(null))
  def parents = _components
  def pr = 1.0 // TODO something else?
  def length = _components.length
  def apply(index:Int) = _components(index)
  def childrenOf[P2>:P](p:P2): Iterable[GeneratedVar] = {
    val index = _components.indexOf(p)
    children.filter(_.isInstanceOf[MixtureOutcome]).asInstanceOf[Iterable[MixtureOutcome]].filter(_.choice.intValue == index)
  }
  override def isDeterministic = true
}
object FiniteMixture {
  def apply[P<:Parameter](n:Int)(constructor: =>P): FiniteMixture[P] = new FiniteMixture[P](for (i <- 1 to n) yield constructor)
}
//class InfiniteMixture[P<:Parameter](val constructor: =>P) extends MixtureComponents[P]

trait DiscreteMixtureVar extends GeneratedDiscreteVariable with MixtureOutcome {
  choice.addOutcome(this)
  components.addChild(this)(null)
  def choice: MixtureChoiceVariable
  def components: FiniteMixture[Proportions]
  def proportions = components(choice.intValue)
  def prFromMixtureComponent(index:Int): Double = components(index).pr(intValue)
  def chosenParents = List(components(choice.intValue))
  override def parents = super.parents match { case list:List[Parameter] => components :: list; case seq:Seq[Parameter] => components +: seq }
  def mixtureSize = components.length
}

class DiscreteMixture(val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable with MixtureOutcome with DiscreteMixtureVar
class CategoricalMixture[A](val components:FiniteMixture[Proportions], val choice:MixtureChoiceVariable, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] with DiscreteMixtureVar

// Outcome, MixtureChoice, Parents
// Common pattern:  mean -> MixtureComponents -> Gaussian

