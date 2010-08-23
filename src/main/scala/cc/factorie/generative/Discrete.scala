/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable.{HashSet,ArrayBuffer}

/*class Binomial(p:RealVarParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/
trait GeneratedDiscreteVar extends GeneratedVar with DiscreteVar {
  def proportions: Proportions
  def parents: Seq[Parameter] = List(proportions)
  def pr: Double = proportions(this.intValue)
  def prFrom(parents:Seq[Parameter]) = parents match { case Seq(p:Proportions) => p(this.intValue) }
  override def prWith(map:scala.collection.Map[Parameter,Parameter]): Double = map.getOrElse(proportions, proportions).asInstanceOf[Proportions](this.intValue)
}
trait GeneratedDiscreteVariable extends DiscreteVariable with GeneratedVariable with GeneratedDiscreteVar {
  def sample(implicit d:DiffList): Unit = set(proportions.sampleInt)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList) = parents match {
    case Seq(p:Proportions) => set(p.sampleInt)
  }
  def maximize(implicit d:DiffList): Unit = set(proportions.maxPrIndex)
}
// A Discrete ~ Multinomial(Proportions), in which we can change the parent
class Discrete(p:Proportions, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable {
  //assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList = null) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  override def parentRefs = List(proportionsRef)
}
trait GeneratedCategoricalVar[A] extends GeneratedDiscreteVar with CategoricalVar[A]
trait GeneratedCategoricalVariable[A] extends CategoricalVariable[A] with GeneratedDiscreteVariable with GeneratedCategoricalVar[A]
class Categorical[A](p:Proportions, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] {
  //assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  override def parentRefs = List(proportionsRef)
}
class ObservedDiscrete(p:Proportions, value:Int) extends DiscreteObservation(value) with GeneratedVar {
  // TODO Rename "DiscreteConstant"?
  //assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  def parents = List(proportionsRef.value)
  override def parentRefs = List(proportionsRef)
  def pr: Double = proportions(this.intValue)
  def prFrom(parents:Seq[Parameter]): Double = parents match {
    case p:Proportions => p(this.intValue)
  }
}
class ObservedDiscretes(val proportions:Proportions, values:Traversable[Int] = Nil) extends DiscreteVars with GeneratedVar with ConstantValue {
  assert(proportions.length <= domainSize)
  proportions.addChild(this)(null)
  def parents = List(proportions)
  private val _values = values.toArray
  def logpr(p:Proportions): Double = { var result = 0.0; forIndex(_values.size)(index => result += math.log(p(index))); result }
  override def logpr: Double = logpr(proportions)
  override def logprFrom(parents:Seq[Parameter]): Double = parents match {
    case p:Proportions => logpr(p)
  }
  def prFrom(parents:Seq[Parameter]): Double = prFrom(parents)
  def pr: Double = math.exp(logpr)
  def vector: Vector = throw new Error
  def indices: Collection[Int] = throw new Error
}

