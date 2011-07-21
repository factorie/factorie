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
import cc.factorie.la._
import scala.collection.mutable.{HashSet,ArrayBuffer}

trait GeneratedDiscreteVar extends DiscreteVar with GeneratedVar 
trait MutableGeneratedDiscreteVar extends GeneratedDiscreteVar with MutableGeneratedVar with MutableDiscreteVar
abstract class Discrete(initialInt: Int = 0) extends DiscreteVariable(initialInt) with MutableGeneratedDiscreteVar

trait GeneratedCategoricalVar[A] extends GeneratedDiscreteVar with CategoricalVar[A]
trait MutableGeneratedCategoricalVar[A] extends MutableGeneratedDiscreteVar with GeneratedCategoricalVar[A] with MutableCategoricalVar[A]
abstract class Categorical[A](initialValue:A) extends CategoricalVariable(initialValue) with MutableGeneratedCategoricalVar[A] 


trait DiscreteGeneratingFamily extends GenerativeFamily {
  type FamilyType <: DiscreteGeneratingFamily
  def prValue(s:StatisticsType, value:Int): Double
  def prValue(s:cc.factorie.Statistics, value:Int): Double = prValue(s.asInstanceOf[StatisticsType], value)
  def prValue(f:FactorType, value:Int): Double
  //def discreteChild(f:FactorType): GeneratedDiscreteVar
  trait Factor extends super.Factor {
    def discreteGeneratingFamily = DiscreteGeneratingFamily.this
    //override def _1: GeneratedDiscreteVar
    def child = _1
    def prValue(s:StatisticsType, value:Int): Double = DiscreteGeneratingFamily.this.prValue(s, value)
    def prValue(value:Int): Double = DiscreteGeneratingFamily.this.prValue(this.asInstanceOf[FactorType], value)
  }
  //type StatisticsType <: Statistics
  trait Statistics extends super.Statistics {
    def prValue(value:Int): Double = DiscreteGeneratingFamily.this.prValue(this.asInstanceOf[StatisticsType], value)
  }
}

object Discrete extends DiscreteGeneratingFamily with GenerativeFamilyWithStatistics2[GeneratedDiscreteVar,Proportions] /*with DiscreteGeneratingFamily*/ {
  //override type StatisticsType = Stat
  //type FamilyType = Discrete
  def pr(s:StatisticsType) = s._2.apply(s._1.intValue)
  def prValue(s:StatisticsType, intValue:Int): Double = s._2.apply(intValue)
  def prValue(f:Factor, intValue:Int): Double = f._2.apply(intValue)
  def sampledValue(s:StatisticsType): DiscreteValue = s._1.domain.getValue(s._2.sampleInt)
  //def discreteChild(f:Factor) = f._1 
  //def maxIntValue(s:StatisticsType): Int = s._2.maxInt
  override def updateCollapsedParents(f:Factor, weight:Double): Boolean = {
    f._2 match {
      case p:DenseCountsProportions => { p.increment(f._1.intValue, weight)(null); true }
      case _ => false
    }
  }
  // TODO Arrange to call this in Factor construction.
  def factorHook(factor:Factor): Unit =
    if (factor._1.domain.size != factor._2.size) throw new Error("Discrete child domain size different from parent Proportions size.")
}

/*class Binomial(p:RealVarParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/



// The binary special case, for convenience
// TODO Rename this Boolean, inherit from BooleanVariable, and move it to a new file

/** The outcome of a coin flip, with boolean value.  */
class Flip(value:Boolean = false) extends BooleanVariable(value) with MutableGeneratedDiscreteVar 

/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double) extends DenseProportions(Seq(1.0-p, p)) {
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  def flip: Flip = { new Flip :~ Discrete(this) }
  def flip(n:Int) : Seq[Flip] = for (i <- 0 until n) yield flip
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
