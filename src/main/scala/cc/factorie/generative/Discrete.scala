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


trait DiscreteGeneratingFactor extends GenerativeFactor {
  //type ChildType <: GeneratedDiscreteVar
  def prValue(value:Int): Double = prValue(statistics, value)
  def prValue(s:StatisticsType, value:Int): Double
}

object Discrete extends GenerativeFamily2[DiscreteVar,Proportions] {
  case class Factor(_1:DiscreteVar, _2:Proportions) extends super.Factor with DiscreteGeneratingFactor {
    def pr(s:Statistics) = s._2.apply(s._1.intValue)
    override def pr: Double = _2.apply(_1.intValue)
    def prValue(s:Statistics, intValue:Int): Double = s._2.apply(intValue)
    override def prValue(intValue:Int): Double = _2.apply(intValue)
    def sampledValue(s:Statistics): DiscreteValue = s._1.domain.getValue(s._2.sampleInt)
    override def sampledValue: DiscreteValue = _1.domain.getValue(_2.sampleInt)
    def maxIntValue(s:Statistics): Int = s._2.maxInt
    override def updateCollapsedParents(weight:Double): Boolean = {
      _2 match {
        case p:DenseCountsProportions => { p.increment(_1.intValue, weight)(null); true }
        case _ => false
      }
    }
  }
  def newFactor(a:DiscreteVar, b:Proportions) = Factor(a, b)
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
class Flip(value:Boolean = false) extends BooleanVariable(value)  

/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double) extends DenseProportions(Seq(1.0-p, p)) {
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  //def flip: Flip = { new Flip :~ Discrete(this) }
  //def flip(n:Int) : Seq[Flip] = for (i <- 0 until n) yield flip
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
