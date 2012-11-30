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
  def prValue(value:Int): Double
  //def prValue(s:StatisticsType, value:Int): Double
}

object Discrete extends GenerativeFamily2[DiscreteVariable,ProportionsVariable] {
  case class Factor(override val _1:DiscreteVariable, override val _2:ProportionsVariable) extends super.Factor(_1, _2) with DiscreteGeneratingFactor {
    //def proportions: Proportions = _2 // Just an alias
    def pr(child:DiscreteValue, proportions:Proportions) = proportions(child.intValue)
    override def pr: Double = _2.value.apply(_1.intValue)
    def prValue(p:Proportions, intValue:Int): Double = p.apply(intValue)
    override def prValue(intValue:Int): Double = _2.value.apply(intValue)
    def sampledValue(p:Proportions): DiscreteValue = _1.domain.apply(p.sampleIndex)
    override def sampledValue: DiscreteValue = _1.domain.apply(_2.value.sampleIndex)
    def maxIntValue(p:Proportions): Int = p.maxIndex // TODO 
    override def updateCollapsedParents(weight:Double): Boolean = { _2.tensor.masses.+=(_1.intValue, weight); true }
  }
  def newFactor(a:DiscreteVariable, b:ProportionsVariable) = {
    if (a.domain.size != b.tensor.length) throw new Error("Discrete child domain size different from parent Proportions size.")
    Factor(a, b) 
  }
}

object MaximizeGeneratedDiscrete extends Maximize {
  def apply(d:DiscreteVariable, model:Model): Unit = {
    val dFactors = model.factors(d)
    require(dFactors.size == 1)
    dFactors.head match {
      case factor:Discrete.Factor => d.set(factor._2.tensor.maxIndex)(null)
      case _ => throw new Error("This Maximizer only handles factors of type Discrete.Factor.")
    }
  }
  def apply(varying:Iterable[DiscreteVariable], model:Model): Unit = for (d <- varying) apply(d, model)
  def infer[V<:DiscreteVariable](varying:V, model:Model): Option[DiscreteMarginal1[V]] = {
    val dFactors = model.factors(varying)
    require(dFactors.size == 1)
    dFactors.head match {
      case factor:Discrete.Factor => Some(new DiscreteMarginal1(varying, new SingletonProportions1(varying.domain.size, factor._2.tensor.maxIndex)))
      case _ => None
    }
  }
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[DiscreteSummary1[DiscreteVariable]] = {
    if (summary ne null) return None
    if (!variables.forall(_.isInstanceOf[DiscreteVariable])) return None
    val result = new DiscreteSummary1[DiscreteVariable]
    for (v <- variables) {
      infer(v.asInstanceOf[DiscreteVariable], model) match {
        case Some(dm) => result += dm
        case _ => return None
      }
    }
    Some(result)
  }

}



/*class Binomial(p:RealVarParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/


// The binary special case, for convenience
// TODO Rename this Boolean, inherit from BooleanVariable, and move it to a new file

/** The outcome of a coin flip, with boolean value.  */
class Flip(value:Boolean = false) extends BooleanVariable(value)  

/** A coin, with Multinomial distribution over outcomes, which are Flips. */
class Coin(p:Double) extends ProportionsVariable(new DenseProportions1(2)) {
  def this() = this(0.5)
  tensor(0) = 1.0 - p
  tensor(1) = p
  assert (p >= 0.0 && p <= 1.0)
  //def flip: Flip = { new Flip :~ Discrete(this) }
  //def flip(n:Int) : Seq[Flip] = for (i <- 0 until n) yield flip
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
