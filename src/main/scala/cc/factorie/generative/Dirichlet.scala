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

// Proportions ~ Dirichlet(Masses)
// Proportions ~ Dirichlet(Proportions, Precision)

object Dirichlet extends GenerativeFamily2[ProportionsVar,MassesVar] {
  self =>
  def pr(value:Proportions, alpha:Masses): Double = {
    require(value.length == alpha.length)
    var result = maths.logGamma(alpha.sum)
    forIndex(value.length)((i:Int) => result -= maths.logGamma(alpha(i)))
    forIndex(value.length)((i:Int) => result += (alpha(i) - 1.0) * math.log(value(i)))
    assert(result == result, "alpha="+alpha.toSeq+" p="+value.toSeq) // NaN?
    result
  }
  def sampledValue(masses:Masses, children:Iterable[DiscreteVar] = Nil): Proportions = new DenseProportions1(sampledArray(masses, children))
  // TODO Make a more general argument type than Iterable[DiscreteVar], like Iterable[Int] (but I'm concerned about boxing)
  def sampledArray(alpha:Masses, children:Iterable[DiscreteVar] = Nil): Array[Double] = {
    var norm = 0.0
    val p = new Array[Double](alpha.length)
    val c = new Array[Double](alpha.length)
    for (child <- children) c(child.intValue) += 1.0
    forIndex(alpha.length)(i => {
      p(i) = maths.nextGamma(alpha(i) + c(i), 1)(cc.factorie.random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    })
    forIndex(alpha.length)(i => p(i) /= norm)
    p
  }
  case class Factor(_1:ProportionsVar, _2:MassesVar) extends super.Factor {
    def pr(s:StatisticsType) = self.pr(s._1, s._2)
    override def pr: Double = self.pr(_1.value, _2.value)
    def sampledValue(s:StatisticsType): Proportions = self.sampledValue(s._2, Nil)
    override def sampledValue: Proportions = self.sampledValue(_2.tensor, Nil)
    override def updateCollapsedChild(): Boolean = { _1.tensor.+=(_2.value); true }
  }
  def newFactor(a:ProportionsVar, b:MassesVar) = Factor(a, b)
}

object DirichletMomentMatching {
  def estimate(masses:MassesVariable, model:GenerativeModel): Unit = {
    val numChildren = model.childFactors(masses).size
    //val massesChildren = masses.children //mean.generatedChildren // TODO Without "generatedChildren" this no longer works for Dirichlet mixtures.
    assert(numChildren > 1)
    //val factors = model.factors(List(mean, precision)); assert(factors.size == mean.children.size); assert(factors.size == precision.children.size)
    // Calculcate and set the mean
    val m = new DenseProportions1(new Array[Double](masses.tensor.length))
    for (factor <- model.childFactors(masses)) factor match { 
      case f:Dirichlet.Factor => {
        require(masses.tensor.length == f._1.tensor.length) // Make sure that each child Proportions has same length as parent masses
        forIndex(m.size)(i => m(i) += f._1.tensor(i))
      }
    }
    forIndex(m.size)(m(_) /= numChildren)
    //mean.set(m)(null)
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new Array[Double](masses.tensor.length)
    for (factor <- model.childFactors(masses)) factor match { 
      case f:Dirichlet.Factor => forIndex(masses.tensor.length)(i => { val diff = f._1.tensor(i) - m(i); variance(i) += diff * diff })
    }
    //for (child <- meanChildren) child match { case p:Proportions => forIndex(mean.length)(i => variance(i) += p(i) * p(i)) }
    //println("variance1="+variance.toList)
    forIndex(masses.tensor.length)(i => variance(i) /= (numChildren - 1.0))
    //forIndex(mean.length)(i => variance(i) = (variance(i) / meanChildren.size - 1.0) - (m(i) * m(i)))
    //println("variance2="+variance.toList)
    var alphaSum = 0.0
    forIndex(masses.tensor.length)(i => if (m(i) != 0.0) alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    val precision = math.exp(alphaSum / (masses.tensor.length - 1))
    assert(precision == precision, "alphaSum="+alphaSum+" variance="+variance.toList+" mean="+m.toSeq) // Check for NaN
    forIndex(m.size)(i => m(i) = m(i) * precision)
    masses.set(m)(null)
  }
}

/** Alternative style of Dirichlet parameterized by 2 parents (mean,precision) rather than 1 (masses). */
object Dirichlet2 extends GenerativeFamily3[ProportionsVar,ProportionsVar,RealVar] {
  def newFactor(a:ProportionsVar, b:ProportionsVar, c:RealVar) = throw new Error("Not yet implemented")
}
  

