/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.directed

import cc.factorie._
import cc.factorie.variable._

// Proportions ~ Dirichlet(Masses)
// Proportions ~ Dirichlet(Proportions, Precision)

object Dirichlet extends DirectedFamily2[ProportionsVariable,MassesVariable] {
  self =>
  def pr(value:Proportions, alpha:Masses): Double = {
    require(value.length == alpha.length)
    var result = maths.logGamma(alpha.sum)
    (0 until value.length).foreach((i:Int) => result -= maths.logGamma(alpha(i)))
    (0 until value.length).foreach((i:Int) => result += (alpha(i) - 1.0) * math.log(value(i)))
    assert(result == result, "alpha="+alpha.toSeq+" p="+value.toSeq) // NaN?
    result
  }
  def sampledValue(masses:Masses, children:Iterable[DiscreteVar] = Nil)(implicit random: scala.util.Random): Proportions = new DenseProportions1(sampledArray(masses, children))
  // TODO Make a more general argument type than Iterable[DiscreteVar], like Iterable[Int] (but I'm concerned about boxing)
  def sampledArray(alpha:Masses, children:Iterable[DiscreteVar] = Nil)(implicit random: scala.util.Random): Array[Double] = {
    var norm = 0.0
    val p = new Array[Double](alpha.length)
    val c = new Array[Double](alpha.length)
    for (child <- children) c(child.intValue) += 1.0
    for (i <- 0 until alpha.length) {
      p(i) = maths.nextGamma(alpha(i) + c(i), 1)(random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    }
    (0 until alpha.length).foreach(i => p(i) /= norm)
    p
  }
  case class Factor(override val _1:ProportionsVariable, override val _2:MassesVariable) extends super.Factor(_1, _2) {
    def pr(p:Proportions, m:Masses) = self.pr(p, m)
    override def pr: Double = self.pr(_1.value, _2.value)
    def sampledValue(masses:Masses)(implicit random: scala.util.Random): Proportions = self.sampledValue(masses, Nil)
    override def sampledValue(implicit random: scala.util.Random): Proportions = self.sampledValue(_2.value, Nil)
    override def updateCollapsedChild(): Boolean = { _1.value.+=(_2.value); true }
  }
  def newFactor(a:ProportionsVariable, b:MassesVariable) = Factor(a, b)
}

object MaximizeDirichletByMomentMatching {
  def apply(masses:MassesVariable, model:DirectedModel): Unit = {
    // Calculate and set the mean
    val m = new DenseMasses1(masses.value.length)
    val childFactors = model.childFactors(masses)
    val numChildren = childFactors.size; assert(numChildren > 1)
    for (factor <- childFactors) factor match { 
      case f:Dirichlet.Factor => {
        m += f._1.value
        //assert(!f._1.tensor.contains(Double.PositiveInfinity)) // TODO Remove this line
        //println("tensor.class="+f._1.tensor.getClass.getName+" tensor.sum="+f._1.tensor.sum+" tensor.max="+f._1.tensor.max+" sum="+m.sum+" max="+m.max)
        //forIndex(m.size)(i => m(i) += f._1.tensor(i))
      }
    }
    m.normalize()
    //assert(m.forall(_ >= 0.0))
    //println("MaximizeDirichletByMomentMatching mean max="+m.max)
    //forIndex(m.size)(m(_) /= numChildren)
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new cc.factorie.la.DenseTensor1(masses.value.length)
    for (factor <- childFactors) factor match { 
      case f:Dirichlet.Factor => {
        val len = m.length
        var i = 0
        while (i < len) {
          val diff = f._1.value(i) - m(i); variance(i) += diff * diff
          i += 1
        }
      }
      //forIndex(masses.tensor.length)(i => { val diff = f._1.tensor(i) - m(i); variance(i) += diff * diff })
    }
    //assert(m.forall(_ >= 0.0))
    //assert(!m.containsNaN)
    //println("MaximizeDirichletByMomentMatching variance max="+variance.max)
    //println("MaximizeDirichletByMomentMatching numChildren="+numChildren)
    //for (child <- meanChildren) child match { case p:Proportions => forIndex(mean.length)(i => variance(i) += p(i) * p(i)) }
    //println("variance1="+variance.toList)
    variance /= (numChildren - 1.0)
    //println("MaximizeDirichletByMomentMatching variance max="+variance.max)
    //assert(!variance.containsNaN)
    //forIndex(masses.tensor.length)(i => variance(i) /= (numChildren - 1.0))
    //println("variance2="+variance.toList)
    var alphaSum = 0.0
    //assert(m.forall(_ >= 0.0))
    //assert(!m.containsNaN)
    m.foreachElement((i,v) => { if (v != 0.0) alphaSum += math.log((v * (1.0 - v) / variance(i)) - 1.0); assert(alphaSum == alphaSum, "culprit: v="+v+" variance="+variance(i)+" incr="+math.log((v * (1.0 - v) / variance(i)) - 1.0)) })
    //assert(m.forall(_ >= 0.0))
    //assert(!m.containsNaN)
    //forIndex(masses.tensor.length)(i => if (m(i) != 0.0) alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    // ... alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    val precision = math.exp(alphaSum / (masses.value.length - 1))
    //println("MaximizeDirichletByMomentMatching precision="+precision)
    assert(precision > 0)
    assert(precision == precision, "alphaSum="+alphaSum+" variance="+variance.asSeq+" mean="+m.asSeq) // Check for NaN
    //assert(m.forall(_ >= 0.0))
    //assert(!m.containsNaN)
    //println("MaximizeDirichletByMomentMatching m="+m)
    m *= precision
    //println("MaximizeDirichletByMomentMatching m="+m)
    //assert(!m.containsNaN)
    //forIndex(m.size)(i => m(i) = m(i) * precision)
    m.foreachElement((i,v) => if (v < 0.0) println("i,v="+i+", "+v))
    //assert(m.forall(_ >= 0.0))
    masses := m
  }
}

object LearnDirichletUsingFrequencyHistograms {

  def apply(masses: MassesVariable, observations: Array[Array[Int]], observationLengths: Array[Int]){
     apply(masses, observations, observationLengths, 1.001, 1.0, 1)
  }

  def apply(masses: MassesVariable, observations: Array[Array[Int]], observationLengths: Array[Int],
            shape: Double, scale: Double, numIters: Int) {

    val parameters = masses.value.toArray

    var denominator = 0.0
    var currentDigamma = 0.0
    var paramSum = 0.0
    parameters.foreach(param => paramSum += param)

    // The histograms contain mostly zeros for large indices. To avoid looping over those indices,
    // find the maximum index at which there is a non-zero entry.
    val maxNonZeroIndices = Array.fill[Int](observations.length)(-1)
    for (i <- 0 until observations.length; j <- 0 until observations(i).length) if (observations(i)(j) > 0) maxNonZeroIndices(i) = j

    for (iterId <- 0 until numIters){
      denominator = 0.0
      currentDigamma = 0.0

      for (i <- 1 until observationLengths.length){
        currentDigamma += 1.0 / (paramSum + i - 1)
        denominator += observationLengths(i) * currentDigamma
      }

      denominator -= 1.0/scale
      paramSum = 0.0

      for (k <- 0 until parameters.length){
        val maxNonzeroIndex = maxNonZeroIndices(k)

        val oldParam = parameters(k)
        parameters(k) = 0.0
        currentDigamma = 0.0

        val histogram = observations(k)
        for (j <- 1 to maxNonzeroIndex){
          currentDigamma += 1.0 / (oldParam + j - 1)
          parameters(k) += histogram(j) * currentDigamma
        }

        parameters(k) = oldParam * (parameters(k) + shape) / denominator
        paramSum += parameters(k)
      }
    }

    masses := new DenseMasses1(masses.value.length)
    for(i <- 0 until parameters.length) masses.increment(i, parameters(i))(null)
    assert(paramSum >= 0.0)
  }

}

/** Alternative style of Dirichlet parameterized by 2 parents (mean,precision) rather than 1 (masses). */
object Dirichlet2 extends DirectedFamily3[ProportionsVar,ProportionsVar,RealVar] {
  def newFactor(a:ProportionsVar, b:ProportionsVar, c:RealVar) = throw new Error("Not yet implemented")
}
  

