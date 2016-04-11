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
import cc.factorie.variable.DoubleVar

// TODO Consider creating PostiveDouble, and then Gamma extends 

object Gamma extends DirectedFamily3[DoubleVar,DoubleVar,DoubleVar] {
  self =>
  def logpr(value:Double, mean:Double, variance:Double): Double = {
    val diff = value - mean
    - diff * diff / (2 * variance) - 0.5 * math.log(2.0 * math.Pi * variance)
  }
  def pr(x:Double, alpha:Double, beta:Double): Double = {
    require(x > 0)
    math.pow(beta, alpha) / maths.gamma(alpha) * math.pow(x, alpha - 1) * math.exp(- beta * x)
  }
  def sampledValue(alpha:Double, beta:Double)(implicit random: scala.util.Random): Double = maths.nextGamma(alpha, beta)(random)
  case class Factor(override val _1:DoubleVar, override val _2:DoubleVar, override val _3:DoubleVar) extends super.Factor(_1, _2, _3) {
    def pr(child:Double, mean:Double, variance:Double): Double = self.pr(child, mean, variance)
    def sampledValue(mean:Double, variance:Double)(implicit random: scala.util.Random): Double = self.sampledValue(mean, variance)
  }
  def newFactor(_1:DoubleVar, _2:DoubleVar, _3:DoubleVar) = Factor(_1, _2, _3)
}



// TODO Finish this.
//class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma, value:Double = 0) extends Gamma(alphaGamma, betaGamma, value)
