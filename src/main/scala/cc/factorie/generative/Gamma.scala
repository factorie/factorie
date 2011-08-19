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

// TODO Consider creating PostiveReal, and then Gamma extends 

//trait GeneratedRealVar extends IntegerVar with GeneratedVar
//abstract class Real(initial: Double = 0.0) extends RealVariable(initial) with GeneratedRealVar with MutableGeneratedVar

object Gamma extends GenerativeFamily3[GeneratedRealVar,RealVarParameter,RealVarParameter] {
  self =>
  def logpr(value:Double, mean:Double, variance:Double): Double = {
    val diff = value - mean
    return - diff * diff / (2 * variance) - 0.5 * math.log(2.0 * math.Pi * variance)
  }
  def pr(x:Double, alpha:Double, beta:Double): Double = {
    require(x > 0)
    math.pow(beta, alpha) / maths.gamma(alpha) * math.pow(x, alpha - 1) * math.exp(- beta * x)
  }
  def sampledValue(alpha:Double, beta:Double): Double = maths.nextGamma(alpha.doubleValue, beta.doubleValue)(cc.factorie.random)
  case class Factor(_1:GeneratedRealVar, _2:RealVarParameter, _3:RealVarParameter) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2, s._3)
    def sampledValue(s:Statistics): Double = self.sampledValue(s._2, s._3)
  }
  def newFactor(_1:GeneratedRealVar, _2:RealVarParameter, _3:RealVarParameter) = Factor(_1, _2, _3)
}



// TODO Finish this.
//class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma, value:Double = 0) extends Gamma(alphaGamma, betaGamma, value)
