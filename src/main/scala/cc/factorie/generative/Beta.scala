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

/** Beta distribution.
    http://en.wikipedia.org/wiki/Beta_distribution */
object Beta extends GenerativeFamily3[RealVar,RealVar,RealVar] {
  self =>
  def logpr(value:Double, alpha:Double, beta:Double): Double = math.log(pr(value, alpha, beta))
  def pr(value:Double, alpha:Double, beta:Double): Double = {
    require(value >= 0.0 && value <= 1.0)
    math.pow(value, alpha-1.0) * math.pow(1.0-value, beta-1.0) / maths.beta(alpha, beta) 
  }
  def sampledValue(alpha:Double, beta:Double): Double = {
    val x = maths.nextGamma(alpha, 1.0)(cc.factorie.random) 
    val y = maths.nextGamma(beta, 1.0)(cc.factorie.random)
    x / (x + y)
  }
  case class Factor(_1:RealVar, _2:RealVar, _3:RealVar) extends super.Factor {
    def pr(s:Statistics): Double = self.pr(s._1, s._2, s._3)
    def sampledValue(s:Statistics): Double = self.sampledValue(s._2, s._3)
  }
  def newFactor(_1:RealVar, _2:RealVar, _3:RealVar) = Factor(_1, _2, _3)
}

object MaximizeBetaByMomentMatching {
  def apply(alpha:RealVariable, beta:RealVariable, model:GenerativeModel): Unit = {
    throw new Error("Not yet implemented.") // TODO Finish this.
  }
}
