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

class GammaTemplate extends GenerativeTemplateWithStatistics3[Gamma,RealVarParameter,RealVarParameter] {
  def unroll1(g:Gamma) = Factor(g, g.alpha, g.beta)
  def unroll2(a:RealVarParameter) = for (g <- a.childrenOfClass[Gamma]; if (g.alpha == a)) yield Factor(g, a, g.beta)
  def unroll3(b:RealVarParameter) = for (g <- b.childrenOfClass[Gamma]; if (g.beta == b)) yield Factor(g, g.alpha, b)
  def pr(s:Stat): Double = pr(s._1, s._2, s._3)
  def pr(x:Double, alpha:Double, beta:Double): Double = {
    require(x > 0)
    math.pow(beta, alpha) / maths.gamma(alpha) * math.pow(x, alpha - 1) * math.exp(- beta * x)
  }
  def logpr(s:Stat) = math.log(pr(s))
  def sampledValue(s:Stat): Double = sampledValue(s._2, s._3)
  def sampledValue(alpha:Double, beta:Double): Double = maths.nextGamma(alpha.doubleValue, beta.doubleValue)(cc.factorie.random)
}
object GammaTemplate extends GammaTemplate


/** The Gamma distribution generating real values with parameters alpha and beta. 
    @author Andrew McCallum. */
class Gamma(val alpha:RealVarParameter, val beta:RealVarParameter, value:Double = 0) extends RealVariable(value) with MutableGeneratedVar {
  val generativeTemplate = GammaTemplate
  def generativeFactor = new GammaTemplate.Factor(this, alpha, beta)
  alpha.addChild(this)(null)
  beta.addChild(this)(null)
  override def parents = List(alpha, beta)
}

// TODO Finish this.
//class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma, value:Double = 0) extends Gamma(alphaGamma, betaGamma, value)
