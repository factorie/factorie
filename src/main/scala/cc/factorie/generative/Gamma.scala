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

/** The Gamma distribution generating real values with parameters alpha and beta. 
    @author Andrew McCallum. */
class Gamma(val alpha:RealVarParameter, val beta:RealVarParameter, value:Double = 0) extends RealVariable(value) with GeneratedVariable {
  alpha.addChild(this)(null)
  beta.addChild(this)(null)
  def parents = List(alpha, beta)
  def pr(alpha:Double, beta:Double) = {
    val x = doubleValue
    assert (x > 0)
    math.pow(beta, alpha) / maths.gamma(alpha) * math.pow(x, alpha - 1) * math.exp(- beta * x)
  }
  def pr = pr(alpha.doubleValue, beta.doubleValue)
  def prFrom(parents:Seq[Parameter]) = parents match {
    case Seq(alpha:RealVar, beta:RealVar) => pr(alpha.doubleValue, beta.doubleValue)
  }
  // TODO def logpr(x:Double) = 
  def sampleFrom(alpha:RealVar, beta:RealVar)(implicit d:DiffList): Unit = 
    set(maths.nextGamma(alpha.doubleValue, beta.doubleValue)(cc.factorie.random))
  def sampleFromParents(implicit d:DiffList = null): this.type = { sampleFrom(alpha, beta); this }
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): this.type = {
    parents match {
      case Seq(alpha:RealVar, beta:RealVar) => sampleFrom(alpha, beta)
    }
    this
  }
}

// TODO Finish this.
//class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma, value:Double = 0) extends Gamma(alphaGamma, betaGamma, value)
