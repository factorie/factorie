/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._

// TODO Consider creating PostiveReal, and then Gamma extends 

/** The Gamma distribution generating real values with parameters alpha and beta. 
    @author Andrew McCallum. */
class Gamma(val alpha:RealVarParameter, val beta:RealVarParameter, value:Double = 0) extends RealVariable(value) with GeneratedVariable {
  alpha.addChild(this)(null)
  beta.addChild(this)(null)
  def parents = List(alpha, beta)
  def pr = {
    val x = doubleValue
    assert (x > 0)
    Math.pow(beta.doubleValue, alpha.doubleValue) / Maths.gamma(alpha.doubleValue) * Math.pow(x, alpha.doubleValue - 1) * Math.exp(- beta.doubleValue * x)
  }
  // TODO def logpr(x:Double) = 
  def sampleFrom(alpha:RealVar, beta:RealVar)(implicit d:DiffList): Unit = 
    set(Maths.nextGamma(alpha.doubleValue, beta.doubleValue)(cc.factorie.random))
  def sample(implicit d:DiffList): Unit = sampleFrom(alpha, beta)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(alpha:RealVar, beta:RealVar) => sampleFrom(alpha, beta)
  }
}

// TODO Finish this.
//class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma, value:Double = 0) extends Gamma(alphaGamma, betaGamma, value)
