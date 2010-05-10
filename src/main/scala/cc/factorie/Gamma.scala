/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

// TODO Consider renaming Real -> PositiveRealValue ??
// TODO Consider Real.value -> Real.toDouble and/or Real.doubleValue GenerativeDistribution[PositiveReal]

// TODO Consider creating PostiveReal, and then Gamma extends 

/** The Gamma distribution generating real values with parameters alpha and beta. 
    @author Andrew McCallum. */
class Gamma(alpha:Real, beta:Real) extends GenerativeDistribution[Real] {
  def this(alpha:Double, beta:Double) = this(new Real(alpha), new Real(beta))
  // Note that there is an implicit conversion from RealValue to Double, which we leverage below
  def pr(x:Double) = {
    assert (x > 0)
    Math.pow(beta.doubleValue, alpha.doubleValue) / Maths.gamma(alpha.doubleValue) * Math.pow(x, alpha.doubleValue - 1) * Math.exp(- beta.doubleValue * x)
  }
  def pr(o:Real): Double = pr(o.doubleValue)
  // TODO def logpr(x:Double) = 
  def sample: Double = Maths.nextGamma(alpha.doubleValue, beta.doubleValue)(Global.random)
  def estimate: Unit = {
    throw new Error("Not yet implemented")
  }
}

// TODO Finish this.
abstract class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma) extends GenerativeDistribution[Real]
