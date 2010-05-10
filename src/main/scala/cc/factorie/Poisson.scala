/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(var lambda:Double) extends GenerativeDistribution[IntegerValue] {
  def mean: Double = lambda
  def variable: Double = lambda
  def pr(k:Int) = Math.pow(lambda, k) * Math.exp(-lambda) / Maths.factorial(k)
  def pr(o:IntegerValue): Double = pr(o.intValue)
  def sample: Int = Maths.nextPoisson(lambda)(Global.random).toInt
  /** This implements the maximum likelihood estimator */
  def estimate: Unit = {
    if (generatedSamples.size == 0) throw new Error("No samles from which to estimate")
    val sum = generatedSamples.sumInts(_.intValue).toDouble
    lambda = sum/generatedSamples.size
  }
}

abstract class GammaPoisson(gamma:Gamma) extends GenerativeDistribution[IntegerValue] {
  throw new Error("Not yet implemented")
}

