/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(val mean:RealValueParameter, value:Int = 0)(implicit d:DiffList = null) extends OrdinalVariable(value) with GeneratedVariable {
  mean.addChild(this)(d)
  def parents = List(mean)
  def pr: Double = pr(this.intValue)
  def pr(k:Int): Double = Math.pow(mean.doubleValue, k) * Math.exp(-mean.doubleValue) / Maths.factorial(k)
  def sampleFrom(mean:Double)(implicit d:DiffList): Unit =
    setByInt(Maths.nextPoisson(mean.doubleValue)(Global.random).toInt)
  def sample(implicit d:DiffList): Unit = sampleFrom(mean.doubleValue)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(mean:RealValue) => sampleFrom(mean.doubleValue)
  }
  /** This implements the maximum likelihood estimator */
  /*def estimate: Unit = {
    if (generatedSamples.size == 0) throw new Error("No samles from which to estimate")
    val sum = generatedSamples.sumInts(_.intValue).toDouble
    lambda = sum/generatedSamples.size
  }*/
}

/*abstract class GammaPoisson(gamma:Gamma) extends Distribution[IntegerValue] {
  throw new Error("Not yet implemented")
}
*/

