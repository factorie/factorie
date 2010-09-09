/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(val mean:RealVarParameter, value:Int = 0)(implicit d:DiffList = null) extends IntegerVariable(value) with GeneratedVariable {
  mean.addChild(this)(d)
  def parents = List(mean)
  def pr: Double = prFrom(mean.doubleValue)
  def prFrom(mean:Double): Double = {
    val k = this.intValue
    math.pow(mean, k) * math.exp(-mean) / Maths.factorial(k)
  }
  def prFrom(parents:Seq[Parameter]): Double = parents match {
    case Seq(mean:RealVar) => prFrom(mean.doubleValue)
  }
  def sampleFrom(mean:Double)(implicit d:DiffList): Unit =
    set(Maths.nextPoisson(mean.doubleValue)(cc.factorie.random).toInt)
  def sample(implicit d:DiffList): Unit = sampleFrom(mean.doubleValue)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(mean:RealVar) => sampleFrom(mean.doubleValue)
  }
  /** This implements the maximum likelihood estimator */
  /*def estimate: Unit = {
    if (generatedSamples.size == 0) throw new Error("No samles from which to estimate")
    val sum = generatedSamples.sumInts(_.intValue).toDouble
    lambda = sum/generatedSamples.size
  }*/
}

/*abstract class GammaPoisson(gamma:Gamma) extends Distribution[IntegerVar] {
  throw new Error("Not yet implemented")
}
*/

