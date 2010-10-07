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

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(val mean:RealVarParameter, value:Int = 0)(implicit d:DiffList = null) extends IntegerVariable(value) with GeneratedVariable {
  mean.addChild(this)(d)
  def parents = List(mean)
  def pr: Double = prFrom(mean.doubleValue)
  def prFrom(mean:Double): Double = {
    val k = this.intValue
    math.pow(mean, k) * math.exp(-mean) / maths.factorial(k)
  }
  def prFrom(parents:Seq[Parameter]): Double = parents match {
    case Seq(mean:RealVar) => prFrom(mean.doubleValue)
  }
  def sampleFrom(mean:Double)(implicit d:DiffList): Unit =
    set(maths.nextPoisson(mean.doubleValue)(cc.factorie.random).toInt)
  def sampleFromParents(implicit d:DiffList = null): this.type = { sampleFrom(mean.doubleValue); this }
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): this.type = {
    parents match {
      case Seq(mean:RealVar) => sampleFrom(mean.doubleValue)
    }
    this
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

