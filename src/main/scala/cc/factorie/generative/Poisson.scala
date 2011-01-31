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

class PoissonTemplate extends GenerativeTemplateWithStatistics2[Poisson,PoissonMeanVar] {
  def unroll1(p:Poisson) = Factor(p, p.mean)
  def unroll2(m:PoissonMeanVar) = for (p <- m.childrenOfClass[Poisson]) yield Factor(p, m)
  def pr(s:Stat) = pr(s._1, s._2)
  def pr(k:Int, mean:Double) = math.pow(mean, k) * math.exp(-mean) / maths.factorial(k)
  def logpr(s:Stat) = math.log(pr(s))
  def sampledValue(s:Stat): Int = sampledValue(s._2)
  def sampledValue(mean:Double): Int = maths.nextPoisson(mean)(cc.factorie.random).toInt
}
object PoissonTemplate extends PoissonTemplate

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(val mean:PoissonMeanVar, value:Int = 0) extends IntegerVariable(value) with MutableGeneratedVar {
  mean.addChild(this)(null)
  val generativeTemplate = PoissonTemplate
  def generativeFactor = new PoissonTemplate.Factor(this, mean)
  override def parents = List(mean)
  /** This implements the maximum likelihood estimator */
  /*def estimate: Unit = {
    if (generatedSamples.size == 0) throw new Error("No samles from which to estimate")
    val sum = generatedSamples.sumInts(_.intValue).toDouble
    lambda = sum/generatedSamples.size
  }*/
}

trait PoissonMeanVar extends RealVar with Parameter

/*abstract class GammaPoisson(gamma:Gamma) extends Distribution[IntegerVar] {
  throw new Error("Not yet implemented")
}
*/

