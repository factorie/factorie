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

/** A collection of discrete counts generated according a multinomial distribution. */
// TODO Should this also inherit from DiscreteVars?
// Or perhaps instead of Masses should be a separate Counts variable that inherits form DiscreteVars, and then this should inherit from Counts?
trait MultinomialVar extends Masses with GeneratedVariable {
  def proportions: Proportions
  proportions.addChild(this)(null)
  def parents: Seq[Parameter] = List(proportions)
  def logprFrom(p:Proportions): Double = activeDomain.foldLeft(0.0)((sum:Double, i:Int) => sum + apply(i) * proportions.logpr(i))
  def pr: Double = math.exp(logpr)
  def prFrom(parents:Seq[Parameter]): Double = logprFrom(parents)
  def sampleFrom(proportions:Proportions)(implicit d:DiffList) = throw new Error("Not yet implemented")
  def sampleFromParents(implicit d:DiffList = null): this.type = { sampleFrom(proportions); this }
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): this.type = {
    parents match { case Seq(p:Proportions) => sampleFrom(p) }
    this
  }
}

/*class SparseMultinomial(theProportions:Proportions) extends SparseCounts(theProportions.size) with Multinomial {
  def this(p:Proportions, occurrences:Seq[Int]) = { this(p); occurrences.foreach(increment(_, 1.0)) }
  def proportions = theProportions
}
*/
