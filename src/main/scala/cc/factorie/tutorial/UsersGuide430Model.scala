/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
/*& Model */

/*&
 * Model Tutorial
 * ==============
 * 
 * Fundamentally a Model is a container for Factors.
 * Its primary function is, given a collection of variables, to return the Factors that neighbor those variables.
 * 
 * The trait ``Model`` leaves abstract how this mapping from Variables to Factors is maintained.
 **/
package cc.factorie.tutorial
object TutorialModel extends App {
  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.variable.BooleanVariable
  import cc.factorie.model.{ ItemizedModel, DotFactorWithStatistics2 }

  /*& Let's start by creating some Variables and Factor classes. **/
  val outputs: Seq[BooleanVariable] = for (i <- 0 until 10) yield new BooleanVariable
  val inputs: Seq[BooleanVariable] = for (i <- 0 until 10) yield new BooleanVariable(i % 2 == 0)
  val markovWeights = new DenseTensor2(Array(Array(1.0, 0.0), Array(0.0, 1.0)))
  class MarkovFactor(b1: BooleanVariable, b2: BooleanVariable) extends DotFactorWithStatistics2(b1, b2) {
    def weights = markovWeights
    override def factorName = "MarkovFactor"
  }
  val inputWeights = new DenseTensor2(Array(Array(1.0, -1.0), Array(-1.0, 1.0)))
  class InputFactor(bi: BooleanVariable, bo: BooleanVariable) extends DotFactorWithStatistics2(bi, bo) {
    def weights = inputWeights
    override def factorName = "InputFactor"
  }

  // ItemizedModel stores a given set of Factors, with their relations to Variables indexed by HashMaps.
  val m1 = new ItemizedModel
  m1 ++= (for (i <- 0 until 9) yield new MarkovFactor(outputs(i), outputs(i + 1)))
  m1 ++= inputs.zip(outputs).map({ case (i: BooleanVariable, o: BooleanVariable) => new InputFactor(i, o) })

  val f1 = m1.factors(outputs)
  println(f1)
}
