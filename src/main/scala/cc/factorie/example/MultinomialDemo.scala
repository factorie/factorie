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



package cc.factorie.example
import cc.factorie._
import cc.factorie.generative._
// Next two lines to get the implicit Estimator objects
//import cc.factorie.generative.DenseProportions._
//import cc.factorie.generative.DenseCountsProportions._

object MultinomialDemo {
  val numSides = 6
  class Roll(die:Proportions, value:Int) extends Discrete(die, value)
  Domain[Roll].size = () => numSides // TODO Make this unnecessary

  def main(args:Array[String]) : Unit = {
    val die = new DenseProportions(List(.1, .2, .3, .2, .2))
    println("True distribution "+die)
    val rolls = for (i <- 1 to 1000) yield new Roll(die, die.sampleInt)
    rolls.foreach(_.sampleFromParents(null))
    die.estimate()
    println("Est  distribution "+die)

    val r = new scala.util.Random
    val die2 = new GrowableDenseCountsProportions
    val rolls2 = for (i <- 1 to 1000) yield new Roll(die2, r.nextInt(6))
    die2.estimate()
    println("Die2 "+die2)
  }

}
