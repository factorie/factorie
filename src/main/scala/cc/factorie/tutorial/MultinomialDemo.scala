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



package cc.factorie.tutorial
import cc.factorie._
import cc.factorie.directed._
import cc.factorie.directed.Discrete
import cc.factorie.variable.{DenseProportions1, ProportionsVariable, DiscreteVariable, DiscreteDomain}
import cc.factorie.infer.Maximize

/** A simple example of generating data by rolling a die, then re-estimating the parameters of the die from that data. */
object MultinomialDemo {
  val numSides = 6
  object RollDomain extends DiscreteDomain(numSides)
  class Roll extends DiscreteVariable { def domain = RollDomain }
  implicit val model = DirectedModel() // ItemizedDirectedModel

  def main(args:Array[String]) : Unit = {
    implicit val random = new scala.util.Random(0)
    val die = new ProportionsVariable(new DenseProportions1(Array(.1, .2, .3, .2, .15, .05)))
    // println("True distribution "+die)
    val rolls = for (i <- 1 to 1000) yield new Roll :~ Discrete(die)
    Maximize(die)
  }

}
