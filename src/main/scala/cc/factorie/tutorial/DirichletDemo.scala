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
import cc.factorie.directed.{Discrete, MaximizeDirichletByMomentMatching, Dirichlet}
import cc.factorie.variable._
import cc.factorie.infer.Maximize

/** Simple demonstration of Dirichlet-distributed proportions generating Discrete values. */
object DirichletDemo {

  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    object WordDomain extends EnumDomain { val a, b, c, d, e, f = Value }
    class Word extends DiscreteVariable { def domain = WordDomain }
    implicit val model = DirectedModel()
    
    val masses = new MassesVariable(new DenseMasses1(WordDomain.size, 2.0))
    val p1 = new ProportionsVariable(new DenseProportions1(WordDomain.size))
    p1 :~ Dirichlet(masses)

    //println("Demonstrating Proportions estimation")
    //println("Initial Proportions "+p1.value)
    val data = for (i <- 0 until 500) yield new Word :~ Discrete(p1)
    MaximizeProportions.infer(Seq(p1), model)
    Maximize(Seq(p1), model)
    //println("Estimated Proportions "+p1.value)
    
    //println("Demonstrating Dirichlet parameter estimation")
    //println("Initial Masses "+masses.value)
    val ps = for (i <- 0 until 1000) yield ProportionsVariable.dense(WordDomain.size) :~ Dirichlet(masses)
    MaximizeDirichletByMomentMatching(masses, model)
    //println("Estimated Masses "+masses.value)
    
    // TODO I'm seeing consistent over-estimates of precision from MaximizeDirichletByMomentMatching
    // It could be a problem in the estimator, or in the Dirichlet.sample implementations.
    // Needs investigation -akm

  }
  
}
