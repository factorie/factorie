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

/** Simple demonstration of Dirichlet-distributed proportions generating Discrete values. */
object DirichletDemo {

  def main(args:Array[String]): Unit = {
    object WordDomain extends EnumDomain { val a, b, c, d, e, f = Value }
    class Word extends CategoricalVariable[String] { def domain = WordDomain }
    implicit val model = GenerativeModel()
    
    val m = new MassesVariable(new DenseMasses1(WordDomain.size, 2.0))
    val p = new ProportionsVariable(new DenseProportions1(WordDomain.size))
    p :~ Dirichlet(m)
    
    // TODO Implement mkString in DoubleSeq
    println("Initial Proportions "+p.value.asSeq.mkString(" "))
    
    val data = for (i <- 0 until 100) yield new Word :~ Discrete(p)
    
    Maximize(Seq(p), model)

    println("Estimated Proportions "+p.value.asSeq.mkString(" "))

    /*
    object WordDomain extends EnumDomain {
      val one, two, three, four, five, six = Value
    }
    class Word(p: Proportions, s:String) extends Categorical(p, s) {
      def domain = WordDomain
    }
   
    val dirmean = new DenseProportions(WordDomain.size)
    val dirprec = new RealParameter(1.0)
    println("Dirichlet1 mean = "+dirmean.toSeq)
    println("Dirichlet1 prec = "+dirprec.doubleValue)
   
    val n = 10000
    println("Sampling "+n+" proportions")
    val proportions = for (i <- 1 to 10000) yield 
      new DenseDirichlet(dirmean, dirprec).sampleFromParents()
   
    println("Example multinomials")
    proportions.take(4).foreach(m => {
      print("prop "); m.foreach(p => print("%8f ".format(p))); println
    })
  
    println("Estimating Dirichlet2 parameters from sampled Multinomials")
    DirichletMomentMatching.estimate(dirmean, dirprec)
    println("Dirichlet2 mean = "+dirmean.toSeq)
    println("Dirichlet2 prec = "+dirprec.doubleValue)
*/
  }
  
}
