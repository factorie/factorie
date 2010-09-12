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

/*
object DirichletDemo {

  def main(args:Array[String]) : Unit = {
    class Word(s:String) extends CategoricalVariable(s) with GeneratedValue
    Domain += new StringDomain[Word] {
      List("one", "two", "three", "four", "five", "six").foreach(index(_))
    }
   
    //val dir  = new Dirichlet[Word](List(1.0,2.0,3.0,4.0,5.0,6.0)) with DirichletMomentMatchingEstimator[Word];
    //val dir  = new Dirichlet[Word](List(1.0,1.0,1.0,1.0,1.0,1.0)) with DirichletMomentMatchingEstimator[Word];
    val dir  = new Dirichlet[Word](List(.1,.1,.1,.1,.1,.1)) with DirichletMomentMatchingEstimator[Word];
    //val dir  = new Dirichlet[Word](List(3.0,3.0,3.0,3.0,3.0,3.0)) with DirichletMomentMatchingEstimator[Word];
    
    println("Dirichlet1 = "+dir.alphas.toList)
    println("Dirichlet1 mean = "+dir.mean.toList)
   
    val n = 10000
    println("Sampling "+n+" multinomials")
    val multinomials = for (i <- 1 to 10000) yield dir.sampleMultinomial
   
    println("Example multinomials")
    multinomials.take(4).foreach(m => {print("mult "); m.foreach(p => print("%8f ".format(p))); println})
  
    val dir2 = new Dirichlet[Word](1.0) with DirichletMomentMatchingEstimator[Word];
    // Commented for Scala 2.8
    //multinomials.foreach(m => m ~ dir2)

    //println("Estimating Dirichlet2 parameters from sampled Multinomials")
    //dir2.estimate
    //println("Dirichlet2 = "+dir2.alphas.toList)
    //println("Dirichlet2 mean = "+dir2.mean.toList)

    0
  }
  
}
*/
