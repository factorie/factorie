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


object GaussianMixtureDemo {
  def main(args:Array[String]): Unit = {
    val numComponents = 2
    implicit val model = GenerativeModel()
    object ZDomain extends DiscreteDomain(numComponents)
    class Z extends DiscreteVariable(random.nextInt(numComponents)) { def domain = ZDomain }
    val meanComponents = Mixture(numComponents)(new DoubleVariable(random.nextDouble * 10))
    val varianceComponents = Mixture(numComponents)(new DoubleVariable(1.0))
    val mixtureProportions = ProportionsVariable.uniform(numComponents)
    // Generate some data
    val data = for (i <- 1 to 100) yield {
      val z = new Z :~ Discrete(mixtureProportions)
      new DoubleVariable() :~ GaussianMixture(meanComponents, varianceComponents, z)
    }
    // A convenience function for getting the Z for a particular DoubleVar data variable x
    def z(x:DoubleVar): Z = model.parentFactor(x).asInstanceOf[GaussianMixture.Factor]._4.asInstanceOf[Z]
    // Get the list of Z variables, so we can pass it into the EMInferencer
    val zs = data.map(z(_))

    // Show a little example data
    data.take(50).foreach(x => println(x+"  z="+z(x).intValue))
    val origMeans = meanComponents.map(_.doubleValue)

    // Now randomly re-assign variable values so we can do the work of re-estimating them
    zs.foreach(_.set(random.nextInt(numComponents))(null))
    meanComponents.foreach(_.set(random.nextDouble)(null))

    // Estimate means and zs by EM
    val em = EMInferencer(meanComponents, zs, model, MaximizeGaussianMean)
    for (i <- 1 to 10) {
      em.process(1)
      println("Estimated means at iteration "+i)
      meanComponents.foreach(m => println(m.doubleValue))
    }
    println("\nOriginal means")
    origMeans.foreach(println(_))
  }
}  


  
//  // The Gaussian mixture components and their mixture weights
//  class G(initialMean:Double) extends Gaussian1[X](initialMean, 1.0) with MixtureComponent[G,X] {
//    override def toString = "G(mean="+this.mean.doubleValue+")"
//  }
//  object mixtureWeights extends UniformMultinomial[Z]
//  // The real-valued data
//  class X extends GeneratedRealVariable[X]
//  // The per-X mixture component membership indicators 
//  class Z extends MarginalizedMixtureChoice[G,X,Z]
//  object alpha extends SymmetricDirichlet[Z](1.0)
//
//  def main(args:Array[String]): Unit = {
//    val numMixtureComponents = 3
//    val numDataPoints = 200
//    // Generative storyline
//    val gs = for (i <- 1 to numMixtureComponents) yield new G(i)
//    val zs = for (i <- 1 to numDataPoints) yield new Z ~ mixtureWeights
//    val xs = zs.map(z => new X :~ [G](z)).toList
//   
//    // Print initial state
//    println("Original means "+gs.map(_.mean))
//    println(xs.map(x => (x,x.generativeSource)))
//   
//    // Randomize assignment of x's to mixture components 
//    zs.foreach(_.setRandomly)
//    //zs.foreach(z => { z.multinomial :== alpha; /*z := z.multinomial.sampleInt*/ })
//    println(xs.map(x => (x,x.generativeSource)))
//    //zs.foreach(z => z.sampleFromParents(null))
//    gs.foreach(g => println("weightedGeneratedSamples "+g.weightedGeneratedSamples.toList))
//    gs.foreach(_.estimate)
//    println("Estimated means from current assignments "+gs.map(_.mean))
//    println(xs.map(x => (x,x.generativeSource)))
//    
//    for (iteration <- 1 to 30) {
//      zs.foreach(_.sampleFromParents(null))
//      gs.foreach(_.estimate)
//      println("Estimated means from current assignments "+gs)
//      //println("Estimated means from current assignments "+gs.map(_.mean))
//      //println(xs.map(x => (x,x.generativeSource)))
//    }
//    //println(gs.toList)
//    //println(xs.map(x => (x,x.generativeSource)))
//  }
  
