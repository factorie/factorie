package cc.factorie.example

import cc.factorie._
import cc.factorie.generative._

object GaussianMixture {
  
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
//    //zs.foreach(z => z.sample(null))
//    gs.foreach(g => println("weightedGeneratedSamples "+g.weightedGeneratedSamples.toList))
//    gs.foreach(_.estimate)
//    println("Estimated means from current assignments "+gs.map(_.mean))
//    println(xs.map(x => (x,x.generativeSource)))
//    
//    for (iteration <- 1 to 30) {
//      zs.foreach(_.sample(null))
//      gs.foreach(_.estimate)
//      println("Estimated means from current assignments "+gs)
//      //println("Estimated means from current assignments "+gs.map(_.mean))
//      //println(xs.map(x => (x,x.generativeSource)))
//    }
//    //println(gs.toList)
//    //println(xs.map(x => (x,x.generativeSource)))
//  }
  
}
