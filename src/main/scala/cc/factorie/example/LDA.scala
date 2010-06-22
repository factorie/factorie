/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.util.Stopwords

object LDADemo {
  val numTopics = 10
  class Z(p:Proportions) extends MixtureChoice(p); Domain[Z].size = numTopics
  class Word(ps:Seq[Proportions], z:MixtureChoice, value:String) extends CategoricalMixture[String](ps, z, value)
  class Document(val file:String) extends ArrayBuffer[Word] { var theta:Proportions = _ }

  def main(args: Array[String]) : Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips05")
    val lexer = new Regex("[a-zA-Z]+")
    // Read observed data and create Documents
    val documents = new ArrayBuffer[Document];
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val d = new Document(file.toString)
        d ++= lexer.findAllIn(Source.fromFile(file).toString).toList.map(_ toLowerCase).filter(!Stopwords.contains(_)).map(new Word(Nil, null, _))
        documents += d
      }
    }
    println("Read "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
    // Create random variables and tell generative storyline
    //val betaMean = new UniformProportions(numTopics)
    //val betaPrecision = new RealVariableParameter(numTopics * 0.01)
    //val alphaMean = new UniformProportions(Domain[Word].size)
    val phis = for (i <- 1 to numTopics) yield new DenseDirichletMultinomial(Domain[Word].size, 0.01) with TypedProportions[Word]
    for (document <- documents) {
      document.theta = new DenseDirichletMultinomial(numTopics, 0.01)  // new DenseDirichlet(...)
      for (i <- 0 until document.length) {
        val z = new Z(document.theta)
        document(i) = new Word(phis, z, document(i).value)
      }
    }
    
    // Fit model
    val zs = documents.flatMap(document => document.map(word => word.choice))
    val sampler = new CollapsedGibbsSampler
    val startTime = System.currentTimeMillis
    for (i <- 1 to 100) {
      sampler.process(zs, 1)
      print("."); Console.flush
      if (i % 3 == 0) {
        println ("Iteration "+i)
        phis.foreach(t => println("Topic "+phis.indexOf(t)+"  "+t.top(20).map(_.value))); println
      }
    } 
    phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }
}
