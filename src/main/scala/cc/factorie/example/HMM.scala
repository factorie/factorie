/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.matching.Regex
import java.io.File
import cc.factorie._
import cc.factorie.util.Stopwords

/*
object HMMDemo {
  val numStates = 10
  class Z(ps:Seq[Proportions], c:MixtureChoice) extends MixtureChoiceMixture(ps, c); Domain[Z].size = numStates
  class Word(ps:Seq[Proportions], z:MixtureChoice, value:String) extends CategoricalMixture(ps, z, value)
  class Sentence(val file:String) extends VariableSequence[Word]

  def main(args: Array[String]) : Unit = {
    // Read observed data and create Documents
    val documents = new ArrayBuffer[Document];
    val lexer = new Regex("[a-zA-Z]+")
    for (directory <- if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips05")) {
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val d = new Document(file.toString)
        d ++= lexer.findAllIn(file.contentsAsString).toList.map(_ toLowerCase).filter(!Stopwords.contains(_)).map(new Word(_))
        documents += d
      }
    }
    println("Read "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
    // Create random variables and tell generative storyline
    //val betaMean = new UniformProportions(numTopics)
    //val betaPrecision = new RealVariableParameter(numTopics * 0.01)
    //val alphaMean = new UniformProportions(Domain[Word].size)
    val od = Array.tabulate(numState)(i => new DenseDirichlet(Domain[Word].size, 0.01)
    val td = Array.tabulate(numState)(i => new DenseDirichlet(numStates, 0.01)
    val pi = new DenseDirichlet(numStates, 0.01)
    var z: Z = new Z(pi, null)
    for (sentence <- sentences; word <- sentence) {
      z = new Z(td, z)
      val word = new Word(od, z, string)
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
        topics.foreach(t => println("Topic "+t.mixtureIndex+"  "+t.top(20).map(_.value))); println
      }
    } 
    topics.foreach(t => {println("\nTopic "+t.mixtureIndex); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }
}
*/
