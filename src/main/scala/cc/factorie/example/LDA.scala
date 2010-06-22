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
  class Z(p:Proportions, value:Int) extends MixtureChoice(p, value); Domain[Z].size = numTopics
  class Word(ps:Seq[Proportions], z:MixtureChoice, value:String) extends CategoricalMixture[String](ps, z, value)
  class Document(val file:String) extends ArrayBuffer[Word] { var theta:Proportions = _ }

  def main(args: Array[String]) : Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips05")
    val lexer = new Regex("[a-zA-Z]+")

    // Read data and create generative variables
    val phis = for (i <- 1 to numTopics) yield new GrowableDenseDirichletMultinomial(0.01) with TypedProportions[Word]
    val documents = new ArrayBuffer[Document];
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val doc = new Document(file.toString)
        doc.theta = new DenseDirichletMultinomial(numTopics, 0.01)
        for (word <- lexer.findAllIn(Source.fromFile(file).toString).toList.map(_ toLowerCase).filter(!Stopwords.contains(_))) {
          val z = new Z(doc.theta, doc.theta.sampleInt)
          doc += new Word(phis, z, word)
        }
        documents += doc
      }
    }
    println("Read "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
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
