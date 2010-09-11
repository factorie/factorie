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
import cc.factorie.generative._
import cc.factorie.util.Stopwords

object LDADemo {
  val numTopics = 10
  class Z(p:Proportions, value:Int) extends MixtureChoice(p, value); Domain[Z].size = numTopics
  class Word(ps:FiniteMixture[Proportions], z:MixtureChoiceVariable, value:String) extends CategoricalMixture[String](ps, z, value)
  class Document(val file:String) extends ArrayBuffer[Word] { var theta:MutableDirichlet = null }

  def main(args: Array[String]) : Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val lexer = new Regex("[a-zA-Z]+")

    // Read data and create generative variables
    val phis = FiniteMixture(numTopics)(new GrowableDenseDirichlet(0.01) with TypedProportions[Word] { override def toString = "Phi("+countsSeq.toList+")" })
    val alphaMean = new DenseProportions(numTopics)
    val alphaPrecision = new RealVariableParameter(numTopics)
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory "+directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val doc = new Document(file.toString)
        doc.theta = new DenseDirichlet(alphaMean, alphaPrecision) //(numTopics, 0.01) // Shouldn't this have been 1.0 instead of 0.01 anyway?
        for (word <- lexer.findAllIn(Source.fromFile(file).mkString).map(_ toLowerCase).filter(!Stopwords.contains(_))) {
          val z = new Z(doc.theta, cc.factorie.random.nextInt(numTopics))
          doc += new Word(phis, z, word)
        }
        documents += doc
      }
    }
    println("\nRead "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
    // Fit model
    val zs = documents.flatMap(document => document.map(word => word.choice))

    //val collapsed = new ArrayBuffer[CollapsedParameter]; collapsed ++= phis; collapsed ++= documents.map(_.theta)
    //for (c <- collapsed) println("LDA collapsed parent="+c.getClass.getName+"@"+c.hashCode+" #children="+c.children.size)

    val sampler = new CollapsedGibbsSampler(phis ++ documents.map(_.theta))
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      sampler.processAll(zs)
      print("."); Console.flush
      if (i % 5 == 0) {
        println ("Iteration "+i)
        (phis ++ documents.map(_.theta)).foreach(p => p.set(sampler.collapsed(p))(null))
        DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        println("alpha = "+alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic "+phis.indexOf(t)+"  "+sampler.collapsed(t).top(10).map(dp => Domain[Word].get(dp.index)).mkString(" ")))
        println
      }
    } 
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }
}

