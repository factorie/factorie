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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter

object LDADemo {
  val numTopics = 10
  class Z(p: Proportions, value: Int) extends MixtureChoice(p, value)
  Domain[Z].size = numTopics
  class Word(ps: FiniteMixture[Proportions], z: MixtureChoiceVariable, value: String) extends CategoricalMixture[String](ps, z, value)
  class Document(val file: String) extends ArrayBuffer[Word] {var theta: MutableDirichlet = null}

  def main(args: Array[String]): Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")

    // Read data and create generative variables
    val phis = FiniteMixture(numTopics)(new GrowableDenseDirichlet(0.01) with TypedProportions[Word] {override def toString = "Phi(" + countsSeq.toList + ")"})
    val alphaMean = new DenseProportions(numTopics)
    val alphaPrecision = new RealVariableParameter(numTopics)
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val doc = new Document(file.toString)
        doc.theta = new DenseDirichlet(alphaMean, alphaPrecision) //(numTopics, 0.01) // Shouldn't this have been 1.0 instead of 0.01 anyway?
        for (word <- alphaSegmenter(file).map(_ toLowerCase).filter(!Stopwords.contains(_))) {
          val z = new Z(doc.theta, cc.factorie.random.nextInt(numTopics))
          doc += new Word(phis, z, word)
        }
        documents += doc
      }
    }
    println("\nRead " + documents.size + " documents with " + documents.foldLeft(0)(_ + _.size) + " tokens and " + Domain[Word].size + " types.")

    // Fit model
    val zs = documents.flatMap(document => document.map(word => word.choice))

    //val collapsed = new ArrayBuffer[CollapsedParameter]; collapsed ++= phis; collapsed ++= documents.map(_.theta)
    //for (c <- collapsed) println("LDA collapsed parent="+c.getClass.getName+"@"+c.hashCode+" #children="+c.children.size)

    val sampler = new CollapsedGibbsSampler(phis ++ documents.map(_.theta))
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      zs.foreach(sampler.process(_))
      print("."); Console.flush
      if (i % 5 == 0) {
        println("Iteration " + i)
        // (phis ++ documents.map(_.theta)).foreach(_.export)
        (phis ++ documents.map(_.theta)).foreach(p => p.set(sampler.collapsed(p))(null))
        DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + sampler.collapsed(t).top(10).map(dp => Domain[Word].get(dp.index)).mkString(" ")))
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
}

