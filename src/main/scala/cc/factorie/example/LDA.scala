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
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter
import generative._

object LDA {
  val numTopics = 10
  implicit val model = GenerativeModel()
  object ZDomain extends DiscreteDomain(numTopics)
  class Z(value: Int = 0) extends DiscreteVariable(value) { def domain = ZDomain }
  object WordDomain extends CategoricalDomain[String]
  class Word(value: String) extends CategoricalVariable(value) { def domain = WordDomain; def z = model.parentFactor(this).asInstanceOf[CategoricalMixture[String]#Factor]._3 }
  class Document(val file: String) extends ArrayBuffer[Word] {var theta: ProportionsVariable = null}
  val beta = MassesVariable.growableUniform(WordDomain, 0.1)
  val alphas = MassesVariable.dense(numTopics, 0.1)
  val phis = Mixture(numTopics)(ProportionsVariable.growableDense(WordDomain) ~ Dirichlet(beta))

  def main(args: Array[String]): Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val doc = new Document(file.toString)
        doc.theta = ProportionsVariable.dense(numTopics) ~ Dirichlet(alphas)
        for (word <- alphaSegmenter(file).map(_ toLowerCase).filter(!Stopwords.contains(_))) {
          val z = new Z :~ Discrete(doc.theta)
          doc +=  new Word(word) :~ CategoricalMixture(phis, z)
        }
        documents += doc
      }
    }
    println("\nRead "+documents.size+" documents, containing "+documents.map(_.size).sum+" tokens.")

    val collapse = new ArrayBuffer[Variable]
    collapse += phis
    collapse ++= documents.map(_.theta)
    val sampler = new CollapsedGibbsSampler(collapse, model)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      for (doc <- documents; word <- doc) sampler.process(word.z)
      if (i % 5 == 0) {
        println("Iteration " + i)
        sampler.export()
        // Turned off hyperparameter optimization
        //DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        //println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.value.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")))
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")

  }
}
