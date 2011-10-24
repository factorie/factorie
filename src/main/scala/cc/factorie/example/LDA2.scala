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

object LDA2 {
  
  val numTopics = 10
  object ZDomain extends DiscreteDomain { def size = numTopics }
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(len:Int) extends PlatedGateVariable(len) { def domain = ZSeqDomain }
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val WordDomain = WordSeqDomain.elementDomain
  class Words(strings:Seq[String]) extends PlatedCategoricalVariable(strings) {
    def domain = WordSeqDomain
    def zs = defaultGenerativeModel.parentFactor(this).asInstanceOf[PlatedDiscreteMixture.Factor]._3
  }
  class Document(val file:String, val theta:CountsProportions, strings:Seq[String]) extends Words(strings)
  val beta = new GrowableUniformMasses(WordDomain, 0.1)
  val alphas = new DenseMasses(numTopics, 0.1)

  def main(args: Array[String]): Unit = {
    val directories = if (args.length > 0) args.toList else List("12", "11", "10", "09", "08").take(1).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
    val phis = Mixture(numTopics)(new GrowableDenseCountsProportions(WordDomain) ~ Dirichlet(beta))
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val theta = new DenseCountsProportions(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_ toLowerCase).filter(!Stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        documents += new Document(file.toString, theta, tokens) ~ PlatedDiscreteMixture(phis, zs)
      }
      println()
    }

    val collapse = new ArrayBuffer[Variable]
    collapse += phis
    collapse ++= documents.map(_.theta)
    val sampler = new CollapsedGibbsSampler(collapse)
    //println("Initialization:"); phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => WordDomain.getCategory(dp.index)).mkString(" ")))

    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      for (doc <- documents) sampler.process(doc.zs)
      if (i % 5 == 0) {
        println("Iteration " + i)
        sampler.export()
        // Turned off hyperparameter optimization
        //DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        //println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => WordDomain.getCategory(dp.index)).mkString(" ")+"  "+t.countsTotal.toInt))
        println("Total words "+phis.map(_.countsTotal).sum.toInt)
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
}

  /*
  val numTopics = 10
  object ZDomain extends DiscreteDomain { def size = numTopics }
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(p: Proportions) extends PlatedMixtureChoice(p, Nil) { def domain = ZSeqDomain }
  //object WordDomain extends CategoricalDomain[String]
  object WordSeqDomain extends CategoricalSeqDomain[String] // { def elementDomain = WordDomain }
  val WordDomain = WordSeqDomain.elementDomain
  class Words(ps: CollapsibleFiniteMixture[Dirichlet], zs: PlatedMixtureChoice) extends PlatedCategoricalMixture[String](ps, zs, Nil) {
    def domain = WordSeqDomain
  }
  class Document(val file:String, var theta: DenseDirichlet, phis:CollapsibleFiniteMixture[Dirichlet], zs:PlatedMixtureChoice) extends Words(phis, zs)

  def main(args: Array[String]): Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")

    // Read data and create generative variables
    val phis = CollapsibleFiniteMixture(numTopics)(new GrowableDenseDirichlet(0.01, WordDomain) with CategoricalProportions[String] {
      override def apply(index:Int) : Double = {
        val result = super.apply(index)
        //println("LDA.phi.apply "+index+" "+result)
        result
      }
      def categoricalDomain = WordSeqDomain.elementDomain
      override def toString = "Phi(" + countsSeq.take(20).toList + ")"
    })
    val alphaMean = new DenseProportions(numTopics)
    val alphaPrecision = new RealVariableParameter(numTopics)
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val theta = new DenseDirichlet(alphaMean, alphaPrecision, Nil) //(numTopics, 0.01) // Shouldn't this have been 1.0 instead of 0.01 anyway?
        val zs = new Zs(theta)
        val doc = new Document(file.toString, theta, phis, zs)
        for (word <- alphaSegmenter(file).map(_ toLowerCase).filter(!Stopwords.contains(_))) {
          zs appendInt cc.factorie.random.nextInt(numTopics)
          doc appendCategory word
        }
        documents += doc
      }
    }
    //documents.trimEnd(documents.length-10)

    println("\nRead " + documents.size + " documents with " + documents.foldLeft(0)(_ + _.size) + " tokens and " + WordDomain.size + " types.")
    // Fit model
    val zss: Seq[PlatedMixtureChoiceVar] = documents.map(document => document.choice)

    //val collapsed = new ArrayBuffer[CollapsedParameter]; collapsed ++= phis; collapsed ++= documents.map(_.theta)
    //for (c <- collapsed) println("LDA collapsed parent="+c.getClass.getName+"@"+c.hashCode+" #children="+c.children.size)

    val collapsedVariables = new ArrayBuffer[CollapsibleParameter]
    collapsedVariables += phis
    collapsedVariables ++= documents.map(_.theta)
    val sampler = new CollapsedGibbsSampler(collapsedVariables)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      zss.foreach(sampler.process(_))
      print("."); Console.flush
      if (i % 5 == 0) {
        println("Iteration " + i)
        // (phis ++ documents.map(_.theta)).foreach(_.export)
        sampler.export()
        //(phis ++ documents.map(_.theta)).foreach(p => p.set(sampler.collapsed(p).value)(null))
        DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => WordDomain.getCategory(dp.index)).mkString(" ")))
        //forIndex(numTopics)(i => println("Topic " +i+"  "+ sampler.collapsed(phis).asInstanceOf[CollapsedFiniteMixture[DirichletMultinomial]].apply(i).top(10).map(dp => WordDomain.getCategory(dp.index)).mkString(" ")))
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
  */
