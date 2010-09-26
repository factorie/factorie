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

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.collection.mutable.WeakHashMap
import scala.util.Sorting
import scala.reflect.Manifest
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords

/** Implements "Cluster LDA" from Hanna Wallach's thesis.
    Runs, but would benefit from a DirichletMultinomial implementation. */

object ClusterLDADemo {
  val filterStopwords = false
  val numTopics = 10
  val numClusters = 3
  class Z(p:Proportions, value:Int = random.nextInt(numTopics)) extends MixtureChoice(p, value); Domain[Z].size = numTopics
  //println("ClusterLDADemo.Domain[Z].size="+Domain[Z].size)
  class Y(p:Proportions, value:Int = random.nextInt(numClusters)) extends MixtureChoice(p, value); Domain[Y].size = numClusters
  class Word(ps:FiniteMixture[Proportions], z:MixtureChoiceVariable, value:String) extends CategoricalMixture[String](ps, z, value)
  class Document(val file:String) extends ArrayBuffer[Word] { var theta:MutableDirichlet = null; var y:Y = _ }

  def main(args: Array[String]) : Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val lexer = new Regex("[a-zA-Z]+")

    // Read data and create generative variables
    val phis = FiniteMixture(numTopics)(new GrowableDenseDirichlet(0.01) with TypedProportions[Word] { override def toString = "Phi("+countsSeq.toList+")" })
    val alphaMeans = FiniteMixture(numClusters)(new DenseProportions(numTopics))
    val alphaPrecisions = FiniteMixture(numClusters)(new RealVariableParameter(numTopics))
    val clusterProportions = new UniformProportions(numClusters)
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory "+directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val doc = new Document(file.toString)
        doc.y = new Y(clusterProportions)
        doc.theta = new DenseDirichletMixture(alphaMeans, alphaPrecisions, doc.y) // (numTopics, 0.01)
        var words = lexer.findAllIn(Source.fromFile(file).mkString).map(_ toLowerCase)
        if (filterStopwords) words = words.filter(!Stopwords.contains(_))
        for (word <- words) {
          val z = new Z(doc.theta)
          doc += new Word(phis, z, word)
        }
        documents += doc
      }
    }
    println("\nRead "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
    //forIndex(numTopics)(i => println("Topic x z-count = "+documents.flatMap(doc => doc.filter(word => word.choice.intValue == i)).size))
  
    // Fit model
    val zs = documents.flatMap(document => document.map(word => word.choice))

    val numClusteringIterations = 20
    val numGibbsIterationsPerClustering = 10
    val startTime = System.currentTimeMillis
    for (j <- 1 to numClusteringIterations) {
      // Sample z's
      val sampler = new CollapsedGibbsSampler(phis ++ documents.map(_.theta))
      for (i <- 1 to numGibbsIterationsPerClustering) {
        zs.foreach(sampler.process(_))
        print("."); Console.flush
        if (i % 5 == 0) {
          println ("Iteration "+i)
          phis.foreach(t => println("Topic "+phis.indexOf(t)+"  "+sampler.collapsed(t).top(10).map(dp => Domain[Word].get(dp.index)).mkString(" ")))
          println
        }
      }
      // Set proportions to the means of their collapsed representations
      (phis ++ documents.map(_.theta)).foreach(p => p.set(sampler.collapsed(p))(null))
      // Re-estimate per-cluster parameters and sample y's
      for (i <- 0 until numClusters) {
        DirichletMomentMatching.estimate(alphaMeans(i), alphaPrecisions(i))
        println("Cluster "+i+" "+alphaMeans(i).mkString(" "))
      }
      val ySampler = new GibbsSampler()
      documents.map(_.y).foreach(ySampler.process(_))
      // Then we must re-collapse the sampler for z's above, because it isn't yet smart enough to work with mixtures of Dirichlet's
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }




  val datadirs1 = Array(
"/Users/mccallum/research/data/text/nipstxt/nips10",
"/Users/mccallum/research/data/text/nipstxt/nips11",
"/Users/mccallum/research/data/text/nipstxt/nips12"
  )
  val datadirs2 = Array(
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/acq",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/alum",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/barley",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/bop",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/carcass",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/castor-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/cocoa",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/coconut",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/coconut-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/coffee",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/copper",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/copra-cake",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/corn",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/cotton",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/cotton-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/cpi",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/cpu",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/crude",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/dfl",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/dlr",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/dmk",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/earn",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/fuel",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/gas",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/gnp",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/gold",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/grain",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/groundnut",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/groundnut-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/heat",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/hog",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/housing",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/income",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/instal-debt",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/interest",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/ipi",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/iron-steel",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/jet",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/jobs",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/l-cattle",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/lead",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/lei",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/lin-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/livestock",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/lumber",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/meal-feed",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/money-fx",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/money-supply",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/naphtha",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/nat-gas",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/nickel",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/nkr",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/nzdlr",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/oat",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/oilseed",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/orange",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/palladium",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/palm-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/palmkernel",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/pet-chem",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/platinum",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/potato",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/propane",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rand",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rape-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rapeseed",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/reserves",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/retail",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rice",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rubber",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/rye",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/ship",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/silver",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/sorghum",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/soy-meal",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/soy-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/soybean",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/strategic-metal",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/sugar",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/sun-meal",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/sun-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/sunseed",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/tea",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/tin",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/trade",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/veg-oil",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/wheat",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/wpi",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/yen",
"/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/zinc"
  )
}
