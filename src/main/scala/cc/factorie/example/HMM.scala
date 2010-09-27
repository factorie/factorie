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
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords

object HMMDemo {
  val numStates = 10
  class Z(ps:FiniteMixture[Proportions], c:MixtureChoiceVariable, i:Int) extends MixtureChoiceMixture(ps, c, i); Domain[Z].size = numStates
  class Zi(p:Proportions, i:Int) extends MixtureChoice(p, i); Domain[Zi].size = numStates
  class Word(ps:FiniteMixture[Proportions], z:Z, value:String) extends CategoricalMixture(ps, z, value) with VarInTypedSeq[Word,Sentence]
  class Sentence(val file:String, val startState:Zi) extends VariableSeq[Word]

  def main(args: Array[String]) : Unit = {
    val keepStopwords = true
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val lexer = new Regex("[a-zA-Z]+")

    // Read data and create generative variables
    val transitions = FiniteMixture(numStates)(new DenseDirichlet(numStates, 1.0))
    val emissions = FiniteMixture(numStates)(new GrowableDenseDirichlet(0.1) with TypedProportions[Word])
    val pi = new DenseDirichlet(numStates, 1.0)
    var sentences = new ArrayBuffer[Sentence]
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val sentence = new Sentence(file.toString, new Zi(pi, cc.factorie.random.nextInt(numStates)))
        for (word <- cc.factorie.app.strings.alphaSegmenter(file).map(_ toLowerCase).filter(keepStopwords || !Stopwords.contains(_))) {
          val z = new Z(transitions, if (sentence.length > 0) sentence.last.choice else sentence.startState, cc.factorie.random.nextInt(numStates))
          sentence += new Word(emissions, z, word)
        }
        sentences += sentence
      }
    }
    println("Read "+sentences.size+" sentences with "+sentences.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
    //sentences = sentences.take(10)

    // Fit model
    val zs = sentences.flatMap(sentence => sentence.map(word => word.choice)) ++ sentences.map(_.startState)
    val sampler = new CollapsedGibbsSampler(transitions ++ emissions ++ List(pi))
    //val sampler = new CollapsedVariationalBayes(zs)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 100) {
      zs.foreach(sampler.process(_))
      print("."); Console.flush
      if (i % 5 == 0) {
        println ("Iteration "+i)
        sampler.export()
        emissions.foreach(t => println("Emissions   "+emissions.indexOf(t)+"  "+t.top(15).map(_.value)))
        transitions.foreach(t => println("Transitions "+transitions.indexOf(t)+"  "+t.top(numStates).map(d => "%d %-5f".format(d.index,d.pr))))
        println
      }
    } 
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }

}
