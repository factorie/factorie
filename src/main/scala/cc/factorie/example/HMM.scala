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

object HMMDemo {
  val numStates = 10
  class Z(ps:Seq[Proportions], c:MixtureChoiceVariable, i:Int) extends MixtureChoiceMixture(ps, c, i); Domain[Z].size = numStates
  class Zi(p:Proportions, i:Int) extends MixtureChoice(p, i); Domain[Zi].size = numStates
  class Word(ps:Seq[Proportions], z:Z, value:String) extends CategoricalMixture(ps, z, value) with VarInTypedSeq[Word,Sentence]
  class Sentence(val file:String, val startState:Zi) extends VariableSeq[Word]

  def main(args: Array[String]) : Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val lexer = new Regex("[a-zA-Z]+")

    // Read data and create generative variables
    val transitions = for (i <- 1 to numStates) yield new DenseDirichletMultinomial(numStates, 1.0)
    val emissions = for (i <- 1 to numStates) yield new GrowableDenseDirichletMultinomial(0.01) with TypedProportions[Word]
    val sentences = new ArrayBuffer[Sentence];
    val pi = new DenseDirichletMultinomial(numStates, 1.0)
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val sentence = new Sentence(file.toString, new Zi(pi, Global.random.nextInt(numStates)))
        for (word <- lexer.findAllIn(Source.fromFile(file).mkString).map(_ toLowerCase)) {
          val z = new Z(transitions, if (sentence.length > 0) sentence.last.choice else sentence.startState, Global.random.nextInt(numStates))
          sentence += new Word(emissions, z, word)
        }
        sentences += sentence
      }
    }
    println("Read "+sentences.size+" sentences with "+sentences.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")

    // Fit model
    val zs = sentences.flatMap(sentence => sentence.map(word => word.choice)) ++ sentences.map(_.startState)
    val sampler = new CollapsedGibbsSampler
    //val sampler = new CollapsedVariationalBayes(zs)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 50) {
      sampler.process(zs, 1)
      print("."); Console.flush
      if (i % 5 == 0) {
        println ("Iteration "+i)
        emissions.foreach(t => println("Emissions   "+emissions.indexOf(t)+"  "+t.top(15).map(_.value)))
        transitions.foreach(t => println("Transitions "+transitions.indexOf(t)+"  "+t.top(numStates).map(d => "%d %-5f".format(d.index,d.pr))))
        println
      }
    } 
    println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
  }

}

