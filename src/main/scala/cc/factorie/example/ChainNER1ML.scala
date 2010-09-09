/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.er._
import cc.factorie.application.LabeledTokenSeqs
import cc.factorie.application.LabeledTokenSeqs.LabeledTokenSeq

object ChainNER1ML {

  // Define the variable classes
  class Token(word:String, labelString:String) extends LabeledTokenSeqs.Token[Label,Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(tag:String, token:Token) extends LabeledTokenSeqs.Label[Token,Label](tag, token)

  // Define the model:
  val model = new Model(
    Foreach[Label] { label => Score(label) },
    Foreach[Label] { label => Score(label.prev, label, label.token) }
  )

  def main(args: Array[String]) : Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")

    // Read training and testing data.
    val trainSentences = LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(new File(args(0))), (word,lab)=>new Token(word,lab), "-DOCSTART-")
    val testSentences =  LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(new File(args(1))), (word,lab)=>new Token(word,lab), "-DOCSTART-")

    // Get the variables to be inferred
    val trainVariables = trainSentences.map(_.labels)
    val testVariables = testSentences.flatMap(_.labels)
    
        // Train and test
    val trainer = new LogLinearMaximumLikelihood(model)
    trainer.process(trainVariables)

    val objective = new Model(new LabelTemplate[Label])


    val lattice = new BPLattice(testVariables, model)
    lattice.updateTreewiseMax()
    lattice.setVariablesToMax

    println("test token accuracy=" + objective.aveScore(testVariables))
  }
}