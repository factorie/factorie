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

object ChainNER1 {
  
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
    val trainLabels = trainSentences.flatMap(_.labels)
    val testLabels = testSentences.flatMap(_.labels)
    (trainLabels ++ testLabels).foreach(_.setRandomly()) 

    // Train for 5 iterations
    val learner = new VariableSettingsSampler[Label](model) with SampleRank with GradientAscentUpdates 
    learner.processAll(trainLabels, 5) // Train for 5 iterations through all Labels

    // Predict, also by sampling, visiting each variable 3 times.
    val predictor = new VariableSettingsSampler[Label](model)
    predictor.processAll(testLabels, 3)
    
    // Evaluate
    println("TRAIN "+LabeledTokenSeq.labelEvaluation[Token,Label](trainLabels).accuracy)
    println("TEST  "+LabeledTokenSeq.labelEvaluation[Token,Label](testLabels).accuracy)
  }

}


