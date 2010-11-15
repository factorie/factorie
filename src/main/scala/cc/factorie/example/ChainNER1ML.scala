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

import java.io.File
import cc.factorie._
import cc.factorie.er._
import cc.factorie.app.tokenseq.labeled
import collection.mutable.ArrayBuffer

object ChainNER1ML {

  // Define the variable classes
  class Token(word: String, labelString: String) extends labeled.Token[Sentence, Label, Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(tag: String, token: Token) extends labeled.Label[Sentence, Token, Label](tag, token)
  class Sentence extends labeled.TokenSeq[Token, Label, Sentence]

  // Define the model:
  val model = new Model(
    new TemplateWithDotStatistics1[Label] {
      override def unroll1(label: Label) = if (!label.hasPrev) Factor(label) else Nil
    },
    new TemplateWithDotStatistics1[Label] {
      override def unroll1(label: Label) = if (!label.hasNext) Factor(label) else Nil
    },
    Foreach[Label] {label => Score(label, label.token)},
    Foreach[Label] {label => Score(label.prev, label, label.token)}
    )

  def main(args: Array[String]): Unit = {

    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")

    def featureFunction(inFeatures: Seq[String]): Seq[String] = {
      val result = new ArrayBuffer[String]
      // Assume the first feature is the word
      result += "DEFAULT"
      result ++= inFeatures
      result
    }

    // Read training and testing data.
    val trainSentences = labeled.TokenSeq.fromOWPL(new File(args(0)), () => new Sentence, (word, lab) => new Token(word, lab), featureFunction _)
    val testSentences = labeled.TokenSeq.fromOWPL(new File(args(1)), () => new Sentence, (word, lab) => new Token(word, lab), featureFunction _)

    // Get the variables to be inferred
    val trainVariables = trainSentences.map(_.labels)
    val testVariables = testSentences.map(_.labels)
    val allTestVariables = testVariables.flatMap(l => l)
    val allTokens: Seq[Token] = (trainSentences ++ testSentences).flatten
    allTokens.foreach(_.freeze) // To enable Template.cachedStatistics

    // Train and test
    println("*** Starting training (#sentences=%d)".format(trainSentences.size))
    val start = System.currentTimeMillis
    val trainer = new LogLinearMaximumLikelihood(model)
    trainer.processAll(trainVariables, 1) // Do just one iteration for initial timing
    println("One iteration took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
    //System.exit(0)

    trainer.processAll(trainVariables) // Keep training to convergence

    val objective = new Model(new Label01LossTemplate[Label])
    // slightly more memory efficient - kedarb
    println("*** Starting inference (#sentences=%d)".format(testSentences.size))
    testVariables.foreach {
      variables => new BPInferencer(model).inferTreewiseMax(variables)
    }
    println("test token accuracy=" + objective.aveScore(allTestVariables))

    println("Total training took " + (System.currentTimeMillis - start) / 1000.0 + " seconds")
  }
}
