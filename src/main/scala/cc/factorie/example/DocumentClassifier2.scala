/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
 This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
 http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
 This software is provided under the terms of the Eclipse Public License 1.0
 as published by http://www.opensource.org.  For further information,
 see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._

/** A raw document classifier without using any of the facilities of cc.factorie.application.DocumentClassification,
 and without using the entity-relationship language of cc.factorie.er.  By contrast, see example/DocumentClassifier1. */
object DocumentClassifier2 {
  
  class Document(file:File) extends BinaryVectorVariable[String] {
    var label = new Label(file.getParentFile.getName, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryVectorVariable
    "\\w+".r.findAllIn(Source.fromFile(file).mkString).foreach(regexMatch => this += regexMatch.toString)
  }
  class Label(name:String, val document:Document) extends LabelVariable(name) 

  val model = new Model(
    /** Bias term just on labels */
    new TemplateWithDotStatistics1[Label], 
    /** Factor between label and observed document */
    new TemplateWithDotStatistics2[Label,Document] {
      def unroll1 (label:Label) = Factor(label, label.document)
      def unroll2 (token:Document) = throw new Error("Document values shouldn't change")
    }
  )

  val objective = new Model(new TrueLabelTemplate[Label])

  def main(args: Array[String]) : Unit = {
    if (args.length < 2) 
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var documents = new ArrayBuffer[Document];
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (! directoryFile.exists) throw new IllegalArgumentException("Directory "+directory+" does not exist.")
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        documents += new Document(file)
      }
    }
    
    // Make a test/train split
    val (testSet, trainSet) = documents.shuffle.split(0.5)
    var trainVariables = trainSet.map(_ label)
    var testVariables = testSet.map(_ label)
    (trainVariables ++ testVariables).foreach(_.setRandomly)

    println(model)
    println(model.factors(trainVariables.first))

    // Train and test
    val learner = new VariableSettingsSampler[Label](model) with SampleRank with GradientAscentUpdates
    val predictor = new VariableSettingsSampler[Label](model)
    learner.learningRate = 1.0
    for (i <- 0 until 10) {
      learner.process (trainVariables, 1)
      learner.learningRate *= 0.9
      predictor.process (testVariables, 1)
      println ("Train accuracy = "+ Global.defaultObjective.aveScore(trainVariables))
      println ("Test  accuracy = "+ Global.defaultObjective.aveScore(testVariables))
    }

  }
}



