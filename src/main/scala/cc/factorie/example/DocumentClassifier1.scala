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
import scala.collection.mutable.ArrayBuffer
import cc.factorie._
import cc.factorie.app.classify

object DocumentClassifier1 {
  
 // Define variable classes
 class Document(file:File) extends classify.document.Document[Label,Document](file) {
   val label = new Label(file.getParentFile.getName, this)
 }
 class Label(labelString:String, document:Document) extends classify.Label[Document,Label](labelString, document)
 
 // The predefined model has factor templates for [Document,Label] and [Label] (the bias)
 val model = classify.newModel[Label,Document]

 def main(args:Array[String]): Unit = {
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
   (trainVariables ++ testVariables).foreach(_.setRandomly())

   println(model)
   println(model.factors(trainVariables.head))

   // Train and test
   val learner = new VariableSettingsSampler[Label](model) with SampleRank with GradientAscentUpdates
   val predictor = new VariableSettingsSampler[Label](model)
   learner.learningRate = 1.0
   for (i <- 0 until 10) {
     learner.processAll(trainVariables)
     learner.learningRate *= 0.9
     predictor.processAll(testVariables)
     println ("Train accuracy = "+ cc.factorie.defaultObjective.aveScore(trainVariables))
     println ("Test  accuracy = "+ cc.factorie.defaultObjective.aveScore(testVariables))
   }

 }
}
