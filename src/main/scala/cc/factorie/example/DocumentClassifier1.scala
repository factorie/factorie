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
  object DocumentDomain extends CategoricalTensorDomain[String];
  class Document(file:File) extends BinaryFeatureVectorVariable[String] {
    def domain = DocumentDomain
    val label = new Label(file.getParentFile.getName, this)
    cc.factorie.app.strings.alphaSegmenter(file).foreach(token => this += token)
  }
  object LabelDomain extends CategoricalDomain[String];
  class Label(labelString:String, val document:Document) extends LabelVariable(labelString) {
    def domain = LabelDomain
  }
 
  // The predefined model has factor templates for [Document,Label] and [Label] (the bias)
  val model = new classify.LogLinearModel[Label,Document](_.document, LabelDomain, DocumentDomain)

  def main(args:Array[String]): Unit = {
    if (args.length < 2) 
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var documents = new classify.LabelList[Label](_.document);
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (! directoryFile.exists) throw new IllegalArgumentException("Directory "+directory+" does not exist.")
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        documents += new Document(file).label
      }
    }
    
    val infogains = new classify.InfoGain(documents)
    println(infogains.top(20).mkString(" "))
    println()
    val plig = new classify.PerLabelInfoGain(documents)
    for (label <- LabelDomain) println(label.category+": "+plig.top(label, 20))
    println()
    val pllo = new classify.PerLabelLogOdds(documents)
    for (label <- LabelDomain) println(label.category+": "+pllo.top(label, 20))
    println()

    // Make a test/train split
    val (trainVariables, testVariables) = documents.shuffle.split(0.5)
    (trainVariables ++ testVariables).foreach(_.setRandomly())

    //println(model)
    //println(model.factors(trainVariables.head))

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
