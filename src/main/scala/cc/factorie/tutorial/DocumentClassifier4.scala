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


package cc.factorie.tutorial

import scala.io.Source
import java.io.File
import cc.factorie._
import app.classify
import cc.factorie.optimize._
import language.postfixOps
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.app.classify.BoostingMultiClassTrainer

/**A document classifier that uses Decision Trees.
    Note that it also does not use any of the facilities of cc.factorie.app.classify.document */
object DocumentClassifier4 {

  object DocumentDomain extends CategoricalVectorDomain[String]
  class Document(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = DocumentDomain
    var label = new Label(file.getParentFile.getName, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryFeatureVectorVariable
    var str = Source.fromFile(file, "ISO-8859-1").mkString
    val headerIndex = str.indexOf("\n\n")
    if (headerIndex > 0) str = str.substring(headerIndex)
    "\\w+".r.findAllIn(str).foreach(regexMatch => this += regexMatch.toString)
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(name: String, val document: Document) extends LabeledCategoricalVariable(name) {
    def domain = LabelDomain
  }

  var useBoostedClassifier = false

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    if (args.length < 2)
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var docLabels = new ArrayBuffer[Label]()
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
      for (file <- new File(directory).listFiles; if file.isFile) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        docLabels += new Document(file).label
      }
    }
    // println("Read "+docLabels.size+" documents, having vocabulary size "+DocumentDomain.dimensionSize)

    // Make a test/train split
    val (testSet, trainSet) = docLabels.shuffle.split(0.5)

    // Train and test the decision tree
//    val trainer = new RandomForestMultiClassTrainer(numTrees = 100, numFeaturesToUse = 10000, numInstancesToSample = 500, treeTrainer = new C45DecisionTreeTrainer { maxDepth = 25 })
    val trainer = new BoostingMultiClassTrainer(numWeakLearners = 1000)

//    val trainer = new DecisionTreeMultiClassTrainer(new C45DecisionTreeTrainer { maxDepth = 1500 })
    val start = System.currentTimeMillis()
    trainer.train[Label](trainSet, (_: Label).document, testSet, (_: Label) => 1.0)
    println(f"Elapsed time: ${System.currentTimeMillis() - start}%dms")
  }
}



