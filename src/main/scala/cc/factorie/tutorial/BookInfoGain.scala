/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.app.classify.backend.OnlineLinearMulticlassTrainer

/** Demonstration of calculating class-word information gain where data coming from book-length */
object BookInfoGain {
  object DocumentDomain extends CategoricalVectorDomain[String]
  class Document(labelString: String, words:Seq[String]) extends BinaryFeatureVectorVariable[String](words) {
    def domain = DocumentDomain
    var label = new Label(labelString, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryFeatureVectorVariable
    //"\\w+".r.findAllIn(Source.fromFile(file, "ISO-8859-1").mkString).foreach(regexMatch => this += regexMatch.toString)
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
    var docLabels = new collection.mutable.ArrayBuffer[Label]
    for (filename <- args) {
      val bookFile = new File(filename)
      if (!bookFile.exists) throw new IllegalArgumentException("Directory " + filename + " does not exist.")
      "\\w+".r.findAllIn(Source.fromFile(bookFile).mkString).toSeq.grouped(500).foreach(words => docLabels += new Document(bookFile.getName, words.filter(!cc.factorie.app.strings.Stopwords.contains(_))).label)
    }

    val infogains = new classify.InfoGain(docLabels, (l: Label) => l.document)
    println(infogains.top(20).mkString(" "))
    println()
//    val plig = new classify.PerLabelInfoGain(docLabels)
//    for (label <- LabelDomain) println(label.category+": "+plig.top(label, 20))
//    println()
    val pllo = new classify.PerLabelLogOdds(docLabels, (l: Label) => l.document)
    for (label <- LabelDomain) println(label.category+": "+pllo.top(label, 40))
    println()

    // Make a test/train split
    val (testSet, trainSet) = docLabels.shuffle.split(0.5)
    val trainLabels = new collection.mutable.ArrayBuffer[Label] ++= trainSet
    val testLabels = new collection.mutable.ArrayBuffer[Label] ++= testSet

    val trainer = new OnlineLinearMulticlassTrainer()
    val classifier = trainer.train(trainLabels, trainLabels.map(_.document))
    val testTrial = new classify.Trial[Label,Document#Value](classifier, trainLabels.head.domain, _.document.value)
    testTrial ++= testLabels

    val trainTrial = new classify.Trial[Label,Document#Value](classifier, trainLabels.head.domain, _.document.value)
    trainTrial ++= trainLabels

    println("Train accuracy = " + trainTrial.accuracy)
    println("Test  accuracy = " + testTrial.accuracy)
  }
}