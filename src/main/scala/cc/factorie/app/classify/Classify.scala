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

package cc.factorie.app.classify

import cc.factorie._
import app.classify
import app.strings.SetBasedStopwords
import java.io._
import cc.factorie.util.{ScriptingUtils, BinarySerializer}
import scala.language.postfixOps
import scala.collection.mutable.ArrayBuffer
import cc.factorie.la.Tensor1
import scala.Some
import cc.factorie.variable._
import scala.Some

// Feature and Label classes

class Label(val labelName: String, val features: Features, val domain: CategoricalDomain[String]) extends LabeledCategoricalVariable(labelName) {
  override def toString = "instance=%s label=%s" format (features.instanceName, categoryValue)
  override val skipNonCategories = true
}
trait Features extends VectorVar {
  this: CategoricalVectorVariable[String] =>
  def labelName: String
  def instanceName: String
  def domain: CategoricalVectorDomain[String]
  def labelDomain: CategoricalDomain[String]
  var label = new Label(labelName, this, labelDomain)
}
class BinaryFeatures(val labelName: String, val instanceName: String, val domain: CategoricalVectorDomain[String], val labelDomain: CategoricalDomain[String])
  extends BinaryFeatureVectorVariable[String] with Features { override val skipNonCategories = true }
class NonBinaryFeatures(val labelName: String, val instanceName: String, val domain: CategoricalVectorDomain[String], val labelDomain: CategoricalDomain[String])
  extends FeatureVectorVariable[String] with Features { override val skipNonCategories = true }

// A TUI for training, running and diagnosing classifiers
object Classify {

  /* Sample Usages:
  *
  *   - Train a classifier, write to disk:
  *
  *     --write-vocabulary
  *     C:\classifier-vocab
  *     --write-classifier
  *     C:\classifytuiclassifier
  *     --training-portion
  *     1.0
  *     --read-text-encoding
  *     ISO-8859-1
  *     --read-text-dirs
  *     "c:\first-directory c:\second-directory ..."
  *
  *   - Use that classifier to classify some new data:
  *
  *     --read-vocabulary
  *     C:\classifytuivocab
  *     --read-classifier
  *     C:\classifytuiclassifier
  *     --write-classifications
  *     c:\classificationresults.txt
  *     --read-text-encoding
  *     ISO-8859-1
  *     --read-text-dirs
  *     "c:\first-directory c:\second-directory ..."
  *
  * */

  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val writeInstances = new CmdOption("write-instances", "instances", "FILE", "Filename in which to save the instances' labels and features.")

      val readInstances = new CmdOption("read-instances", "instances", "FILE", "Filename from which to read the instances' labels and features.")
      val readTrainingInstances = new CmdOption("read-training-instances", "instances", "FILE", "Filename from which to read the training instances' labels and features.")
      val readValidationInstances = new CmdOption("read-validation-instances", "instances", "FILE", "Filename from which to read the validation instances' labels and features.")
      val readTestingInstances = new CmdOption("read-testing-instances", "instances", "FILE", "Filename from which to read the testing instances' labels and features.")

      val readSVMLight = new CmdOption("read-svm-light", "instances", "FILE", "Filename from which to read the instances' labels and features in SVMLight format.")
      val readBinaryFeatures = new CmdOption("read-binary-features", true, "true|false", "If true, features will be binary as opposed to counts.  Default is true.")

      val readTextDirs = new CmdOption("read-text-dirs", "textdir", "DIR...", "Directories from which to read text documents; tokens become features; directory name is the label.")
      val readTextLines = new CmdOption("read-text-lines", "textfile", "FILE.txt", "Filename from which to read the instances' labels and features; first word is the label value")
      val readTextTokenRegex = new CmdOption("read-text-token-regex", "\\p{Alpha}+", "REGEX", "Regular expression defining extent of text tokens.")
      val readTextSkipHeader = new CmdOption("read-text-skip-header", false, "true|false", "Skip text up until double newline.")
      val readTextSkipHTML = new CmdOption("read-text-skip-html", false, "true|false", "Exclude HTML tags.")
      val readTextEncoding = new CmdOption("read-text-encoding", "UTF-8", "ENCODING", "The name of the encoding to use, e.g. UTF-8.")
      val readTextPreserveCase = new CmdOption("read-text-preserve-case", false, "true|false", "Do not downcase text.")
      val readTextStoplist = new CmdOption("read-text-stoplist", "stoplist.txt", "FILE", "File from which to read all stopwords, one per line.")
      val readTextExtraStopwords = new CmdOption("read-text-extra-stopwords", "stopwords.txt", "FILE", "File from which to read stopwords to add to standard list, one per line.")
      val readTextUseStoplist = new CmdOption("read-text-use-stoplist", "stoplist.txt", "FILE", "Remove words on the stoplist.")
      val readTextGramSizes = new CmdOption("read-text-gram-sizes", List(1), "1,2", "Include among the features all n-grams of sizes specified.  For example, to get all unigrams and bigrams, use --gram-sizes 1,2.  This option occurs after the removal of stop words, if removed.")

      val writeVocabulary = new CmdOption("write-vocabulary", "vocabulary", "FILE", "Filename in which to save the vocabulary.")
      val readVocabulary = new CmdOption("read-vocabulary", "vocabulary", "FILE", "Filename from which to load the vocabulary.")

      val writeClassifications = new CmdOption("write-classifications", "classifications", "FILE", "Filename in which to save the classifications.")

      val writeClassifier = new CmdOption("write-classifier", "classifier", "FILE", "Filename in which to save the classifier.")
      val readClassifier = new CmdOption("read-classifier", "classifier", "FILE", "Filename from which to read the classifier.")
      val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the instances that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
      val validationPortion = new CmdOption("validation-portion", 0.0, "FRACTION", "The fraction of the instances that should be used for validation")
      val localRandomSeed = new CmdOption("random-seed", 0, "N", "The random seed for randomly selecting a proportion of the instance list for training")

      val trainer = new CmdOption("trainer", "new SVMMultiClassTrainer", "code", "Scala code to construct a ClassifierTrainer class.")
      // TODO Consider enabling the system to use multiple ClassifierTrainers at the same time, and compare results
      val crossValidation = new CmdOption("cross-validation", 0, "N", "The number of folds for cross-validation (DEFAULT=0)")

      val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.")

      val printInfoGain = new CmdOption("print-infogain", false, "true|false", "Print the training features with highest information gain.")
    }
    opts.parse(args)

    object FeaturesDomain extends CategoricalVectorDomain[String]
    object LabelDomain extends CategoricalDomain[String]

    // set local random seed
    implicit val random = new scala.util.Random(opts.localRandomSeed.value)

    def fileToString(f: File): String = {
      if (!f.exists || !f.isFile) throw new IllegalArgumentException("File " + f.getName + " does not exist.")
      scala.io.Source.fromFile(f, opts.readTextEncoding.value).mkString
    }

    def readStoplist(fileName: String): SetBasedStopwords = {
      val stopListFile = new File(fileName)
      val stopwords = new SetBasedStopwords
      fileToString(stopListFile).split("\n").foreach(stopwords +=)
      stopwords
    }

    // Some global variables
    val segmenter = new cc.factorie.app.strings.RegexSegmenter(opts.readTextTokenRegex.value.r)
    val stoplist =
      if (opts.readTextStoplist.wasInvoked) {
        readStoplist(opts.readTextStoplist.value)
      } else if (opts.readTextExtraStopwords.wasInvoked) {
        val stopWords = readStoplist(opts.readTextExtraStopwords.value)
        cc.factorie.app.strings.Stopwords.asArray.foreach(stopWords +=)
        stopWords
      } else {
        cc.factorie.app.strings.Stopwords
      }
    val labels = new ArrayBuffer[Label]()
    val trainingLabels = new ArrayBuffer[Label]()
    val validationLabels = new ArrayBuffer[Label]()
    val testingLabels = new ArrayBuffer[Label]()

    // Helper functions
    def textIntoFeatures(text: String, features: CategoricalVectorVariable[String]): Unit = {
      if (opts.readTextSkipHTML.value) throw new Error("Not yet implemented.")
      val text2 = if (opts.readTextSkipHeader.value) Some(text.indexOf("\n\n"))
          .filter(-1 !=).orElse(Some(text.indexOf("\r\n\r\n")))
          .filter(-1 !=).map(text.substring(_)).getOrElse(text)
        else text
      val text3 = if (opts.readTextPreserveCase.value) text2 else text2.toLowerCase
      for (gramSize <- opts.readTextGramSizes.value)
        for (words <- segmenter(text3).filterNot(stoplist.contains(_)).sliding(gramSize))
          features += words.mkString(",")
    }

    def readInstancesFromFileSVMLight(fileName: String): ArrayBuffer[Label] = {
      val textFile = new File(fileName)
      val text = fileToString(textFile)
      Serialize.readInstancesSVMLight(text, FeaturesDomain, LabelDomain)
    }

    def readInstancesFromFile(fileName: String): ArrayBuffer[Label] = {
      val cubbie = new LabelListCubbie(FeaturesDomain, LabelDomain, opts.readBinaryFeatures.value)
      val file = new File(fileName)
      BinarySerializer.deserialize(cubbie, file)
      cubbie.fetch()
    }

    def writeInstances(ll: ArrayBuffer[Label], file: File): Unit = {
      val cubbie = new LabelListCubbie(FeaturesDomain, LabelDomain, opts.readBinaryFeatures.value)
      cubbie.store(ll)
      BinarySerializer.serialize(cubbie, file)
    }

    // Read vocabulary
    if (opts.readVocabulary.wasInvoked) {
      val vocabFile = new File(opts.readVocabulary.value)
      // TODO should we make an overload that doesnt req. a cubbie and reads the cubbie type from file? -luke
      // or automatically picks the cubbie based on the input type?
      BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, LabelDomain, vocabFile)
      FeaturesDomain.freeze() // TODO should this always be frozen? otherwise weightsSet won't match... -luke
      LabelDomain.freeze()
    }

    // Read instances
    if (opts.readTextDirs.wasInvoked) {
      var numDocs = 0; var numDirs = 0
      for (directory <- opts.readTextDirs.value.split("\\s+")) {
        val directoryFile = new File(directory)
        if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
        for (file <- new File(directory).listFiles; if file.isFile) {
          //println ("Directory " + directory + " File " + file + " documents.size " + documents.size)
          val labelName = directoryFile.getName
          val instanceName = labelName + "-" + file.getName
          val features =
            if (opts.readBinaryFeatures.value) new BinaryFeatures(labelName, instanceName, FeaturesDomain, LabelDomain)
            else new NonBinaryFeatures(labelName, instanceName, FeaturesDomain, LabelDomain)
          // Use directory as label category
          textIntoFeatures(fileToString(file), features)
          labels += features.label
          numDocs += 1
        }
        numDirs += 1
      }
      println("Read %d documents in %d directories." format (numDocs, numDirs))
    } else if (opts.readTextLines.wasInvoked) {
      sys.error("Reading text line instances is unimplemented")
    } else if (opts.readSVMLight.wasInvoked) {
      labels ++= readInstancesFromFileSVMLight(opts.readSVMLight.value)
    } else if (opts.readInstances.wasInvoked) {
      labels ++= readInstancesFromFile(opts.readInstances.value)
    }

    println("# of distinct class labels: " + LabelDomain.size)
    println("Class labels: " + LabelDomain._elements.mkString(", "))
    println("Vocabulary size: " + FeaturesDomain.dimensionDomain.size)

    // Write vocabulary
    if (opts.writeVocabulary.wasInvoked) {
      val vocabFile = new File(opts.writeVocabulary.value)
      BinarySerializer.serialize(FeaturesDomain.dimensionDomain, LabelDomain, vocabFile)
    }

    // if readclassifier is set, then we ignore instances labels and classify them
    if (opts.readClassifier.wasInvoked) {
      val classifierFile = new File(opts.readClassifier.value)
      val cubbie = new LinearMultiClassClassifierCubbie
      BinarySerializer.deserialize(cubbie, classifierFile)
      val classifier = cubbie.fetch
      val classifications = labels.map(l => classifier.classification(l.features.value))
      for (cl <- labels) println(cl)
      if (opts.writeInstances.wasInvoked) {
        val instancesFile = new File(opts.writeInstances.value)
        instancesFile.createNewFile()
        writeInstances(labels, instancesFile)
      }
      return
    }

    // see if they used the specific instance reading options
    if (labels.length == 0) {
      if (opts.readTrainingInstances.wasInvoked) trainingLabels ++= readInstancesFromFile(opts.readTrainingInstances.value)
      if (opts.readValidationInstances.wasInvoked) validationLabels ++= readInstancesFromFile(opts.readValidationInstances.value)
      if (opts.readTestingInstances.wasInvoked) testingLabels ++= readInstancesFromFile(opts.readTestingInstances.value)
    } else {
      val (trainSet, testAndValidationSet) =
        if (opts.trainingPortion == 1.0) (labels, Seq(): Seq[Label]) else labels.shuffle.split(opts.trainingPortion.value)
      val (valSet, testSet) =
        if (opts.validationPortion.value == 0.0) (Seq(): Seq[Label], testAndValidationSet)
        else testAndValidationSet.split(opts.validationPortion.value / (1 - opts.trainingPortion.value))
      trainingLabels ++= trainSet
      testingLabels ++= testSet
      validationLabels ++= valSet
    }

    if (opts.printInfoGain.wasInvoked) {
      println("Top 20 features with highest information gain: ")
      new InfoGain(trainingLabels, (l: Label) => l.features).top(20).foreach(println(_))
    }

    if (opts.writeInstances.wasInvoked) {
      val instancesFile = new File(opts.writeInstances.value)
      instancesFile.createNewFile()
      val bigll = new ArrayBuffer[Label]()
      bigll ++= (trainingLabels ++ testingLabels ++ validationLabels)
      writeInstances(bigll, instancesFile)
    }

    val classifierTrainer = ScriptingUtils.eval[MultiClassTrainerBase[MultiClassClassifier[Tensor1]]](
      "{ implicit val rng = new scala.util.Random(0); " + opts.trainer.value + "}", Seq("cc.factorie.app.classify._", "cc.factorie.optimize._"))

    val start = System.currentTimeMillis

    val classifier = classifierTrainer.train(trainingLabels, trainingLabels.map(_.features))

    println("Classifier trained in " + ((System.currentTimeMillis - start) / 1000.0) + " seconds.")

    if (opts.writeClassifier.wasInvoked) {
      val classifierFile = new File(opts.writeClassifier.value)
      classifier match {
        case model: LinearMultiClassClassifier =>
          val mc = new LinearMultiClassClassifierCubbie
          mc.store(model)
          BinarySerializer.serialize(mc, classifierFile)
      }
    }

    opts.evaluator.value match {
      case "Trial" =>
        if (trainingLabels.length > 0) {
          val trainTrial = new classify.Trial[Label, Tensor1](classifier, trainingLabels.head.domain, _.features.value)
          trainTrial ++= trainingLabels
          println("== Training Evaluation ==")
          println(trainTrial.toString())
        }
        if (testingLabels.length > 0) {
          val testTrial = new classify.Trial[Label, Tensor1](classifier, trainingLabels.head.domain, _.features.value)
          testTrial ++= testingLabels
          println("== Testing Evaluation ==")
          println(testTrial.toString())
        }
        if (validationLabels.length > 0) {
          val validationTrial = new classify.Trial[Label, Tensor1](classifier, trainingLabels.head.domain, _.features.value)
          validationTrial ++= validationLabels
          println("== Validation Evaluation ==")
          println(validationTrial.toString())
        }
    }
  }
}
