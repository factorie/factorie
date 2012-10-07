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
import app.strings.{SetBasedStopwords, StringSet}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import java.io.File

/** Infrastructure for independent classification of feature vectors with String-valued features. 
    @author Andrew McCallum
@since 0.8
  */

/** Performs independent prediction of (iid) Labels (all of which must share the same domain).
    Has abstract method "labelDomain". */
trait Classifier[L <: MutableDiscreteVar[_]] {
  def labelDomain: DiscreteDomain // This is necessary for LabelEvaluation
  /** Return a record summarizing the outcome of applying this classifier to the given label.  Afterwards the label will have the same value it had before this call. */
  def classification(label: L): Classification[L]
  def classifications(labels: Iterable[L]): Seq[Classification[L]] = labels.toSeq.map(label => classification(label))
  /** Set the label to classifier-predicted value and return a Classification object summarizing the outcome. */
  def classify(label: L): Classification[L] = {
    val c = classification(label);
    c.globalize(null);
    c
  }
  def classify(labels: Iterable[L]): Seq[Classification[L]] = {
    val c = classifications(labels);
    c.foreach(_.globalize(null));
    c
  }
}

/** A classifier that uses a Model to score alternative label values. */
class ModelBasedClassifier[L <: MutableDiscreteVar[_]](val model: Model2[Variable], val labelDomain: DiscreteDomain) extends Classifier[L] {
  def classification(label: L): Classification[L] = {
    require(label.domain eq labelDomain)
    new Classification(label, this, label.proportions(model))
  }
}

/** An object that can train a Classifier given a LabelList. */
trait ClassifierTrainer {
  def train[L <: LabeledCategoricalVariable[_], F <: DiscreteTensorVar](il: LabelList[L, F]): Classifier[L]
}

/** An object that can gather  */
trait ClassifierEvaluator[L <: LabeledCategoricalVariable[_]] {
  def += (c: Classification[L]): Unit
  def toString: String
}

// A TUI for training, running and diagnosing classifiers
object Classifier {

  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val writeInstances = new CmdOption("write-instances", "instances", "FILE", "Filename in which to save the instances' labels and features.")

      val readInstances = new CmdOption("read-instances", "instances", "FILE", "Filename from which to read the instances' labels and features.")
      val readTrainingInstances = new CmdOption("read-training-instances", "instances", "FILE", "Filename from which to read the training instances' labels and features.")
      val readValidationInstances = new CmdOption("read-validation-instances", "instances", "FILE", "Filename from which to read the validation instances' labels and features.")
      val readTestingInstances = new CmdOption("read-testing-instances", "instances", "FILE", "Filename from which to read the testing instances' labels and features.")

      val readSVMLight = new CmdOption("read-svm-light", "instances", "FILE", "Filename from which to read the instances' labels and features in SVMLight format.")
      val readBinaryFeatures = new CmdOption("read-binary-features", false, "true|false", "If true, features will be binary.")

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

      val writeClassifier = new CmdOption("write-classifier", "classifier", "FILE", "Filename in which to save the classifier.")
      val readClassifier = new CmdOption("read-classifier", "classifier", "FILE", "Filename from which to read the classifier.")
      val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the instances that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
      val validationPortion = new CmdOption("validation-portion", 0.0, "FRACTION", "The fraction of the instances that should be used for validation")
      val localRandomSeed = new CmdOption("random-seed", -1, "N", "The random seed for randomly selecting a proportion of the instance list for training")

      val trainer = new CmdOption("trainer", "MaxEntTrainer", "Class()", "The constructor for a ClassifierTrainer class.")
      // TODO Consider enabling the system to use multiple ClassifierTrainers at the same time, and compare results
      val crossValidation = new CmdOption("cross-validation", 0, "N", "The number of folds for cross-validation (DEFAULT=0)")

      val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.")

      val printInfoGain = new CmdOption("print-infogain", false, "true|false", "Print the training features with highest information gain.")
    }
    opts.parse(args)

    // set local random seed
    if (opts.localRandomSeed.wasInvoked)
      setRandomSeed(opts.localRandomSeed.value)

    // Feature and Label classes
    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]
    class Label(labelName: String, val features: Features) extends LabeledCategoricalVariable(labelName) {
      def domain = LabelDomain
    }
    trait Features extends DiscreteTensorVar {
      this: CategoricalTensorVariable[String] =>
      def labelName: String
      def domain = FeaturesDomain
      var label = new Label(labelName, this)
    }
    class BinaryFeatures(val labelName: String) extends BinaryFeatureVectorVariable[String] with Features {}
    class NonBinaryFeatures(val labelName: String) extends FeatureVectorVariable[String] with Features {}

    def fileToString(f: File): String = scala.io.Source.fromFile(f, opts.readTextEncoding.value).mkString

    // Some global variables
    val segmenter = new cc.factorie.app.strings.RegexSegmenter(opts.readTextTokenRegex.value.r)
    val stoplist =
      if (opts.readTextStoplist.wasInvoked) {
        val stopListFile = new File(opts.readTextStoplist.value)
        if (!stopListFile.exists || !stopListFile.isFile)
          throw new IllegalArgumentException("Stoplist file " + opts.readTextStoplist.value + " does not exist.")
        val stopwords = new SetBasedStopwords
        val words = fileToString(stopListFile).split("\n").foreach(stopwords +=)
        stopwords
      } else {
        cc.factorie.app.strings.Stopwords
      }
    val labels = new LabelList[Label, Features](_.features)
    val trainingLabels = new LabelList[Label, Features](_.features)
    val validationLabels = new LabelList[Label, Features](_.features)
    val testingLabels = new LabelList[Label, Features](_.features)

    // Helper functions
    def textIntoFeatures(text: String, features: CategoricalTensorVariable[String]): Unit = {
      if (opts.readTextSkipHTML.value) throw new Error("Not yet implemented.")
      val text2 = if (opts.readTextSkipHeader.value) text.substring(text.indexOf("\n\n")) else text
      val text3 = if (opts.readTextPreserveCase.value) text2 else text2.toLowerCase
      for (word <- segmenter(text3)) {
        if (!stoplist.contains(word)) features += word
      }
    }

    // Read instances
    if (opts.readTextDirs.wasInvoked) {
      for (directory <- args) {
        val directoryFile = new File(directory)
        if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
          val features = if (opts.readBinaryFeatures.value) new BinaryFeatures(directory) else new NonBinaryFeatures(directory)
          // Use directory as label category
          textIntoFeatures(fileToString(file), features)
          labels += features.label
        }
      }
    }

    val (testSet, trainSet) = labels.shuffle.split(opts.trainingPortion.value)
    trainingLabels ++= trainSet
    testingLabels ++= testSet

    val classifierTrainer = opts.trainer.value match {
      case "MaxEntTrainer" => new MaxEntTrainer()
      case "MaxEntLikelihoodTrainer" => new MaxEntLikelihoodTrainer()
      case "MaxEntSampleRankTrainer" => new MaxEntSampleRankTrainer()
      case "NaiveBayesTrainer" => new NaiveBayesTrainer()
      case e => throw new IllegalArgumentException("Unknown ClassifierTrainer: " + e)
    }

    val start = System.currentTimeMillis

    val classifier = classifierTrainer.train(trainingLabels)

    val testTrial = new classify.Trial[Label](classifier)
    testTrial ++= testingLabels

    val trainTrial = new classify.Trial[Label](classifier)
    trainTrial ++= trainingLabels

    println("Train accuracy = " + trainTrial.accuracy)
    println("Test  accuracy = " + testTrial.accuracy)
    println("Number of ms to train/test: " + (System.currentTimeMillis - start))
  }
}