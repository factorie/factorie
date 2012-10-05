package cc.factorie.app.classify

import cc.factorie._
import app.classify
import app.classify._
import app.strings.SetBasedStopwords
import java.io.{PrintStream, PrintWriter, File}

// Feature and Label classes

class Label(labelName: String, val features: Features, val domain: CategoricalDomain[String]) extends LabeledCategoricalVariable(labelName)
trait Features extends DiscreteTensorVar {
  this: CategoricalTensorVariable[String] =>
  def labelName: String
  def domain: CategoricalTensorDomain[String]
  def labelDomain: CategoricalDomain[String]
  var label = new Label(labelName, this, labelDomain)
}
class BinaryFeatures(val labelName: String, val domain: CategoricalTensorDomain[String], val labelDomain: CategoricalDomain[String])
  extends BinaryFeatureVectorVariable[String] with Features {}
class NonBinaryFeatures(val labelName: String, val domain: CategoricalTensorDomain[String], val labelDomain: CategoricalDomain[String])
  extends FeatureVectorVariable[String] with Features {}


// A TUI for training, running and diagnosing classifiers
object ClassifyTUI {

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

    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]

    // set local random seed
    if (opts.localRandomSeed.wasInvoked)
      setRandomSeed(opts.localRandomSeed.value)

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
    val labels = new LabelList[Label, Features](_.features)
    val trainingLabels = new LabelList[Label, Features](_.features)
    val validationLabels = new LabelList[Label, Features](_.features)
    val testingLabels = new LabelList[Label, Features](_.features)

    // Helper functions
    def textIntoFeatures(text: String, features: CategoricalTensorVariable[String]): Unit = {
      if (opts.readTextSkipHTML.value) throw new Error("Not yet implemented.")
      val text2 = if (opts.readTextSkipHeader.value) text.substring(text.indexOf("\n\n")) else text
      val text3 = if (opts.readTextPreserveCase.value) text2 else text2.toLowerCase
      for (word <- segmenter(text3))
        if (!stoplist.contains(word)) features += word
    }

    def readInstancesFromFile(fileName: String): LabelList[Label, Features] = {
      val textFile = new File(fileName)
      val text = fileToString(textFile)
      Deserialize.readInstancesSVMLight(text)
    }

    // Read instances
    if (opts.readTextDirs.wasInvoked) {
      for (directory <- opts.readTextDirs.value.split("\\s+")) {
        val directoryFile = new File(directory)
        if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
          val features =
            if (opts.readBinaryFeatures.value) new BinaryFeatures(directory, FeaturesDomain, LabelDomain)
            else new NonBinaryFeatures(directory, FeaturesDomain, LabelDomain)
          // Use directory as label category
          textIntoFeatures(fileToString(file), features)
          labels += features.label
        }
      }
    } else if (opts.readTextLines.wasInvoked) {
      labels ++= readInstancesFromFile(opts.readTextLines.value)
    } else if (opts.readSVMLight.wasInvoked) {
      labels ++= readInstancesFromFile(opts.readSVMLight.value)
    } else if (opts.readInstances.wasInvoked) {
      labels ++= readInstancesFromFile(opts.readInstances.value)
    }

    // if readclassifier is set, then we ignore instances labels and classify them
    if (opts.readClassifier.wasInvoked) {
      val classifier = Deserialize.readClassifier(fileToString(new File(opts.readClassifier.value)))
      classifier.classify(labels)
      if (opts.writeInstances.wasInvoked) {
        val instancesFile = new File(opts.writeInstances.value)
        instancesFile.createNewFile()
        Serialize.writeInstancesSVMLight(labels, new PrintStream(instancesFile))
      }
      return
    }

    // see if they used the specific instance reading options
    if (labels.length == 0) {
      if (opts.readTrainingInstances.wasInvoked) trainingLabels ++= readInstancesFromFile(opts.readTrainingInstances.value)
      if (opts.readValidationInstances.wasInvoked) validationLabels ++= readInstancesFromFile(opts.readValidationInstances.value)
      if (opts.readTestingInstances.wasInvoked) testingLabels ++= readInstancesFromFile(opts.readTestingInstances.value)
    } else {
      val (trainSet, testAndValidationSet) = labels.shuffle.split(opts.trainingPortion.value)
      val (valSet, testSet) = testAndValidationSet.split(opts.validationPortion.value)
      trainingLabels ++= trainSet
      testingLabels ++= testSet
      validationLabels ++= valSet
    }

    if (opts.writeInstances.wasInvoked) {
      val instancesFile = new File(opts.writeInstances.value)
      instancesFile.createNewFile()
      Serialize.writeInstancesSVMLight(trainingLabels ++ testingLabels ++ validationLabels, new PrintStream(instancesFile))
    }

    val classifierTrainer = opts.trainer.value match {
      case "MaxEntTrainer" => new MaxEntTrainer()
      case "MaxEntLikelihoodTrainer" => new MaxEntLikelihoodTrainer()
      case "MaxEntSampleRankTrainer" => new MaxEntSampleRankTrainer()
      case "NaiveBayesTrainer" => new NaiveBayesTrainer()
      case e => throw new IllegalArgumentException("Unknown ClassifierTrainer: " + e)
    }

    val start = System.currentTimeMillis

    val classifier = classifierTrainer.train(trainingLabels)

    if (opts.writeClassifier.wasInvoked) {
      val trainerFile = new File(opts.writeClassifier.value)
      trainerFile.createNewFile()
      Serialize.writeClassifier(classifier, FeaturesDomain, new PrintStream(trainerFile))
    }

    val testTrial = new classify.Trial[Label](classifier)
    testTrial ++= testingLabels

    val trainTrial = new classify.Trial[Label](classifier)
    trainTrial ++= trainingLabels

    println("Train accuracy = " + trainTrial.accuracy)
    println("Test  accuracy = " + testTrial.accuracy)
    println("Number of ms to train/test: " + (System.currentTimeMillis - start))
  }
}
