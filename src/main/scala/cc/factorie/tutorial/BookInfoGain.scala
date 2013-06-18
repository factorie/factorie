package cc.factorie.example
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import app.classify
import classify._
import la.Tensor

object BookInfoGain {
  object DocumentDomain extends CategoricalTensorDomain[String]
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
    var docLabels = new classify.LabelList[Label, Document](_.document)
    for (filename <- args) {
      val bookFile = new File(filename)
      if (!bookFile.exists) throw new IllegalArgumentException("Directory " + filename + " does not exist.")
      "\\w+".r.findAllIn(Source.fromFile(bookFile).mkString).toSeq.grouped(500).foreach(words => docLabels += new Document(bookFile.getName, words.filter(!cc.factorie.app.strings.Stopwords.contains(_))).label)
    }

    val infogains = new classify.InfoGain(docLabels)
    println(infogains.top(20).mkString(" "))
    println()
//    val plig = new classify.PerLabelInfoGain(docLabels)
//    for (label <- LabelDomain) println(label.category+": "+plig.top(label, 20))
//    println()
    val pllo = new classify.PerLabelLogOdds(docLabels)
    for (label <- LabelDomain) println(label.category+": "+pllo.top(label, 40))
    println()

    // Make a test/train split
    val (testSet, trainSet) = docLabels.shuffle.split(0.5)
    val trainLabels = new classify.LabelList[Label, Document](trainSet, _.document)
    val testLabels = new classify.LabelList[Label, Document](testSet, _.document)

    val classifier = new MaxEntTrainer().train(trainLabels)
    val testTrial = new classify.Trial[Label](classifier)
    testTrial ++= testLabels

    val trainTrial = new classify.Trial[Label](classifier)
    trainTrial ++= trainLabels

    println("Train accuracy = " + trainTrial.accuracy)
    println("Test  accuracy = " + testTrial.accuracy)
  }
}