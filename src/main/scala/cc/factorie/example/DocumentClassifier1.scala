package cc.factorie.example
import java.io.File
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.Implicits._

object DocumentClassifier1 {
  import cc.factorie.application.DocumentClassification
  import cc.factorie.application.FeatureVectorClassification
  
  // Define variable classes
  class Document(file:File) extends DocumentClassification.Document[Label,Document](file) {
    //override type GetterClass = DocumentGetter
    def newLabel(lab:String) = new Label(lab, this)
  }
  class Label(labelString:String, document:Document) extends DocumentClassification.Label[Document,Label](labelString, document)
  
  class DocumentGetter extends FeatureVectorClassification.InstanceGetter[Label,Document] {
    override def newLabelGetter = new LabelGetter
  }
  
  class LabelGetter extends FeatureVectorClassification.LabelGetter[Document,Label] {
    override def newInstanceGetter = new DocumentGetter
    def labelIsCapitalized = getOneWay(label => Bool(java.lang.Character.isUpperCase(label.value(0))))
  }

  // The predefined model has factor templates for [Document,Label] and [Label] (the bias)
  val model = DocumentClassification.newModel

  def main(args:Array[String]): Unit = {
    if (args.length < 2) 
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var documents = new ArrayBuffer[Document];
    for (directory <- args) {
    	val directoryFile = new File(directory)
    	if (! directoryFile.exists) throw new IllegalArgumentException("Directory "+directory+" does not exist.")
    	for (file <- new File(directory).listFiles; if (file.isFile)) {
    		println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
    		documents += new Document(file)
    	}
    }

    // Make a test/train split
    val (testSet, trainSet) = documents.shuffle.split(0.5)
    var trainVariables = trainSet.map(_ label)
    var testVariables = testSet.map(_ label)
    (trainVariables ++ testVariables).foreach(_.setRandomly)

    // Train and test
    val learner = new GibbsSampler1[Label](model) with SampleRank with PerceptronUpdates
    val predictor = new GibbsSampler(model)
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
