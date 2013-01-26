package cc.factorie.app.nlp.parse.nonproj

import ParserSupport._
import cc.factorie.app.classify.ModelBasedClassifier
import java.io.File
import java.io.PrintWriter
import cc.factorie.BinaryFileSerializer
import scala.io.Source
import cc.factorie.BinaryCubbieFileSerializer
import cc.factorie.CategoricalDomainCubbie

// An abstraction which allows for easily changing the predictor
trait ParserClassifier {
  def classify(v: ParseDecisionVariable): ParseDecision
}

// The standard SVM one-vs-all classifier
// The interface here is simpler than it seems: we're only saving, loading, and classifying.
// To clean this up, we need better serialization support.
class BaseParserClassifier(val backingClassifier: ModelBasedClassifier[ParseDecisionVariable]) extends ParserClassifier {
  
  private var _gzip = false
  
  private def saveModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    BinaryFileSerializer.serialize(backingClassifier.model, file, gzip = _gzip)
  }
  
  private def loadModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    BinaryFileSerializer.deserialize(backingClassifier.model, file, gzip = _gzip)
  }
  
  private def saveLabelDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "label-domain")
    val pw = new PrintWriter(file)
    for (c <- DecisionDomain.categories.drop(1)) // drop the default category
      pw.println("%d %d %s" format (c.leftOrRightOrNo, c.shiftOrReduceOrPass, c.label))
    pw.close()
  }
  
  val DecisionLine = """(-?\d) (-?\d) (.*)""".r
  private def loadLabelDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "label-domain")
    for (l <- Source.fromFile(file).getLines())
      l match { case DecisionLine(leftRight, shiftReduce, label) => DecisionDomain.index(new ParseDecision(leftRight.toInt, shiftReduce.toInt, label)) }
    DecisionDomain.freeze()
  }
  
  private def saveFeatureDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "feat-domain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
  }
  
  private def loadFeatureDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "feat-domain")
    BinaryCubbieFileSerializer.deserialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
    NonProjParserFeaturesDomain.freeze()
  }
  
  def save(folder: File, gzip: Boolean): Unit = { 
    _gzip = gzip
    saveModel(folder)
    saveLabelDomain(folder)
    saveFeatureDomain(folder)
  }
  
  def load(folder: File, gzip: Boolean): Unit = { 
    _gzip = gzip
    loadLabelDomain(folder)
    loadFeatureDomain(folder)
    loadModel(folder)
  }
  
  def classify(v: ParseDecisionVariable): ParseDecision =
    DecisionDomain.category(backingClassifier.classify(v).bestLabelIndex)
  
}

