package cc.factorie.app.nlp.parse.nonproj

import ParserSupport._
import cc.factorie.app.classify.ModelBasedClassifier
import java.io.File
import java.io.PrintWriter
import cc.factorie.BinarySerializer
import scala.io.Source
import cc.factorie.BinarySerializer
import cc.factorie.CategoricalDomainCubbie
import cc.factorie.CubbieConversions
import java.io.DataInputStream
import java.io.BufferedInputStream
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import cc.factorie.ModelCubbie
import java.io.BufferedOutputStream
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream

// An abstraction which allows for easily changing the predictor
trait ParserClassifier {
  def classify(v: ParseDecisionVariable): ParseDecision
}

// The standard SVM one-vs-all classifier
// The interface here is simpler than it seems: we're only saving, loading, and classifying.
// To clean this up, we need better serialization support.
class BaseParserClassifier(val backingClassifier: ModelBasedClassifier[ParseDecisionVariable]) extends ParserClassifier {
  
  import CubbieConversions._
  
  private var _gzip = false
  private var _doubleGzip = false
  
  private def saveModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    file.createNewFile()
    if (_doubleGzip) {
      val fileStream = new BufferedOutputStream(new FileOutputStream(file))
      BinarySerializer.serialize(
        new ModelCubbie(backingClassifier.model), 
        new DataOutputStream(new BufferedOutputStream(new GZIPOutputStream(new GZIPOutputStream(fileStream))))
      )
    }
    else
      BinarySerializer.serialize(backingClassifier.model, file, gzip = _gzip)
  }
  
  private def loadModel(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "model")
    if (_doubleGzip) {
      val fileStream = new BufferedInputStream(new FileInputStream(file))
      BinarySerializer.deserialize(
        new ModelCubbie(backingClassifier.model), 
        new DataInputStream(new BufferedInputStream(new GZIPInputStream(new GZIPInputStream(fileStream))))
      )
    }
    else
      BinarySerializer.deserialize(backingClassifier.model, file, gzip = _gzip)
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
    BinarySerializer.serialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
  }
  
  private def loadFeatureDomain(folder: File): Unit = {
    val file = new File(folder.getAbsolutePath() + "/" + "feat-domain")
    BinarySerializer.deserialize(new CategoricalDomainCubbie(NonProjParserFeaturesDomain.dimensionDomain), file, gzip = _gzip)
    NonProjParserFeaturesDomain.freeze()
  }
  
  def save(folder: File, gzip: Boolean = false, doubleGzip: Boolean = false): Unit = {
    _gzip = gzip
    _doubleGzip = doubleGzip
    saveModel(folder)
    saveLabelDomain(folder)
    saveFeatureDomain(folder)
  }
  
  def load(folder: File, gzip: Boolean, doubleGzip: Boolean = false): Unit = { 
    _gzip = gzip
    _doubleGzip = doubleGzip
    loadLabelDomain(folder)
    loadFeatureDomain(folder)
    loadModel(folder)
  }
  
  def classify(v: ParseDecisionVariable): ParseDecision =
    DecisionDomain.category(backingClassifier.classify(v).bestLabelIndex)
  
}

