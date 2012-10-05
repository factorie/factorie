package cc.factorie.app.classify

import cc.factorie.app.classify._
import java.io.PrintStream
import cc.factorie.la.WeightsTensor
import cc.factorie.{CategoricalTensorDomain, CategoricalDomain}
import javax.management.remote.rmi._RMIConnection_Stub

// TODO: use the builtin load and save for CategoricalDomain
// instead of writing featurestring:value etc. just write the domain on the top line
// and then number:number on every line. do same with classifier weights too
object Serialize {

  //  def writeInstances(labels: LabelList[Label, Features], out: PrintStream): Unit = {
  //
  //  }

  def writeClassifier(classifier: Classifier[Label], featureDomain: CategoricalTensorDomain[String], out: PrintStream): Unit = classifier match {
    case cls: ModelBasedClassifier[Label] =>
      val weights = cls.model.weightsTensor
      val fileStr = new StringBuilder
      val families = weights.families.toSeq
      val labelDomain = classifier.labelDomain
      fileStr ++= (cls.getClass.getName + "\n")
      val featureNames = featureDomain.dimensionDomain.toSeq.map(_.category)
      fileStr ++= (featureNames.mkString(" ") + "\n")
      for (fIdx <- 0 to families.length - 1) {
        val family = families(fIdx)
        fileStr ++= (labelDomain(fIdx) + " ")
        weights(family).foreachActiveElement((idx, weight) => {
          val weightStr = idx + ":" + weight + " "
          fileStr ++= weightStr
        })
        fileStr ++= "\n"
      }
      out.append(fileStr)
  }

  def writeInstancesSVMLight(labels: Iterable[Label], out: PrintStream): Unit = {
    for (label <- labels) {
      val labelStr = new StringBuilder
      labelStr ++= label.features.labelName
      label.features.tensor.foreachElement((idx, f) => {
        val str = " " + label.domain.category(idx) + ":" + f.asInstanceOf[Int]
        labelStr ++= str
      })
      labelStr ++= "\n"
      out.append(labelStr)
    }
  }

  //  def writeClassifications(labels: Classification[Features], out: PrintStream): Unit = {
  //
  //  }

}

object Deserialize {

  //  def readInstances(instancesString: String): LabelList[Label, Features] = {
  //
  //  }

  def readClassifier(classifierString: String): Classifier[Label] = {
    val rawSplitStr = classifierString.split("(\r\n)|\n")
    val classifierName = rawSplitStr(0)
    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]
    val model = new LogLinearModel[Label, Features](_.features, LabelDomain, FeaturesDomain)
    val featureStrings = rawSplitStr(1).split("\\s+")
    FeaturesDomain.dimensionDomain ++= featureStrings
    var famIdx = 0
    for (familyWeights <- rawSplitStr.drop(2)) {
      val rawWeightStrs = familyWeights.split("\\s+")
      LabelDomain += rawWeightStrs(0)
      for (pairStr <- rawWeightStrs.drop(1)) {
        val Array(idxStr, weightStr) = pairStr.split(":")
        model.evidenceTemplate.weights(famIdx, idxStr.toInt) = weightStr.toDouble
      }
      famIdx += 1
    }
    new ModelBasedClassifier(model, LabelDomain)
  }

  def readInstancesSVMLight(instancesString: String): LabelList[Label, Features] = {
    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]
    val instances = new LabelList[Label, Features](_.features)
    for (rawInstStr <- instancesString.split("(\r\n)|\n")) {
      val instStr = if (rawInstStr.contains("#")) rawInstStr.substring(0, rawInstStr.indexOf('#')) else rawInstStr
      val terms = instStr.split("\\s+")
      val rawClassStr = terms(0)
      val classStr = if (rawClassStr == "+1") "1" else rawClassStr
      val features = new NonBinaryFeatures(classStr, FeaturesDomain, LabelDomain)
      for (term <- terms.drop(1); if term != "") {
        val Array(feature, count) = term.split(":")
        features +=(feature, count.toInt)
      }
      instances += features.label
    }
    instances
  }

  //  def readClassifications(labels: Classification[Features], out: PrintStream): Unit = {
  //
  //  }

}