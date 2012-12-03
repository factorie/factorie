package cc.factorie.app.classify

import cc.factorie.app.classify._
import java.io.{PrintWriter, BufferedReader, PrintStream}
import cc.factorie._
import cc.factorie.util.Cubbie

class ModelBasedClassifierCubbie(cls: Classifier[Label]) extends Cubbie {
//  val labelDomain = CubbieSlot[CategoricalDomainCubbie]("labelDomain", () => throw new Error)
//  labelDomain := new CategoricalDomainCubbie(cls.labelDomain.asInstanceOf[CategoricalDomain[String]])
  val model = CubbieSlot[ModelCubbie]("model", () => throw new Error)
  model := new ModelCubbie  (cls.asInstanceOf[ModelBasedClassifier[Label]].model)
}

object Serialize {
  def writeClassification(toSerialize: Classification[Label], str: PrintStream) = {
    val writer = new PrintWriter(str)
    writer.println(toSerialize.label)
    writer.flush()
  }
  def writeInstancesSVMLight(labels: Iterable[Label], out: PrintStream): Unit = {
    for (label <- labels) {
      val labelStr = new StringBuilder
      labelStr ++= label.features.labelName
      label.features.tensor.foreachActiveElement((idx, f) => {
        val str = " " + label.domain.category(idx) + ":" + f.asInstanceOf[Int]
        labelStr ++= str
      })
      labelStr ++= "\n"
      out.append(labelStr)
    }
  }
  def readInstancesSVMLight(instancesString: String): LabelList[Label, Features] = {
    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]
    val instances = new LabelList[Label, Features](_.features)
    var i = 0
    for (rawInstStr <- instancesString.split("(\r\n)|\n")) {
      i += 1
      val instStr = if (rawInstStr.contains("#")) rawInstStr.substring(0, rawInstStr.indexOf('#')) else rawInstStr
      val terms = instStr.split("\\s+")
      val rawClassStr = terms(0)
      val classStr = if (rawClassStr == "+1") "1" else rawClassStr
      val features = new NonBinaryFeatures(classStr, i.toString, FeaturesDomain, LabelDomain)
      for (term <- terms.drop(1); if term != "") {
        val Array(feature, count) = term.split(":")
        features +=(feature, count.toDouble)
      }
      instances += features.label
    }
    instances
  }
}