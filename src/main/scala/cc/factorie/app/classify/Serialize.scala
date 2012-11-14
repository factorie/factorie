package cc.factorie.app.classify

import cc.factorie.app.classify._
import java.io.{PrintWriter, BufferedReader, PrintStream}
import cc.factorie.la.WeightsTensor
import cc.factorie._
import javax.management.remote.rmi._RMIConnection_Stub
import java.util.zip.GZIPOutputStream
import cc.factorie.util.Cubbie

// TODO: use the builtin load and save for CategoricalDomain
// instead of writing featurestring:value etc. just write the domain on the top line
// and then number:number on every line. do same with classifier weights too

// TODO: this doesn't work because in order for the Tensor to know how long it's supposed to be,
// we need to have read the categories out first! -luke
class ClassifierCubbie(cls: Classifier[Label]) extends Cubbie {
  val labelDomain = CubbieSlot[CategoricalDomainCubbie]("labelDomain", () => throw new Error)
  val model = CubbieSlot[ModelCubbie]("model", () => throw new Error)
  labelDomain := new CategoricalDomainCubbie(cls.labelDomain.asInstanceOf[CategoricalDomain[String]])
  model := new ModelCubbie(cls.asInstanceOf[ModelBasedClassifier[Label]].model)
  def fetch(cls: Classifier[Label]) = {
    labelDomain.value.fetch(cls.labelDomain.asInstanceOf[CategoricalDomain[String]])
    cls
  }
}

object Serialize {

  implicit object ClassificationSerializer extends Serializer[Classification[Label]] {
    def serialize(toSerialize: Classification[Label], str: PrintStream) = {
      val writer = new PrintWriter(str)
      writer.println(toSerialize.label)
      writer.flush()
    }
    def deserialize(deserializeTo: Classification[Label], reader: BufferedReader) = sys.error("Can't deserialize classifications.")
  }

  implicit object ClassifierSerializer extends Serializer[Classifier[Label]] {
    def serialize(toSerialize: Classifier[Label], str: PrintStream) = toSerialize match {
      case cls: ModelBasedClassifier[Label] =>
        Serializer.serialize(cls.labelDomain.asInstanceOf[CategoricalDomain[String]], str)
        Serializer.serialize(cls.model, str)
    }
    def deserialize(deserializeTo: Classifier[Label], str: BufferedReader) = deserializeTo match {
      case cls: ModelBasedClassifier[Label] =>
        Serializer.deserialize(cls.labelDomain.asInstanceOf[CategoricalDomain[String]], str)
        Serializer.deserialize(cls.model, str)
    }
  }

  def writeInstancesSVMLight(labels: Iterable[Label], out: PrintStream): Unit = {
    for (label <- labels) {
      val labelStr = new StringBuilder
      labelStr ++= label.features.labelName
      labelStr ++= (" " + label.features.instanceName)
      label.features.tensor.foreachElement((idx, f) => {
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
    for (rawInstStr <- instancesString.split("(\r\n)|\n")) {
      val instStr = if (rawInstStr.contains("#")) rawInstStr.substring(0, rawInstStr.indexOf('#')) else rawInstStr
      val terms = instStr.split("\\s+")
      val rawClassStr = terms(0)
      val instanceStr = terms(1)
      val classStr = if (rawClassStr == "+1") "1" else rawClassStr
      val features = new NonBinaryFeatures(classStr, instanceStr, FeaturesDomain, LabelDomain)
      for (term <- terms.drop(2); if term != "") {
        val Array(feature, count) = term.split(":")
        features +=(feature, count.toInt)
      }
      instances += features.label
    }
    instances
  }
}