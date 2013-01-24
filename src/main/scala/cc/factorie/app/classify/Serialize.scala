package cc.factorie.app.classify

import java.io.{PrintWriter, PrintStream}
import cc.factorie._
import cc.factorie.util.Cubbie
import collection.mutable.ArrayBuffer

class ModelBasedClassifierCubbie(cls: Classifier[Label]) extends Cubbie {
//  val labelDomain = CubbieSlot[CategoricalDomainCubbie]("labelDomain", () => throw new Error)
//  labelDomain := new CategoricalDomainCubbie(cls.labelDomain.asInstanceOf[CategoricalDomain[String]])
  val model = CubbieSlot[ModelCubbie]("model", () => throw new Error)
  model := new ModelCubbie(cls.asInstanceOf[ModelBasedClassifier[Label]].model)
}

// TODO could maybe make this cleaner if we added custom serializers for different tensors that didn't require
// preexisting tensors to be passed in.. Currently this is quite slow. -luke
class LabelListCubbie(
  featuresDomain: CategoricalDimensionTensorDomain[String],
  labelDomain: CategoricalDomain[String],
  isBinary: Boolean)
  extends Cubbie {
  val labels = new StringListSlot("labels")
  labels := Seq[String]()
  val features = new CubbieListSlot[FeaturesCubbie]("features", () => new FeaturesCubbie)
  features := Seq[FeaturesCubbie]()
  def store(ll: LabelList[Label, Features]): Unit = {
    labels := ll.map(_.labelName)
    features := ll.map(l => {
      val indices = new ArrayBuffer[Int]
      val values = new ArrayBuffer[Double]
      l.features.tensor.foreachActiveElement((i, v) => { indices += i; values += v })
      val fc = new FeaturesCubbie
      fc.indices := indices
      fc.values := values
      fc
    })
  }
  def fetch(): LabelList[Label, Features] = {
    val ll = new LabelList[Label, Features](_.features)
    for ((l, f) <- labels.value.zip(features.value)) {
      val features =
        if (isBinary) new BinaryFeatures(l, "", featuresDomain, labelDomain)
        else new NonBinaryFeatures(l, "", featuresDomain, labelDomain)
      for ((i, v) <- f.indices.value.zip(f.values.value))
        features.tensor(i) = v
      ll += new Label(l, features, labelDomain)
    }
    ll
  }
}

class FeaturesCubbie extends Cubbie {
  val indices = new IntListSlot("indices")
  indices := Seq[Int]()
  val values = new DoubleListSlot("values")
  values := Seq[Double]()
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
  def readInstancesSVMLight(instancesString: String, featuresDomain: CategoricalDimensionTensorDomain[String], labelDomain: CategoricalDomain[String]): LabelList[Label, Features] = {
    val instances = new LabelList[Label, Features](_.features)
    var i = 0
    for (rawInstStr <- instancesString.split("(\r\n)|\n")) {
      i += 1
      val instStr = if (rawInstStr.contains("#")) rawInstStr.substring(0, rawInstStr.indexOf('#')) else rawInstStr
      val terms = instStr.split("\\s+")
      val rawClassStr = terms(0)
      val classStr = if (rawClassStr == "+1") "1" else rawClassStr
      val features = new NonBinaryFeatures(classStr, i.toString, featuresDomain, labelDomain)
      for (term <- terms.drop(1); if term != "") {
        val Array(feature, count) = term.split(":")
        features +=(feature, count.toDouble)
      }
      instances += features.label
    }
    instances
  }
}