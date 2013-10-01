package cc.factorie.app.classify

import java.io.{PrintWriter, PrintStream}
import cc.factorie._
import cc.factorie.util.Cubbie
import collection.mutable.ArrayBuffer
import cc.factorie.variable.{CategoricalVectorDomain, CategoricalDomain}


// TODO could maybe make this cleaner if we added custom serializers for different tensors that didn't require
// preexisting tensors to be passed in.. Currently this is quite slow. -luke
class LabelListCubbie(
  featuresDomain: CategoricalVectorDomain[String],
  labelDomain: CategoricalDomain[String],
  isBinary: Boolean)
  extends Cubbie {
  val labels = new StringListSlot("labels")
  labels := Seq[String]()
  val features = new CubbieListSlot[FeaturesCubbie]("features", () => new FeaturesCubbie)
  features := Seq[FeaturesCubbie]()
  def store(ll: ArrayBuffer[Label]): Unit = {
    labels := ll.map(_.labelName)
    features := ll.map(l => {
      val indices = new ArrayBuffer[Int]
      val values = new ArrayBuffer[Double]
      l.features.value.foreachActiveElement((i, v) => { indices += i; values += v })
      val fc = new FeaturesCubbie
      fc.indices := indices
      fc.values := values
      fc
    })
  }
  def fetch(): ArrayBuffer[Label] = {
    val ll = new ArrayBuffer[Label]()
    for ((l, f) <- labels.value.zip(features.value)) {
      val features =
        if (isBinary) new BinaryFeatures(l, "", featuresDomain, labelDomain)
        else new NonBinaryFeatures(l, "", featuresDomain, labelDomain)
      for ((i, v) <- f.indices.value.zip(f.values.value))
        features.value(i) = v
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
  def writeInstancesSVMLight(labels: Iterable[Label], out: PrintStream): Unit = {
    for (label <- labels) {
      val labelStr = new StringBuilder
      labelStr ++= label.features.labelName
      label.features.value.foreachActiveElement((idx, f) => {
        val str = " " + label.domain.category(idx) + ":" + f.asInstanceOf[Int]
        labelStr ++= str
      })
      labelStr ++= "\n"
      out.append(labelStr)
    }
  }
  def readInstancesSVMLight(instancesString: String, featuresDomain: CategoricalVectorDomain[String], labelDomain: CategoricalDomain[String]): ArrayBuffer[Label] = {
    val instances = new ArrayBuffer[Label]()
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