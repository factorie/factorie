package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestFeatureVectorVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testBinaryFeatureVectorVariable(): Unit = {

    object DocumentDomain extends CategoricalVectorDomain[String]
    assert(DocumentDomain.dimensionSize == 0)

    class Document extends BinaryFeatureVectorVariable[String] {
      override def domain: CategoricalVectorDomain[String] = DocumentDomain
    }

    val document = new Document
    document += "hello"
    document += "world"
    document ++= Seq("a", "b", "c")
    assert(DocumentDomain.dimensionSize == 5)

    println(DocumentDomain.stringToCategory("hello"))
    println(DocumentDomain.stringToCategory("xyz"))
    assert(DocumentDomain.dimensionSize == 5)

    assert(document.activeCategories.contains("hello"))
  }

  @Test
  def testFeatureVectorVariable(): Unit = {
    object featureDomain extends CategoricalVectorDomain[String]
    val v = new FeatureVectorVariable[String]() {
      override def domain: CategoricalVectorDomain[String] = featureDomain
    }

    v += "hello"
    v += "world"

    println(v)
  }
}
