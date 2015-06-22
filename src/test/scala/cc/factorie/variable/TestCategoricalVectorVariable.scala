package cc.factorie.variable

import cc.factorie.la.GrowableSparseBinaryTensor1
import org.junit.Test
import org.scalatest.junit._


class TestCategoricalVectorVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testCategoricalVectorVariable(): Unit = {
    object DocumentDomain extends CategoricalVectorDomain[String]
    class Document extends CategoricalVectorVariable[String] {

      // the value is not set in CategoricalVectorVariable
      set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))(null)

      override def domain: CategoricalVectorDomain[String] = DocumentDomain
    }

    val document = new Document
    document += "hello"
    document += "world"
    document ++= Seq("a", "b", "c")

    println(document.activeCategories.contains("hello"))
  }

}
