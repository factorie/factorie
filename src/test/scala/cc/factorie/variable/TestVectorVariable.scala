package cc.factorie.variable

import cc.factorie.la.{GrowableSparseBinaryTensor1}
import org.junit.Test
import org.scalatest.junit._


class TestVectorVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  val vectorDimensionDomain = new DiscreteDomain(3)
  // a VectorDomain
  val vectorDomain = new VectorDomain {
    override type Value = BooleanValue
    override def dimensionDomain: DiscreteDomain = vectorDimensionDomain
  }

  @Test
  def testVectorDomain(): Unit = {

    assert(vectorDomain.dimensionName(1) == "1")
    assert(vectorDomain.dimensionSize == 3)

    // VectorDomain provides a proxy to freeze the underlying dimensionDomain
    assert(!vectorDomain.dimensionDomain.frozen)
    vectorDomain.freeze()
    assert(vectorDomain.dimensionDomain.frozen)
  }

  @Test
  def testVectorVariable(): Unit = {
    val v = new VectorVariable {
      override def domain: VectorDomain = vectorDomain

      // VectorVariable does not specify how to save the value
      set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))(null)
    }

    assert(!v.contains(0))
    v.update(0, 1.0)(null)
    assert(v.contains(0))
  }

}
