package cc.factorie.variable

import cc.factorie.util.DoubleSeq
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestProportionsVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDenseProportions1(): Unit = {
    val m1 = new DenseProportions1(4, 1)
    assertEquals(0.25, m1.pr(0), 0.001)

    val m2 = new DenseProportions1(DoubleSeq(1.0, 1,1,1))
    assertEquals(0.25, m2.pr(0), 0.001)

    val m3 = new DenseProportions1(Array(1.0, 1,1,1))
    assertEquals(0.25, m3.pr(0), 0.001)
  }

}

class TestGrowableDenseProportions1 extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testGrowableDenseProportions1(): Unit = {
    object GrowableDomain extends CategoricalDomain[String]
    val p = new GrowableDenseProportions1(GrowableDomain)
    assert(p.size == 0)

    GrowableDomain.value("hello")
    assert(p.size == 1)
    assertEquals(1.0, p(0), 0.001)

    GrowableDomain.value("world")
    assert(p.size == 2)
    assertEquals(0.5, p(0), 0.001)
  }
}

class TestGrowableUniformProportions1 extends JUnitSuite {

  @Test
  def testGrowableUniformProportions1(): Unit = {
    object GrowableDomain extends CategoricalDomain[String]
    val p = new GrowableUniformProportions1(GrowableDomain)
    assert(p.size == 0)

    GrowableDomain.value("hello")
    assert(p.size == 1)
    assertEquals(1.0, p(0), 0.001)

    GrowableDomain.value("world")
    assert(p.size == 2)
    assertEquals(0.5, p(0), 0.001)
  }
}