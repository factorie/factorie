package cc.factorie.variable

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestMassesVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDenseMasses1(): Unit = {
    var m = new DenseMasses1(4, 1.0)
    assert(m.dim1 == 4)
    assertEquals(1, m(0), 0.001)
    assertEquals(0.25, m.pr(1), 0.001)

    m += (0, 1.0)
    assertEquals(5, m.massTotal, 0.001)
    assertEquals(2, m(0), 0.001)
    assertEquals(0.4, m.pr(0), 0.001)
  }

}
