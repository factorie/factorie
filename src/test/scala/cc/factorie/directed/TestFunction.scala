package cc.factorie.directed

import cc.factorie.variable.DoubleVariable
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit._


class TestFunction extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDoubleSum(): Unit = {
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)

    val a = new DoubleVariable(1.0)
    val b = new DoubleVariable(2.0)
//    val c = new DoubleSum(a, b)
//    assertEquals(3.0, c.doubleValue, 0.001)
  }

}
