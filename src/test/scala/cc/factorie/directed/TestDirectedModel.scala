package cc.factorie.directed

import cc.factorie.util.FastLogging
import cc.factorie.variable.DoubleVariable
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestDirectedModel extends JUnitSuite with FastLogging {

  @Test
  def testDirectedModel(): Unit = {
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)

    assert(model.isInstanceOf[ItemizedDirectedModel])

    val mean = new DoubleVariable(1)
    val variance = new DoubleVariable(2.0)

    val data = for (i <- 1 to 10) yield new DoubleVariable :~ Gaussian(mean, variance)

    assert(model.factors(mean).size == 10)
    assert(model.childFactors(mean).size == 10)

  }

}
