package cc.factorie.directed

import cc.factorie.maths
import cc.factorie.util.FastLogging
import cc.factorie.variable.{IntegerVariable, DoubleVariable}
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestPoisson extends JUnitSuite with FastLogging {

  @Test
  def testPoisson(): Unit = {
    val mean = new DoubleVariable(1.0)
    val k = new IntegerVariable(2)

    val f = Poisson.newFactor(k, mean)
    assertEquals(math.pow(mean.doubleValue, k.intValue) * math.exp(-mean.doubleValue) / maths.factorial(k.intValue), f.pr(k.intValue, mean.doubleValue), 0.001)
  }

}
