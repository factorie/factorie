package cc.factorie.directed

import cc.factorie.util.FastLogging
import cc.factorie.variable.DoubleVariable
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestGaussian extends JUnitSuite with FastLogging {

  @Test
  def testGaussian(): Unit = {
    val mean = new DoubleVariable(0.0)
    val variance = new DoubleVariable(1.0)
    val value = new DoubleVariable(2.0)

    val f = Gaussian.newFactor(value, mean, variance)
    assert(f.pr == Gaussian.pr(value.doubleValue, mean.doubleValue, variance.doubleValue))
    assert(f.logpr == Gaussian.logpr(value.doubleValue, mean.doubleValue, variance.doubleValue))
    assertEquals(f.logpr, math.log(f.pr), 0.001)
  }

}
