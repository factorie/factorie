package cc.factorie.directed

import cc.factorie.util.FastLogging
import cc.factorie.variable.DoubleVariable
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestBeta extends JUnitSuite with FastLogging {

  @Test
  def testBta(): Unit = {
    val alpha = new DoubleVariable(1.0)
    val beta = new DoubleVariable(3.0)
    val value = new DoubleVariable(0.5)

    // mean = alpha / (alpha + beta)
    assertEquals(0.25, Beta.mean(alpha.doubleValue, beta.doubleValue), 0.01)

    val f = Beta.newFactor(value, alpha, beta)
    assert(f.pr(value.doubleValue, alpha.doubleValue, beta.doubleValue) == Beta.pr(value.doubleValue, alpha.doubleValue, beta.doubleValue))
  }

}
