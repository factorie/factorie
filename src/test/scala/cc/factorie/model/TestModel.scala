package cc.factorie.model

import org.junit.Test
import org.scalatest.junit._


class TestModel extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testItemizedModel() {
    val m = new ItemizedModel()
    assert(m.factors.size == 0)
  }

}
