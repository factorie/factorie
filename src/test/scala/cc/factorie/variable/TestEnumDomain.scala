package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestEnumDomain extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testEnumDomain(): Unit = {

    object DiceDomain extends EnumDomain { val ONE, TWO, THREE, FOUR, FIVE, SIX = Value }
    class DiceSample extends DiscreteVariable { def domain = DiceDomain }

    // size
    assert(DiceDomain.size == 6)

    assert(DiceDomain.ONE == 0)
    assert(DiceDomain.TWO == 1)
    assert(DiceDomain.THREE == 2)
    assert(DiceDomain.FOUR == 3)
    assert(DiceDomain.FIVE == 4)
    assert(DiceDomain.SIX == 5)

    assert(DiceDomain.category(0) == "ONE")
    assert(DiceDomain.category(1) == "TWO")
    assert(DiceDomain.category(2) == "THREE")
    assert(DiceDomain.category(3) == "FOUR")
    assert(DiceDomain.category(4) == "FIVE")
    assert(DiceDomain.category(5) == "SIX")
  }

}
