package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestDiscreteVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  // dice has six possible values, from 0 to 5
  object DiceDomain extends DiscreteDomain(6)

  class Dice(v: Int) extends DiscreteVariable(v) {
    override def domain: DiscreteDomain = DiceDomain
  }

  @Test
  def testDiscreteVariable(): Unit = {
    val v = new Dice(0)

    assert(v.domain == DiceDomain)
    assert(v.value == DiceDomain(0))
    assert(v.intValue == 0)

    v.set(2)(null)
    assert(v.value == DiceDomain(2))

    v.set(DiceDomain(4))(null)
    assert(v.value == DiceDomain(4))
  }


  @Test
  def testDiscreteVariableDiff(): Unit = {
    val d = new DiffList

    val v = new Dice(0)
    assert(v.value == DiceDomain(0))
    assert(v.intValue == 0)

    v.set(2)(d)
    assert(v.value == DiceDomain(2))
    d.undo()
    assert(v.value == DiceDomain(0))
    d.redo()
    assert(v.value == DiceDomain(2))
  }

}
