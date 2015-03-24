package cc.factorie.variable

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit._


class TestDiscreteSeqVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  object DiceDomain extends DiscreteDomain(6)
  object DiceSeqDomain extends DiscreteSeqDomain { def elementDomain = DiceDomain }
  // number of dice we want to pick
  class DiceSeq(num:Int) extends DiscreteSeqVariable(num) { def domain = DiceSeqDomain }

  @Test
  def testDiscreteSeqDomain(): Unit = {
  }

  @Test
  def testDiscreteSeqVariable(): Unit = {
    // lets create a three dice sequence
    val ds1 = new DiceSeq(3)
    // each dice should be initialized to 0
    assertArrayEquals(Array(0,0,0), ds1.toSeq.map(_.intValue).toArray)

    // set value for an element
    ds1.set(0, 1)(null)
    assertArrayEquals(Array(1,0,0), ds1.toSeq.map(_.intValue).toArray)
  }

}
