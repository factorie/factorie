package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestEdgeVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testEdgeVariable() {

    val i = new IntegerVariable(0)
    val j = new IntegerVariable(0)
    val k = new IntegerVariable(0)

    // Variable whose value is a Tuple2 of Scala pointers
    val e = new EdgeVariable(i, j)
    assert(e.src === i)
    assert(e.dst === j)

    e.setSrc(k)(null)
    assert(e.src === k)

    e.setDst(i)(null)
    assert(e.dst === i)
  }

  @Test
  def testArrowVariable() {
    val i = new IntegerVariable(0)
    val j = new IntegerVariable(0)
    val k = new IntegerVariable(0)

    // Variable whose value is a Tuple2 of Scala pointers,
    // but you can only change the second of the pair
    val a = new ArrowVariable(i, j)

    assert(a.src === i)
    assert(a.dst === j)

    a.set(k)(null)
    assert(a.dst === k)
  }
}
