package cc.factorie.directed

import cc.factorie.util.FastLogging
import cc.factorie.variable._
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestPlatedDiscrete extends JUnitSuite with FastLogging {

  // Support we have a bunch of coins, we flip each coin and check the results
  object CoinDomain extends DiscreteDomain(2)
  object CoinSeqDomain extends DiscreteSeqDomain { def elementDomain = CoinDomain }
  class CoinSeq(num:Int) extends DiscreteSeqVariable(num) { def domain = CoinSeqDomain }

  @Test
  def testPlatedDiscrete(): Unit = {
    // 0 is tail, 1 is head
    // all coins have p(tail) = 0.6, p(head) = 0.4
    val p = new ProportionsVariable(new DenseProportions1(Array(0.6, 0.4)))

    // construct the directed model and flip coins from the given distribution
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)
    val cs = new CoinSeq(1000) :~ PlatedDiscrete(p)

    // check the generated sequence
    val numTails = cs.intValues.filter(_ == 0).length
    assertEquals(0.6, numTails.toDouble/1000, 0.01)
  }

}
