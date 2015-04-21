package cc.factorie.directed

import cc.factorie.variable._
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit._


class TestMaximizeProportions extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testInfer(): Unit = {
    implicit val model = DirectedModel()

    object DiceDomain extends EnumDomain { val ONE, TWO, THREE, FOUR, FIVE, SIX = Value }
    class DiceSample(value:Int) extends DiscreteVariable(value) { def domain = DiceDomain }

    // this is our dice
    val p = new ProportionsVariable(new DenseProportions1(DiceDomain.size))

    // suppose we have an even dice
    // now generate some samples for this proportion
    new DiceSample(DiceDomain.ONE) ~ Discrete(p)
    new DiceSample(DiceDomain.TWO) ~ Discrete(p)
    new DiceSample(DiceDomain.THREE) ~ Discrete(p)
    new DiceSample(DiceDomain.FOUR) ~ Discrete(p)
    new DiceSample(DiceDomain.FIVE) ~ Discrete(p)
    new DiceSample(DiceDomain.SIX) ~ Discrete(p)

    // we can estimate the proportion from the sample data
    val s = MaximizeProportions.infer(Seq(p), model)

    val prop: Double = 1.0/6
    assertArrayEquals(Array(prop, prop, prop, prop, prop, prop), s.marginal(p)._1.value.asInstanceOf[DenseProportions1].toArray, 0.1)
  }
}
