package cc.factorie.directed

import cc.factorie.infer.Maximize
import cc.factorie.util.FastLogging
import cc.factorie.variable._
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestDirichlet extends JUnitSuite with FastLogging {

  @Test
  def testDirichlet(): Unit = {
    implicit val random = new scala.util.Random(0)
    object WordDomain extends EnumDomain { val a, b, c, d, e, f = Value }
    class Word extends DiscreteVariable { def domain = WordDomain }
    implicit val model = DirectedModel()

    val masses = new MassesVariable(new DenseMasses1(WordDomain.size, 2.0))
    assertArrayEquals(Array(2.0,2.0,2.0,2.0,2.0,2.0), masses.value.toArray, 0.01)

    // generate
    val p1 = new ProportionsVariable(new DenseProportions1(WordDomain.size))
    p1 :~ Dirichlet(masses)

    val data = for (i <- 0 until 500) yield new Word :~ Discrete(p1)

    assert(model.parentFactor(p1).touches(masses))
    assert(model.childFactors(p1).size == 500)

    val s1 = MaximizeProportions.infer(Seq(p1), model)
    val s2 = Maximize(Seq(p1), model)

//    val ps = for (i <- 0 until 1000) yield ProportionsVariable.dense(WordDomain.size) :~ Dirichlet(masses)
//    MaximizeDirichletByMomentMatching(masses, model)
  }

}
