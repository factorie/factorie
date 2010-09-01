package cc.factorie

import cc.factorie._
import junit.framework._
import Assert._

/**
 * @author kedarb
 * @since Sep 1, 2010
 */

class TestBP extends TestCase {
  type BPVar = BeliefPropagation.BPVariable

  class BinVar(i: Int) extends DiscreteVariable(i) with NoVariableCoordination
  Domain[BinVar].size = 2
  class NamedBinVar(val name: String, i: Int) extends BinVar(i)

  private def newTemplate1(score0: Double, score1: Double) =
    new InitializedTemplate(new TemplateWithVectorStatistics1[BinVar] {
      def score(s: Stat) = {
        // println(s._1.intValue + " true? " + (s._1.intValue == 0))
        if (s._1.intValue == 0) score0 else score1
      }
    })

  private def e(num: Double) = Math.exp(num)

  def testV1F1 = {
    // one variable, one factor
    val v = new BinVar(0)
    var lattice: BPLattice = null
    // 1) equal potentials
    lattice = new BPLattice(new Model(newTemplate1(1, 1)), Array(v))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 0.5, 0.001)
    // 2) unequal potentials
    lattice = new BPLattice(new Model(newTemplate1(2, 1)), Array(v))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), e(1) / (1 + e(1)), 0.001)
  }

  def testV1F2 = {
    // one variable, two factors
    val v = new BinVar(0)
    var lattice: BPLattice = null
    // 1) f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}
    lattice = new BPLattice(new Model(newTemplate1(2, 1), newTemplate1(1, 2)), Array(v))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 0.5, 0.001)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    lattice = new BPLattice(new Model(newTemplate1(0, 1), newTemplate1(0, 1)), Array(v))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 1.0 / (1 + e(2)), 0.001)
  }

  def testV2F1 = {
    // two variables, one factor
    val v1 = new NamedBinVar("a", 1)
    val v2 = new NamedBinVar("b", 1)
    var lattice: BPLattice = null
    // create template between v1 and v2
    def newTemplate2(scoreEqual: Double, scoreUnequal: Double) =
      new InitializedTemplate(new TemplateWithVectorStatistics2[NamedBinVar, NamedBinVar] {
        def unroll1(v1: NamedBinVar) = if (v1.name == "a") Factor(v1, v2) else Nil

        def unroll2(v2: NamedBinVar) = if (v2.name == "b") Factor(v1, v2) else Nil

        def score(s: Stat) = if (s._1.intValue == s._2.intValue) scoreEqual else scoreUnequal
      })
    lattice = new BPLattice(new Model(newTemplate2(1000, 0)), Array(v1, v2))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v1)(0), 0.5, 0.001)
    assertEquals(lattice.marginal(v2)(0), 0.5, 0.001)
  }

  def testTwoChain = {
    // two variables, three factors: two unary, one binary
    val v1 = new NamedBinVar("a", 1)
    val v2 = new NamedBinVar("b", 1)
    var lattice: BPLattice = null
    def newTemplate2(scoreEqual: Double, scoreUnequal: Double) =
      new InitializedTemplate(new TemplateWithVectorStatistics2[NamedBinVar, NamedBinVar] {
        def unroll1(v1: NamedBinVar) = if (v1.name == "a") Factor(v1, v2) else Nil

        def unroll2(v2: NamedBinVar) = if (v2.name == "b") Factor(v1, v2) else Nil

        def score(s: Stat) = if (s._1.intValue == s._2.intValue) scoreEqual else scoreUnequal
      })
    lattice = new BPLattice(new Model(newTemplate1(1, 0), newTemplate2(9, 0)), Array(v1, v2))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v1)(0), (1 + e(10)) / (2 + e(8) + e(10)), 0.001)
  }
}

object TestBPRunner {
  def suite: TestSuite = {
    val suite = new TestSuite
    suite.addTestSuite(classOf[TestBP])
    suite
  }

  def main(args: Array[String]) {
    junit.textui.TestRunner.run(suite)
  }
}