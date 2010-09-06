package cc.factorie

import cc.factorie._
import junit.framework._
import Assert._
import collection.mutable.HashMap

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

  private def newTemplate2(nm1: String, nm2: String, nm2var: HashMap[String, NamedBinVar],
                           scoreEqual: Double, scoreUnequal: Double) =
    new InitializedTemplate(new TemplateWithVectorStatistics2[NamedBinVar, NamedBinVar] {
      def unroll1(v1: NamedBinVar) = if (v1.name == nm1) Factor(v1, nm2var(nm2)) else Nil

      def unroll2(v2: NamedBinVar) = if (v2.name == nm2) Factor(nm2var(nm1), v2) else Nil

      def score(s: Stat) = if (s._1.intValue == s._2.intValue) scoreEqual else scoreUnequal
    })

  private def e(num: Double) = Math.exp(num)

  def testV1F1 = {
    // one variable, one factor
    val v = new BinVar(0)
    var lattice: BPLattice = null
    // 1) equal potentials
    lattice = new BPLattice(Array(v), new Model(newTemplate1(1, 1)))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 0.5, 0.001)
    // 2) unequal potentials
    lattice = new BPLattice(Array(v), new Model(newTemplate1(2, 1)))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), e(1) / (1 + e(1)), 0.001)
  }

  def testV1F2 = {
    // one variable, two factors
    val v = new BinVar(0)
    var lattice: BPLattice = null
    // 1) f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}
    lattice = new BPLattice(Array(v), new Model(newTemplate1(2, 1), newTemplate1(1, 2)))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 0.5, 0.001)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    lattice = new BPLattice(Array(v), new Model(newTemplate1(0, 1), newTemplate1(0, 1)))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v)(0), 1.0 / (1 + e(2)), 0.001)
  }

  def testV2F1 = {
    // two variables, one factor
    val nm2var = new HashMap[String, NamedBinVar]

    val v1 = new NamedBinVar("a", 1)
    nm2var += "a" -> v1

    val v2 = new NamedBinVar("b", 1);
    nm2var += "b" -> v2

    var lattice: BPLattice = null
    // create template between v1 and v2
    lattice = new BPLattice(Array(v1, v2), new Model(newTemplate2("a", "b", nm2var, 1000, 0)))
    lattice.updateTreewise(false)
    assertEquals(lattice.marginal(v1)(0), 0.5, 0.001)
    assertEquals(lattice.marginal(v2)(0), 0.5, 0.001)
  }

  def testTwoChain = {
    // two variables, three factors: two unary, one binary
    val nm2var = new HashMap[String, NamedBinVar]

    val v1 = new NamedBinVar("a", 1)
    nm2var += "a" -> v1

    val v2 = new NamedBinVar("b", 1)
    nm2var += "b" -> v2

    var lattice: BPLattice = null
    val model = new Model(newTemplate1(1, 0), newTemplate2("a", "b", nm2var, 9, 0))
    lattice = new BPLattice(Array(v1, v2), model)
    lattice.updateTreewise(false)
    // print factor marginal
    model.factors(v2).foreach {
      f =>
        if (f.variables.size == 1) {
          val marginals = lattice.marginalMap(f)
          assertEquals(marginals(List(0)), (1 + e(10)) / (2 + e(8) + e(10)), 0.001)
          assertEquals(marginals(List(1)), (1 + e(8)) / (2 + e(8) + e(10)), 0.001)
        } else {
          val marginals = lattice.marginalMap(f)
          assertEquals(marginals(List(0, 0)), e(10) / (2 + e(8) + e(10)), 0.001)
          assertEquals(marginals(List(0, 1)), 1.0 / (2 + e(8) + e(10)), 0.001)
          assertEquals(marginals(List(1, 0)), 1.0 / (2 + e(8) + e(10)), 0.001)
          assertEquals(marginals(List(1, 1)), e(8) / (2 + e(8) + e(10)), 0.001)
        }
    }
    assertEquals(lattice.marginal(v1)(0), (1 + e(10)) / (2 + e(8) + e(10)), 0.001)
    assertEquals(lattice.sumLogZ, 11.127, 0.001)
  }

  def testThreeChain = {
    // three variables, five factors: three unary, two binary
    val nm2var = new HashMap[String, NamedBinVar]

    val v1 = new NamedBinVar("a", 0)
    nm2var += "a" -> v1

    val v2 = new NamedBinVar("b", 1)
    nm2var += "b" -> v2

    val v3 = new NamedBinVar("c", 0)
    nm2var += "c" -> v3

    // create model
    val model = new Model(newTemplate1(0, 1), newTemplate2("a", "b", nm2var, 2, -1), newTemplate2("b", "c", nm2var, 1, 3))
    val lattice = new BPLattice(Array(v1, v2, v3), model)
    lattice.updateTreewise(false)
    assertEquals(lattice.sumLogZ, Math.log(e(1) + e(2) + 2 * e(3) + e(4) + 2 * e(6) + e(7)), 0.001)
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
