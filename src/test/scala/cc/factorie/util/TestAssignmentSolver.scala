package cc.factorie.util
import org.junit._
import Assert._
import cc.factorie.la.DenseTensor2

/**
 * User: apassos
 * Date: 6/3/13
 * Time: 12:00 PM
 */
class TestAssignmentSolver {
  @Test def testSingleSource() {
    val weights = new DenseTensor2(1, 10)
    weights(0, 5) = 5
    val result = new AssignmentSolver(weights).solve()
    assertEquals(1, result.length)
    assertEquals((0,5), result.head)
  }

  @Test def testSingleTarget() {
    val weights = new DenseTensor2(10, 1)
    weights(5, 0) = 5
    val result = new AssignmentSolver(weights).solve()
    assertEquals(1, result.length)
    assertEquals((5,0), result.head)
  }

  @Test def testAscendingChain() {
    val weights = new DenseTensor2(5, 5)
    for (i <- 0 until 5) weights(i, i) = 2
    for (i <- 0 until 4) weights(i,i+1) = 3
    val result = new AssignmentSolver(weights).solve()
    assertEquals(4, result.length)
    val parents = Array.fill(5)(-1)
    for ((s,t) <- result) parents(s) = t
    for (i <- 0 until 4) assertEquals(i+1, parents(i))
  }


  @Test def testAscendingChainLose() {
    val weights = new DenseTensor2(5, 5)
    for (i <- 0 until 5) weights(i, i) = 4
    for (i <- 0 until 4) weights(i,i+1) = 3
    val result = new AssignmentSolver(weights).solve()
    assertEquals(5, result.length)
    val parents = Array.fill(5)(-1)
    for ((s,t) <- result) parents(s) = t
    for (i <- 0 until 5) assertEquals(i, parents(i))
  }

  @Test def testHeaviestMatch() {
    val weights = new DenseTensor2(3, 3)
    weights(0, 0) = 0
    weights(0, 1) = 3
    weights(0, 2) = 0
    weights(1, 0) = 2
    weights(1, 1) = 10
    weights(1, 2) = 3
    weights(2, 0) = 0
    weights(2, 1) = 5
    weights(2, 2) = 0
    val result = new AssignmentSolver(weights).solve()
    val parents = Array.fill(3)(-1)
    for ((s,t) <- result) parents(s) = t
    assertEquals(1, parents(1))
  }
}
