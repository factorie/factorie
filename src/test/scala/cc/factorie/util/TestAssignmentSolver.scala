/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
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
