/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.la

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestTensor2 extends JUnitSuite with cc.factorie.util.FastLogging {

  val eps = 1e-4

  @Test
  def testDenseTensor2(): Unit = {
    val t1 = new DenseTensor2(2,2)

    // initial value is 0
    assertEquals(0.0, t1(0,0), eps)
    assertEquals(0.0, t1(0,1), eps)
    assertEquals(0.0, t1(1,0), eps)
    assertEquals(0.0, t1(1,1), eps)

    // construct an matrix
    // | 0.2 0.4 |
    // | 0.8 0.6 |
    t1(0,0) = 0.2
    t1(0,1) = 0.4
    t1(1,0) = 0.8
    t1(1,1) = 0.6

    val t1equal = new DenseTensor2(Array(Array(0.2, 0.4), Array(0.8, 0.6)))
    assertArrayEquals(t1.toArray, t1equal.toArray, eps)

    assertEquals(0.2, t1(0,0), eps)

    val t2 = new DenseTensor2(2,2)
    // construct an matrix
    // | 0.1 0.3 |
    // | 0.9 0.7 |
    t2(0,0) = 0.1
    t2(0,1) = 0.3
    t2(1,0) = 0.9
    t2(1,1) = 0.7

    val t3 = new DenseTensor1(2)
    t3(0) = 0.1
    t3(1) = 0.9

    // | 0.2 0.4 | * | 0.1 | = | 0.38 |
    // | 0.8 0.6 |   | 0.9 |   | 0.62 |
    val t4 = t1 * t3
    assertArrayEquals(Array(0.38, 0.62), t4.toArray, eps)

    // | 0.2 0.4 | leftMultiply | 0.1 | = | 0.1 0.9 | * | 0.2  0.4 | = | 0.74 |
    // | 0.8 0.6 |              | 0.9 |                 | 0.8  0.6 |   | 0.58 |
    val t5 = t1 leftMultiply  t3
    assertArrayEquals(Array(0.74, 0.58), t5.toArray, eps)

    // println(t1 outer t3)
    // not fully implemented, which will cause infinite looping
    // t1 outer t2

  }
}
