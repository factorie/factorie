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
package cc.factorie.variable

import cc.factorie.util.DoubleSeq
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestProportionsVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDenseProportions1(): Unit = {
    val m1 = new DenseProportions1(4, 1)
    assertEquals(0.25, m1.pr(0), 0.001)

    val m2 = new DenseProportions1(DoubleSeq(1.0, 1,1,1))
    assertEquals(0.25, m2.pr(0), 0.001)

    val m3 = new DenseProportions1(Array(1.0, 1,1,1))
    assertEquals(0.25, m3.pr(0), 0.001)
  }

}

class TestGrowableDenseProportions1 extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testGrowableDenseProportions1(): Unit = {
    object GrowableDomain extends CategoricalDomain[String]
    val p = new GrowableDenseProportions1(GrowableDomain)
    assert(p.size == 0)

    GrowableDomain.value("hello")
    assert(p.size == 1)
    assertEquals(1.0, p(0), 0.001)

    GrowableDomain.value("world")
    assert(p.size == 2)
    assertEquals(0.5, p(0), 0.001)
  }
}

class TestGrowableUniformProportions1 extends JUnitSuite {

  @Test
  def testGrowableUniformProportions1(): Unit = {
    object GrowableDomain extends CategoricalDomain[String]
    val p = new GrowableUniformProportions1(GrowableDomain)
    assert(p.size == 0)

    GrowableDomain.value("hello")
    assert(p.size == 1)
    assertEquals(1.0, p(0), 0.001)

    GrowableDomain.value("world")
    assert(p.size == 2)
    assertEquals(0.5, p(0), 0.001)
  }
}