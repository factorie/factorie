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

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestMassesVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDenseMasses1(): Unit = {
    var m = new DenseMasses1(4, 1.0)
    assert(m.dim1 == 4)
    assertEquals(1, m(0), 0.001)
    assertEquals(0.25, m.pr(1), 0.001)

    m += (0, 1.0)
    assertEquals(5, m.massTotal, 0.001)
    assertEquals(2, m(0), 0.001)
    assertEquals(0.4, m.pr(0), 0.001)
  }

}
