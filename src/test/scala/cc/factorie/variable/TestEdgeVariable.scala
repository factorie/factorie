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

import org.junit.Test
import org.scalatest.junit._


class TestEdgeVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testEdgeVariable() {

    val i = new IntegerVariable(0)
    val j = new IntegerVariable(0)
    val k = new IntegerVariable(0)

    // Variable whose value is a Tuple2 of Scala pointers
    val e = new EdgeVariable(i, j)
    assert(e.src === i)
    assert(e.dst === j)

    e.setSrc(k)(null)
    assert(e.src === k)

    e.setDst(i)(null)
    assert(e.dst === i)
  }

  @Test
  def testArrowVariable() {
    val i = new IntegerVariable(0)
    val j = new IntegerVariable(0)
    val k = new IntegerVariable(0)

    // Variable whose value is a Tuple2 of Scala pointers,
    // but you can only change the second of the pair
    val a = new ArrowVariable(i, j)

    assert(a.src === i)
    assert(a.dst === j)

    a.set(k)(null)
    assert(a.dst === k)
  }
}
