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
package cc.factorie.directed

import cc.factorie.variable.DoubleVariable
import org.junit.Test
import org.scalatest.junit._


class TestFunction extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testDoubleSum(): Unit = {
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)

    val a = new DoubleVariable(1.0)
    val b = new DoubleVariable(2.0)
//    val c = new DoubleSum(a, b)
//    assertEquals(3.0, c.doubleValue, 0.001)
  }

}
