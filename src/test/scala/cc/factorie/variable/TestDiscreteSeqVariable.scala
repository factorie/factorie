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


class TestDiscreteSeqVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  object DiceDomain extends DiscreteDomain(6)
  object DiceSeqDomain extends DiscreteSeqDomain { def elementDomain = DiceDomain }
  // number of dice we want to pick
  class DiceSeq(num:Int) extends DiscreteSeqVariable(num) { def domain = DiceSeqDomain }

  @Test
  def testDiscreteSeqDomain(): Unit = {
  }

  @Test
  def testDiscreteSeqVariable(): Unit = {
    // lets create a three dice sequence
    val ds1 = new DiceSeq(3)
    // each dice should be initialized to 0
    assertArrayEquals(Array(0,0,0), ds1.toSeq.map(_.intValue).toArray)

    // set value for an element
    ds1.set(0, 1)(null)
    assertArrayEquals(Array(1,0,0), ds1.toSeq.map(_.intValue).toArray)
  }

}
