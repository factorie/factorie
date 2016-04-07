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


class TestDiscreteVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  // dice has six possible values, from 0 to 5
  object DiceDomain extends DiscreteDomain(6)

  class Dice(v: Int) extends DiscreteVariable(v) {
    override def domain: DiscreteDomain = DiceDomain
  }

  @Test
  def testDiscreteVariable(): Unit = {
    val v = new Dice(0)

    assert(v.domain == DiceDomain)
    assert(v.value == DiceDomain(0))
    assert(v.intValue == 0)

    v.set(2)(null)
    assert(v.value == DiceDomain(2))

    v.set(DiceDomain(4))(null)
    assert(v.value == DiceDomain(4))
  }


  @Test
  def testDiscreteVariableDiff(): Unit = {
    val d = new DiffList

    val v = new Dice(0)
    assert(v.value == DiceDomain(0))
    assert(v.intValue == 0)

    v.set(2)(d)
    assert(v.value == DiceDomain(2))
    d.undo()
    assert(v.value == DiceDomain(0))
    d.redo()
    assert(v.value == DiceDomain(2))
  }

}
