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


class TestDiff extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testCreateDiff() {
    //this test just shows how variables create diff objects that point to them
    val b = new BooleanVariable(true)
    val diff = new DiffList
    b.set(false)(diff)
    assert(diff(0).variable === b)
  }

  @Test
  def testDiffUndoAndRedo() {
    val i = new IntegerVariable(2)

    val d = new DiffList
    i.set(3)(d) // This method will create a Diff object and append it to the DiffList d.
    assert(3 == i.value)

    d.undo()
    assert(2 == i.value)

    d.redo()
    assert(3 == i.value)
  }
}
