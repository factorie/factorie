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


class TestEnumDomain extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testEnumDomain(): Unit = {

    object DiceDomain extends EnumDomain { val ONE, TWO, THREE, FOUR, FIVE, SIX = Value }
    class DiceSample extends DiscreteVariable { def domain = DiceDomain }

    // size
    assert(DiceDomain.size == 6)

    assert(DiceDomain.ONE == 0)
    assert(DiceDomain.TWO == 1)
    assert(DiceDomain.THREE == 2)
    assert(DiceDomain.FOUR == 3)
    assert(DiceDomain.FIVE == 4)
    assert(DiceDomain.SIX == 5)

    assert(DiceDomain.category(0) == "ONE")
    assert(DiceDomain.category(1) == "TWO")
    assert(DiceDomain.category(2) == "THREE")
    assert(DiceDomain.category(3) == "FOUR")
    assert(DiceDomain.category(4) == "FIVE")
    assert(DiceDomain.category(5) == "SIX")
  }

}
