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


class TestLabeledVariable extends JUnitSuite with cc.factorie.util.FastLogging {


  // LabeledCategoricalVariable
  @Test
  def testLabeledCategoricalVariable(): Unit = {
    object GenderDomain extends CategoricalDomain[String] {
      value("male")
      value("female")
      freeze()
    }
    val v = new LabeledCategoricalVariable[String]("male") {
      override def domain: CategoricalDomain[String] = GenderDomain
    }

    assert(v.target.value == GenderDomain.value("male"))

    v.set(GenderDomain.value("female"))(null)
    assert(v.value == GenderDomain.value("female"))
    assert(!v.valueIsTarget)

    v.set(GenderDomain.value("male"))(null)
    assert(v.value == GenderDomain.value("male"))
    assert(v.valueIsTarget)

  }

  // LabeledIntegerVariable
  @Test
  def testLabeledIntegerVariable(): Unit = {
    val v = new LabeledIntegerVariable(2)
    v.set(0)(null)

    assert(v.intValue == 0)
    assert(v.target.intValue == 2)
    assert(!v.valueIsTarget)

    v.set(2)(null)
    assert(v.intValue == 2)
    assert(v.valueIsTarget)
  }

  // LabeledBooleanVariable
  @Test
  def testLabeledBooleanVariable(): Unit = {
    val v = new LabeledBooleanVariable(true) {}
    assert(v.target.booleanValue)

    v.set(false)(null)
    assert(!v.booleanValue)
    assert(!v.valueIsTarget)

    v.set(true)(null)
    assert(v.booleanValue)
    assert(v.valueIsTarget)
  }

}
