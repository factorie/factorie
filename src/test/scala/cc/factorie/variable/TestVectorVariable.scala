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

import cc.factorie.la.GrowableSparseBinaryTensor1
import org.junit.Test
import org.scalatest.junit._


class TestVectorVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  val vectorDimensionDomain = new DiscreteDomain(3)
  // a VectorDomain
  val vectorDomain = new VectorDomain {
    override type Value = BooleanValue
    override def dimensionDomain: DiscreteDomain = vectorDimensionDomain
  }

  @Test
  def testVectorDomain(): Unit = {

    assert(vectorDomain.dimensionName(1) == "1")
    assert(vectorDomain.dimensionSize == 3)

    // VectorDomain provides a proxy to freeze the underlying dimensionDomain
    assert(!vectorDomain.dimensionDomain.frozen)
    vectorDomain.freeze()
    assert(vectorDomain.dimensionDomain.frozen)
  }

  @Test
  def testVectorVariable(): Unit = {
    val v = new VectorVariable {
      override def domain: VectorDomain = vectorDomain

      // VectorVariable does not specify how to save the value
      set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))(null)
    }

    assert(!v.contains(0))
    v.update(0, 1.0)(null)
    assert(v.contains(0))
  }

}
