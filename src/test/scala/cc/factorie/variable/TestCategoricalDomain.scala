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


class TestCategoricalDomain extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testAdditionsWithCounting(): Unit = {
    val domain = new CategoricalDomain[String](List("yes", "no"))
    domain.freeze()
    assert(domain.size == 2) // domain should have 2 categories
    assert(domain.count("yes") == 0) // nothing should be counted yet
    domain.gatherCounts = true
    domain ++= List("yes") // ++= should increment the count
    assert(domain.count("yes") == 1)
    domain += "yes"  // += should also increment the count
    assert(domain.count("yes") == 2)
  }

  @Test
  def testCategoricalDomain(): Unit = {

    object FootSizeDomain extends CategoricalDomain[String] {
      value("LARGE")
      value("SMALL")
      freeze()
    }
    // test domain properties
    assert(FootSizeDomain.dimensionSize == 2)
    assert(FootSizeDomain.dimensionDomain == FootSizeDomain)

    // Add new category
    // here should raise an exception, but current implementation just ignore it
    FootSizeDomain += "MEDIUM"
    assert(FootSizeDomain.value("MEDIUM") == null)
    FootSizeDomain.unfreeze()
    FootSizeDomain += "MEDIUM"
    assert(FootSizeDomain.value("MEDIUM") != null)
    assert(FootSizeDomain.dimensionSize == 3)
    FootSizeDomain.freeze()

    // read category values, should be an instance of CategoricalValue[String]
    assert(FootSizeDomain.head.isInstanceOf[CategoricalValue[String]])
    assert(FootSizeDomain.head.category == "LARGE")

    class FootSize(category:String) extends CategoricalVariable(category) {
      override def domain = FootSizeDomain
    }
    // test domain variable
    var v = new FootSize("LARGE")
    assert(v.value == FootSizeDomain.value("LARGE"))
    assert(v.domain == FootSizeDomain)

  }

}
