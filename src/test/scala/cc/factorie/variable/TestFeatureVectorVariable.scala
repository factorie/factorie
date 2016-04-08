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


class TestFeatureVectorVariable extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testBinaryFeatureVectorVariable(): Unit = {

    object DocumentDomain extends CategoricalVectorDomain[String]
    assert(DocumentDomain.dimensionSize == 0)

    class Document extends BinaryFeatureVectorVariable[String] {
      override def domain: CategoricalVectorDomain[String] = DocumentDomain
    }

    val document = new Document
    document += "hello"
    document += "world"
    document ++= Seq("a", "b", "c")
    assert(DocumentDomain.dimensionSize == 5)

    println(DocumentDomain.stringToCategory("hello"))
    println(DocumentDomain.stringToCategory("xyz"))
    assert(DocumentDomain.dimensionSize == 5)

    assert(document.activeCategories.contains("hello"))
  }

  @Test
  def testFeatureVectorVariable(): Unit = {
    object featureDomain extends CategoricalVectorDomain[String]
    val v = new FeatureVectorVariable[String]() {
      override def domain: CategoricalVectorDomain[String] = featureDomain
    }

    v += "hello"
    v += "world"

    println(v)
  }
}
