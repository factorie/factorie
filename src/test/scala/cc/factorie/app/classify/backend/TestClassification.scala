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
package cc.factorie.app.classify.backend

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit._


class TestLinearMulticlassClassifier extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testLinearMulticlassClassifier(): Unit = {
    val labelSize = 2
    val featureSize = 2
    val classifier = new LinearMulticlassClassifier(labelSize, featureSize)

    // assign a weight
    val weightTensor = new DenseTensor2(Array(Array(0.2, 0.4), Array(0.8, 0.6)))
    classifier.weights.set(weightTensor)

    // feed the classifier a feature
    val features = new DenseTensor1(featureSize)
    features(0) = 0.1
    features(1) = 0.9
    assertArrayEquals(weightTensor.leftMultiply(features).toArray, classifier.predict(features).toArray, 0.001)
  }

}
