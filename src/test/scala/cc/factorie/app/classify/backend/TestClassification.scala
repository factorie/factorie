package cc.factorie.app.classify.backend

import cc.factorie.la.{DenseTensor2, DenseTensor1}
import org.junit.Test
import org.junit.Assert._
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
