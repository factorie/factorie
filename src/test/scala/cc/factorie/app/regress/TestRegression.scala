package cc.factorie.app.regress

import cc.factorie._
import cc.factorie.la._
import org.junit.Test
import org.junit.Assert._
import cc.factorie.optimize.LinearObjectives
import cc.factorie.variable.{TensorVariable, DiffList}

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 9/15/12
 * Time: 3:00 PM
 * To change this template use File | Settings | File Templates.
 */

class MyTensorVariable(x0: Double, x1: Double, y: Double)(implicit d: DiffList = null) extends TensorVariable[Tensor1] {
  set(new DenseTensor1(1))
  value(0) = y

  val inner = new TensorVariable[Tensor1]
  inner.set(new DenseTensor1(2))
  inner(0) = x0
  inner(1) = x1

  def getFeatures = inner
}

class TestRegression {
  @Test def testSimpleRegression() {
    val y0 = new MyTensorVariable(1, 2, 4)
    val y1 = new MyTensorVariable(2, 1, 5)
    val y2 = new MyTensorVariable(1, 1, 3)

    val regressor = LinearRegressionTrainer.train[TensorVariable[Tensor1], MyTensorVariable](Seq(y0, y1, y2), f => f.getFeatures, 0.0)
    assertEquals(4, regressor.regress(y0).dependantValue(0), 0.01)
    assertEquals(5, regressor.regress(y1).dependantValue(0), 0.01)
    assertEquals(3, regressor.regress(y2).dependantValue(0), 0.01)

    val regressor2 = LinearRegressionTrainer.train[TensorVariable[Tensor1], MyTensorVariable](Seq(y0, y1, y2), f => f.getFeatures, 0.0, LinearObjectives.epsilonInsensitiveSqMultivariate(0.001))
    assertEquals(4, regressor2.regress(y0).dependantValue(0), 0.01)
    assertEquals(5, regressor2.regress(y1).dependantValue(0), 0.01)
    assertEquals(3, regressor2.regress(y2).dependantValue(0), 0.01)
  }
}
