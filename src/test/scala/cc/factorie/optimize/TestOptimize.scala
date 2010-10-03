package cc.factorie.optimzie

import junit.framework._
import Assert._
import org.scalatest.junit.JUnitSuite
import cc.factorie.maths.ArrayOps
import cc.factorie.la.DenseVector
import util.Random
import cc.factorie.optimize._

/**
 * Created by IntelliJ IDEA.
 * User: gdruck
 * Date: Sep 30, 2010
 * Time: 2:02:12 PM
 * To change this template use File | Settings | File Templates.
 */

class TestOptimize extends TestCase {

  def testLineOptimizer = {
    var function = new SimpleFunction()
    var optimizer = new BackTrackLineOptimizer(function)
    // with a step of 1, this should jump over the
    // maximum and need to backtrack
    val gradient = new Array[Double](1)
    function.getOptimizableGradient(gradient)
    optimizer.optimize(gradient,1)
    println(function.optimizableParameter(0))
    assertEquals(function.optimizableParameter(0),2.5,1e-6)
  }

  def testLMBFGS = {

    val rand = new Random(1)

    for (i <- 0 until 10) {
      val a = rand.nextInt(10) + 1.0
      val b = rand.nextInt(10) + 1.0
      val c = rand.nextInt(10) + 1.0
      val d = rand.nextInt(10) + 1.0

      var function = new BivariateQuadraticFunction(a,b,c,d)
      // this value is far enough away from the max that
      // optimizing will require at least two iterations
      var optimizer = new LimitedMemoryBFGS(function)
      optimizer.tolerance = 1e-8
      optimizer.gradientTolerance = 1e-8
      optimizer.optimize()

      //println(function.optimizableParameter(0) + " " + c/(2*a))
      //println(function.optimizableParameter(1) + " " + d/(2*b))

      assertEquals(function.optimizableParameter(0),c/(2*a),1e-4)
      assertEquals(function.optimizableParameter(1),d/(2*b),1e-4)
    }
  }

  def testGradientAscent = {

    val rand = new Random(1)

    for (i <- 0 until 10) {
      val a = rand.nextInt(10) + 1.0
      val b = rand.nextInt(10) + 1.0
      val c = rand.nextInt(10) + 1.0
      val d = rand.nextInt(10) + 1.0

      var function = new BivariateQuadraticFunction(a,b,c,d)
      // this value is far enough away from the max that
      // optimizing will require at least two iterations
      var optimizer = new GradientAscent(function)

      // reduce all tolerances to make sure
      // we do not stop until actually at the
      // maximum
      optimizer.tolerance = 1e-12
      optimizer.gradientTolerance = 1e-8
      optimizer.lineOptimizer.absTolx = 1e-8
      optimizer.lineOptimizer.relTolx = 1e-8
      optimizer.optimize()

      //println(function.optimizableParameter(0) + " " + c/(2*a))
      //println(function.optimizableParameter(1) + " " + d/(2*b))

      assertEquals(function.optimizableParameter(0),c/(2*a),1e-4)
      assertEquals(function.optimizableParameter(1),d/(2*b),1e-4)
    }
  }

    def testConjugateGradient = {

    val rand = new Random(1)

    for (i <- 0 until 10) {
      val a = rand.nextInt(10) + 1.0
      val b = rand.nextInt(10) + 1.0
      val c = rand.nextInt(10) + 1.0
      val d = rand.nextInt(10) + 1.0

      var function = new BivariateQuadraticFunction(a,b,c,d)
      // this value is far enough away from the max that
      // optimizing will require at least two iterations
      var optimizer = new ConjugateGradient(function)
      optimizer.tolerance = 1e-12
      optimizer.gradientTolerance = 1e-8
      optimizer.lineOptimizer.absTolx = 1e-8
      optimizer.lineOptimizer.relTolx = 1e-8
      try {
        optimizer.optimize()
      }
      catch {
        case e:Exception => e.printStackTrace()
      }

      //println(function.optimizableParameter(0) + " " + c/(2*a))
      //println(function.optimizableParameter(1) + " " + d/(2*b))

      assertEquals(function.optimizableParameter(0),c/(2*a),1e-4)
      assertEquals(function.optimizableParameter(1),d/(2*b),1e-4)
    }
  }
}

class BivariateQuadraticFunction(var a : Double, var b : Double, var c : Double, var d : Double)
        extends OptimizableByValueAndGradient {
  var x = new Array[Double](2)

  def numOptimizableParameters : Int = 2

  def getOptimizableParameters(a:Array[Double]) = {
    assertTrue(a.length == 2)
    Array.copy(x, 0, a, 0, x.length)
  }

  def setOptimizableParameters(a:Array[Double]) = {
    assertTrue(a.length == 2)
    Array.copy(a, 0, x, 0, a.length)
  }

  def optimizableParameter(index:Int): Double = {
    assertTrue(index < 2)
    x(index)
  }

  def optimizableParameter_=(index:Int, d:Double): Unit ={
    assertTrue(index < 2);
    x(index) = d
  }

  def optimizableValue : Double = {
    - a * x(0) * x(0) - b * x(1) * x(1) + c * x(0) + d * x(1)
  }
  
  def getOptimizableGradient(gradient:Array[Double]) = {
    assertTrue(gradient.length == 2)
    gradient(0) = -2 * a * x(0) + c
    gradient(1) = - 2 * b * x(1) + d
  }

}

class SimpleFunction extends OptimizableByValueAndGradient {
  var x : Double = 0.0

  def numOptimizableParameters : Int = 1

  def getOptimizableParameters(a:Array[Double]) = {
    assertTrue(a.length == 1)
    a(0) = x
    a
  }

  def setOptimizableParameters(a:Array[Double]) = {
    assertTrue(a.length == 1)
    x = a(0)
  }

  def optimizableParameter(index:Int): Double = {
    assertTrue(index == 0)
    x
  }

  def optimizableParameter_=(index:Int, d:Double): Unit ={
    assertTrue(index == 0);
    x = d
  }

  def optimizableValue : Double = {
    -x*x + 5 * x
  }

  def getOptimizableGradient(a:Array[Double]) = {
    a(0) = -2 * x + 5
    a
  }
}

object TestOptimizeRunner {
  def suite: TestSuite = {
    val suite = new TestSuite
    suite.addTestSuite(classOf[TestOptimize])
    suite
  }

  def main(args: Array[String]) {
    junit.textui.TestRunner.run(suite)
  }
}