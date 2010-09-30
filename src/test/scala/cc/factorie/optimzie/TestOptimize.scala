package cc.factorie.optimzie

import junit.framework._
import Assert._
import org.scalatest.junit.JUnitSuite
import cc.factorie.optimize.{GradientAscent, BackTrackLineOptimizer, OptimizableByValueAndGradient}

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

  def testGradientAscent = {
    var function = new SimpleFunction()
    var optimizer = new GradientAscent(function)
    optimizer.optimize()
    println(function.optimizableParameter(0))
    assertEquals(function.optimizableParameter(0),2.5,1e-6)
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