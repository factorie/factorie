/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.optimize
import cc.factorie._
import scala.collection.mutable.IndexedSeq
import cc.factorie.la.ArrayLA.Implicits._

/** Maximizes an Optimizable object by successive linear searches in gradient directions.
    @author Andrew McCallum */
class GradientAscent(val optimizable: OptimizableByValueAndGradient) extends Optimizer {
  var isConverged = false
  var maxStep = 1.0
  var tolerance = 0.001
  var eps = 1.0e-10
  var gradientNormMax = 100.0
  var initialStepSize = 0.2
  var step = initialStepSize

  val lineMaximizer = new BackTrackLineOptimizer(optimizable)

  def optimize(numIterations:Int = Math.MAX_INT): Boolean = {
    var value = optimizable.optimizableValue
    var gradient = optimizable.getOptimizableGradient()
    for (iteration <- 0 to numIterations) {
      // Ensure step size not to large
      val sum = gradient.twoNorm
      if (sum > gradientNormMax) gradient *= (gradientNormMax / sum)
      step = lineMaximizer.optimize(gradient, step)
      val newValue = optimizable.optimizableValue
      if (2.0*Math.abs(newValue-value) < tolerance * (Math.abs(newValue)+Math.abs(value)+eps)) {
        isConverged = true
        return true
      }
      value = newValue
      gradient = optimizable.getOptimizableGradient()
    }
    return false
  }

}
