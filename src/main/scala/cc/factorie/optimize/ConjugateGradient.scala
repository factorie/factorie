/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.optimize

import cc.factorie._
import scala.collection.mutable.IndexedSeq
import cc.factorie.la.ArrayLA
import cc.factorie.la.ArrayLA.Implicits._

/**Maximize the Optimizable object by line search on successive conjugate gradients,
in the Polak and Ribiere version,
as described in "Numeric Recipes in C", Section 10.6. @author Andrew McCallum */
class ConjugateGradient(val optimizable: OptimizableByValueAndGradient, initialStepSize: Double = 0.01) extends Optimizer with FastLogging {
  var isConverged = false
  val lineMaximizer = new BackTrackLineOptimizer(optimizable)
  var tolerance = 0.0001
  var maxIterations = 1000
  // "eps" is a small number to recitify the special case of converging to exactly zero function value
  val eps = 1.0e-10

  // The state of a conjugate gradient search
  var fp = 0.0;
  var gg = 0.0;
  var gam = 0.0;
  var dgg = 0.0;
  var step = 0.0;
  var fret = 0.0
  var xi: Array[Double] = null;
  var g: Array[Double] = null;
  var h: Array[Double] = null
  var iterations = 0

  def reset(): Unit = xi = null

      def partialReset() {
      fp = optimizable.optimizableValue
      xi = optimizable.getOptimizableGradient()
      g = ArrayLA.copy(xi)
      h = ArrayLA.copy(xi)
      step = initialStepSize
      iterations = 0
    }


  def optimize(numIterations: Int = maxIterations): Boolean = {
    if (isConverged) return true;
    var prevStepSize = initialStepSize
    var searchingGradient = true

    if (xi == null) partialReset()

    for (iterationCount <- 0 until numIterations) {
      logger.info("ConjugateGradient: At iteration " + iterations + ", cost = " + fp);
      try {
        prevStepSize = step;
        step = lineMaximizer.optimize(xi, step);
      } catch {
        case e: IllegalArgumentException => {
          logger.error("ConjugateGradient caught " + e.toString());
          throw new Error("Time to implement testValueAndGradientCurrentParameters")
          //TestOptimizable.testValueAndGradientCurrentParameters(optimizable);
          //TestOptimizable.testValueAndGradientInDirection(optimizable, xi);
          ////System.out.println ("Trying ConjugateGradient restart.");
          ////return this.maximize (maxable, numIterations);
        }
      }

      if (step == 0.0) {
        if (searchingGradient) {
          logger.info("ConjugateGradient converged: Line maximizer got step 0 in gradient direction.  "
                  + "Gradient absNorm=" + ArrayLA.absNorm(xi))
          isConverged = true
          return true
        } else
          logger.info("Line maximizer got step 0; gradient (absNorm=" + ArrayLA.absNorm(xi) + ") probably pointing up hill.  Resetting gradient.")
        partialReset()
        step = prevStepSize
        searchingGradient = true
        // continue  Scala doesn't have "continue", hence "else" below
      } else {
        fret = optimizable.optimizableValue

        println("objective=" + fret)

        // This termination provided by "Numeric Recipes in C".
        if (2.0 * Math.abs(fret - fp) <= tolerance * (Math.abs(fret) + Math.abs(fp) + eps)) {
          logger.info("ConjugateGradient converged: old value= " + fp + " new value= " + fret + " tolerance=" + tolerance);
          isConverged = true;
          return true;
        }
        fp = fret;
        xi = optimizable.getOptimizableGradient()

        logger.info("Gradient infinityNorm = " + ArrayLA.infinityNorm(xi));
        // This termination provided by McCallum
        if (ArrayLA.infinityNorm(xi) < tolerance) {
          logger.info("ConjugateGradient converged: maximum gradient component " + ArrayLA.infinityNorm(xi) + ", less than " + tolerance)
          isConverged = true;
          return true;
        }

        dgg = 0.0;
        gg = 0.0
        var gj = 0.0;
        var xj = 0.0
        forIndex(xi.length)(j => {
          gj = g(j)
          gg += gj * gj
          xj = -xi(j)
          dgg = (xj + gj) * xj
        })
        if (gg == 0.0) {
          logger.info("ConjugateGradient converged: gradient is exactly zero.")
          isConverged = true
          return true // In unlikely case that gradient is exactly zero, then we are done
        }
        gam = dgg / gg

        var hj = 0.0
        forIndex(xi.length)(j => {
          xj = xi(j)
          g(j) = xj
          hj = h(j)
          hj = xj + gam * hj
          h(j) = hj
        })
        assert(!ArrayLA.isNaN(h))

        ArrayLA.set(xi, h)
        searchingGradient = false

        iterations += 1
        if (iterations > maxIterations) {
          logger.warn("Too many iterations in ConjugateGradient.java")
          isConverged = true
          return true
        }
      } // closing "else" that would have been handled by "continue"
    }
    false
  }

}
