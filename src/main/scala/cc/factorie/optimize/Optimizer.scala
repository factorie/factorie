/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.optimize
import cc.factorie._

/** A method of iterative, numeric function maximization, given a function provided as an Optimizable object. */
trait Optimizer {
  /** Returns true if converged. */
  def optimize(numIterations:Int = Math.MAX_INT): Boolean
  def isConverged: Boolean
  def optimizable: Optimizable
}
