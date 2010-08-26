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

/** Maximize an Optimizable object by Limited-memory BFGS, 
    as described in Byrd, Nocedal, and Schnabel, 
    "Representations of Quasi-Newton Matrices and Their Use in Limited Memory Methods" */
class LimitedMemoryBFGS(val optimizable: OptimizableByValue with OptimizableByGradient) extends Optimizer {
  var isConverged = false
  def optimize(numIterations:Int = Math.MAX_INT): Boolean = throw new Error("Not yet implemented")
}
