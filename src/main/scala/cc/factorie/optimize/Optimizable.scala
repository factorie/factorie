/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.optimize
import cc.factorie._
import scala.collection.mutable.IndexedSeq

/** Functions that can be maximized by numeric optimization must provide these
    methods for getting and setting their parameters.
    @see Optimizer
    @author Andrew McCallum */
trait Optimizable {
  def numOptimizableParameters: Int
  /** If argument is null, an array will be allocated for you and returned. */
  def getOptimizableParameters(a:Array[Double] = null): Array[Double]
  def setOptimizableParameters(a:Array[Double])
  def optimizableParameter(index:Int): Double
  def optimizableParameter_=(index:Int, d:Double): Unit
}

/** An Optimizable object that can provide the value of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByValue extends Optimizable {
  def optimizableValue: Double
}

/** An Optimizable object that can provide the gradient of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByGradient extends Optimizable {
  /** If argument is null, an array will be allocated for you and returned. */
  def getOptimizableGradient(a:Array[Double] = null): Array[Double]
}

/** An Optimizable object that can provide both the value and the gradient of the function being optimized. 
    @author Andrew McCallum */
trait OptimizableByValueAndGradient extends OptimizableByValue with OptimizableByGradient

// Used for online per-instance methods, like voted perceptron.
// TODO Design still under consideration
/*trait OptimizableByGradientIterator extends Optimizable {
  def optimizableGradientIterator: Iterator[Array[Double]]
}*/
